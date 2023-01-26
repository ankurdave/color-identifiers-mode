;;; color-identifiers-mode.el --- Color identifiers based on their names -*- lexical-binding: t -*-

;; Copyright (C) 2014 Ankur Dave

;; Author: Ankur Dave <ankurdave@gmail.com>
;; Url: https://github.com/ankurdave/color-identifiers-mode
;; Created: 24 Jan 2014
;; Version: 1.1
;; Keywords: faces, languages
;; Package-Requires: ((dash "2.5.0") (emacs "24"))

;; This file is not a part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Color Identifiers is a minor mode for Emacs that highlights each source code
;; identifier uniquely based on its name.  It is inspired by a post by Evan
;; Brooks: https://medium.com/p/3a6db2743a1e/

;; Check out the project page, which has screenshots, a demo, and usage
;; instructions: https://github.com/ankurdave/color-identifiers-mode

;;; Code:

(require 'advice)
(require 'color)
(require 'dash)
(require 'cl-lib)
(require 'rx)

(defgroup color-identifiers nil "Color identifiers based on their names."
  :group 'faces)

(defvar color-identifiers:timer nil
  "Timer for running `color-identifiers:refresh'.")

(defvar color-identifiers:recoloring-delay 2
  "The delay before running `color-identifiers:refresh'.")

(defun color-identifiers:enable-timer ()
  (if color-identifiers:timer
      ;; Someone set the timer. Activate in case we cancelled it.
      (unless (memq color-identifiers:timer timer-idle-list)
        (timer-activate-when-idle color-identifiers:timer))
    (setq color-identifiers:timer
          (run-with-idle-timer color-identifiers:recoloring-delay t 'color-identifiers:refresh)))
  )

(defvar-local color-identifiers:colorize-behavior nil
  "For internal use. Stores the element of
`color-identifiers:modes-alist' that is relevant to the current
major mode")

;;;###autoload
(define-minor-mode color-identifiers-mode
  "Color the identifiers in the current buffer based on their names."
  :init-value nil
  :lighter " ColorIds"
  (if color-identifiers-mode
      (progn
        (setq color-identifiers:colorize-behavior
              (assoc major-mode color-identifiers:modes-alist))
        (if (not color-identifiers:colorize-behavior)
            (progn
              (print "Major mode is not supported by color-identifiers, disabling")
              (color-identifiers-mode -1))
          (color-identifiers:regenerate-colors)
          (color-identifiers:refresh)
          (add-to-list 'font-lock-extra-managed-props 'color-identifiers:fontified)
          (font-lock-add-keywords nil '((color-identifiers:colorize . default)) t)
          (color-identifiers:enable-timer)
          (ad-activate 'enable-theme)))
    (when color-identifiers:timer
      (cancel-timer color-identifiers:timer))
    (font-lock-remove-keywords nil '((color-identifiers:colorize . default)))
    (ad-deactivate 'enable-theme))
  (color-identifiers:refontify))

;;;###autoload
(define-global-minor-mode global-color-identifiers-mode
  color-identifiers-mode color-identifiers-mode-maybe)

(defadvice enable-theme (after color-identifiers:regen-on-theme-change)
  "Regenerate colors for color-identifiers-mode on theme change."
  (color-identifiers:regenerate-colors))

;;; USER-VISIBLE VARIABLES AND FUNCTIONS =======================================

(defcustom color-identifiers-coloring-method 'sequential
  "How to assign colors: sequentially or using the hash of the identifier.
Sequential color assignment (the default) reduces collisions
between adjacent identifiers. Hash-based color assignment ensures
that a particular identifier is always assigned the same color
across buffers."
  :type '(choice
          (const :tag "Sequential" sequential)
          (const :tag "Hash-based" hash)))


(defcustom color-identifiers-avoid-faces nil
  "Which color faces to avoid: A list of faces whose foreground
color should be avoided when generating colors, this can be warning colors,
error colors etc."
  :type '(repeat face))

(defvar color-identifiers:modes-alist nil
  "Alist of major modes and the ways to distinguish identifiers in those modes.
The value of each cons cell provides four constraints for finding
identifiers.  A word must match all four constraints to be
colored as an identifier.  The cons cell has the form (MAJOR-MODE
IDENTIFIER-CONTEXT-RE IDENTIFIER-RE IDENTIFIER-FACES
IDENTIFIER-EXCLUSION-RE).

MAJOR-MODE is the name of the mode in which this rule should be used.
IDENTIFIER-CONTEXT-RE is a regexp matching the text that must precede an
identifier.
IDENTIFIER-RE is a regexp whose first capture group matches identifiers.
IDENTIFIER-FACES is a list of faces with which the major mode decorates
identifiers or a function returning such a list.  If the list includes nil,
unfontified words will be considered.
IDENTIFIER-EXCLUSION-RE is a regexp that must not match identifiers,
or nil.

If a scan function is registered for a mode, candidate
identifiers will be further restricted to those returned by the
scan function.")

(defvar color-identifiers:num-colors 10
  "The number of different colors to generate.")

(defvar color-identifiers:color-luminance nil
  "HSL luminance of identifier colors. If nil, calculated from the luminance
of the default face.")

(defvar color-identifiers:min-color-saturation 0.0
  "The minimum saturation that identifier colors will be generated with.")

(defvar color-identifiers:max-color-saturation 1.0
  "The maximum saturation that identifier colors will be generated with.")

(defvar color-identifiers:mode-to-scan-fn-alist nil
  "Alist from major modes to their declaration scan functions, for internal use.
If no scan function is registered for a particular mode, all
candidates matching the constraints in
`color-identifiers:modes-alist' will be colored.

Modify this variable using
`color-identifiers:set-declaration-scan-fn'.")

(defvar color-identifiers-mode-hook nil
  "List of functions to run every time the mode enabled")

(defvar color-identifiers:re-not-inside-class-access
  (rx (or (not (any ".")) line-start)
      (zero-or-more space))
  "This regexp matches anything except inside a class instance
  access, like foo.bar" )

(defun color-identifiers:set-declaration-scan-fn (mode scan-fn)
  "Register SCAN-FN as the declaration scanner for MODE.
SCAN-FN must scan the entire current buffer and return the
identifiers to highlight as a list of strings. Only identifiers
produced by SCAN-FN that also match all constraints in
`color-identifiers:modes-alist' will be colored.

See `color-identifiers:elisp-get-declarations' for an example
SCAN-FN."
  (let ((entry (assoc mode color-identifiers:mode-to-scan-fn-alist)))
    (if entry
        (setcdr entry scan-fn)
      (add-to-list 'color-identifiers:mode-to-scan-fn-alist
                   (cons mode scan-fn)))))

(defsubst color-identifiers:curr-identifier-faces ()
  (if (functionp (nth 3 color-identifiers:colorize-behavior))
      (funcall (nth 3 color-identifiers:colorize-behavior))
    (nth 3 color-identifiers:colorize-behavior)))

;;; MAJOR MODE SUPPORT =========================================================

(defun color-identifiers:get-declarations ()
  "Extract a list of identifiers declared in the current buffer."
  (let ((result (make-hash-table :test 'equal))
        (identifier-faces (color-identifiers:curr-identifier-faces)))
    ;; Entities that major mode highlighted as variables
    (save-excursion
      (let ((next-change (next-property-change (point-min))))
        (while next-change
          (goto-char next-change)
          (let ((face-at-point (get-text-property (point) 'face)))
            (when (or (and face-at-point (memq face-at-point identifier-faces))
                      ;; If we fontified X in the past, keep X in the list for
                      ;; consistency. Otherwise `scan-identifiers' will stop
                      ;; colorizing new Xes while older ones remain colorized.
                      (get-text-property (point) 'color-identifiers:fontified))
              (puthash (substring-no-properties (symbol-name (symbol-at-point))) t result)))
          (setq next-change (next-property-change (point))))))
    (hash-table-keys result)))

(dolist (maj-mode '(c-mode c++-mode java-mode rust-mode rustic-mode meson-mode typescript-mode cuda-mode tsx-ts-mode typescript-ts-mode))
  (add-to-list
   'color-identifiers:modes-alist
   `(,maj-mode . (""
                  "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                  (nil font-lock-variable-name-face tree-sitter-hl-face:variable)))))

;; Scala
(add-to-list
 'color-identifiers:modes-alist
 `(scala-mode . (,color-identifiers:re-not-inside-class-access
                 "\\_<\\([[:lower:]]\\([_]??[[:lower:][:upper:]\\$0-9]+\\)*\\(_+[#:<=>@!%&*+/?\\\\^|~-]+\\|_\\)?\\)"
                 (nil scala-font-lock:var-face font-lock-variable-name-face tree-sitter-hl-face:variable))))

;;; JavaScript
(add-to-list
 'color-identifiers:modes-alist
 `(js-mode . (,color-identifiers:re-not-inside-class-access
              "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
              (nil font-lock-variable-name-face))))

(add-to-list
 'color-identifiers:modes-alist
 `(js2-mode . (,color-identifiers:re-not-inside-class-access
               "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
               (nil font-lock-variable-name-face js2-function-param))))

(add-to-list
 'color-identifiers:modes-alist
 `(js3-mode . (,color-identifiers:re-not-inside-class-access
               "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
               (nil font-lock-variable-name-face js3-function-param-face))))

(add-to-list
 'color-identifiers:modes-alist
 `(js-jsx-mode . (,color-identifiers:re-not-inside-class-access
                  "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                  (nil font-lock-variable-name-face js2-function-param))))

(add-to-list
 'color-identifiers:modes-alist
 `(js2-jsx-mode . (,color-identifiers:re-not-inside-class-access
                   "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                   (nil font-lock-variable-name-face js2-function-param))))

;; CoffeeScript
;; May need to add the @ to the symbol syntax
;; (add-hook 'coffee-mode-hook (lambda () (modify-syntax-entry ?\@ "_"))) in .emacs
(add-to-list
 'color-identifiers:modes-alist
 `(coffee-mode . (,color-identifiers:re-not-inside-class-access "\\_<\\([a-zA-Z_$@]\\(?:\\s_\\|\\sw\\)*\\)" (nil font-lock-variable-name-face))))

;; Sgml mode and the like
(dolist (maj-mode '(sgml-mode html-mode jinja2-mode))
  (add-to-list
   'color-identifiers:modes-alist
   `(,maj-mode . ("</?!?"
                  "\\_</?!?\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                  (nil font-lock-function-name-face)))))

;; Ruby
(add-to-list
 'color-identifiers:modes-alist
 `(ruby-mode . (,color-identifiers:re-not-inside-class-access
                "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                (nil tree-sitter-hl-face:variable))))

;; R
(add-to-list
 'color-identifiers:modes-alist
 `(R-mode . (,color-identifiers:re-not-inside-class-access "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)" (nil))))

;; SQL
(add-to-list
 'color-identifiers:modes-alist
 `(sql-mode . (,color-identifiers:re-not-inside-class-access "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)" (nil))))

;; Groovy
(add-to-list
 'color-identifiers:modes-alist
 `(groovy-mode . (,color-identifiers:re-not-inside-class-access
                  "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                  (nil font-lock-variable-name-face))))

;; Objective-C
(add-to-list
 'color-identifiers:modes-alist
 `(objc-mode . (nil
                "\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                (nil font-lock-variable-name-face)
                "[a-zA-Z_$]\\(\\s_\\|\\sw\\)*\\s-*[(:]")))

;; Golang
(add-to-list
 'color-identifiers:modes-alist
 `(go-mode . (,color-identifiers:re-not-inside-class-access
              "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
              (nil font-lock-variable-name-face tree-sitter-hl-face:variable))))

;; Python
(cl-eval-when 'compile (require 'python))
(defun color-identifiers:python-get-declarations ()
  "Extract a list of identifiers declared in the current buffer.
For Python support within color-identifiers-mode.  Supports
function arguments and variable assignment, but not yet lambda
arguments, loops (for .. in), or for comprehensions."
  (let ((result (make-hash-table :test 'equal))
        (identifier-faces (color-identifiers:curr-identifier-faces)))
    ;; Function arguments
    (save-excursion
      (goto-char (point-min))
      (while (python-nav-forward-defun)
        (condition-case nil
            (let ((arglist (sexp-at-point)))
              (when (and arglist (listp arglist))
                (let* ((first-arg (car arglist))
                       (rest (cdr arglist))
                       (rest-args
                        (-map (lambda (token) (cadr token))
                              (-filter (lambda (token) (and (listp token) (eq (car token) '\,))) rest)))
                       (args-filtered (cons first-arg rest-args))
                       (params (-map (lambda (token)
                                       (car (split-string (symbol-name token) "[=:]")))
                                     args-filtered)))
                  (dolist (param params) (puthash param t result)))))
          (wrong-type-argument nil))))
    ;; Entities that python-mode highlighted as variables
    (save-excursion
      (let ((next-change (next-property-change (point-min))))
        (while next-change
          (goto-char next-change)
          (let ((face-at-point (get-text-property (point) 'face)))
            (when (or (and face-at-point (memq face-at-point identifier-faces))
                      ;; If we fontified it in the past, assume it should
                      ;; continue to be fontified. This avoids alternating
                      ;; between fontified and unfontified.
                      (get-text-property (point) 'color-identifiers:fontified))
              (puthash (substring-no-properties (symbol-name (symbol-at-point))) t result)))
          (setq next-change (next-property-change (point))))))
    (hash-table-keys result)))
(color-identifiers:set-declaration-scan-fn
 'python-mode 'color-identifiers:python-get-declarations)

(add-to-list
 'color-identifiers:modes-alist
 `(python-mode . (,color-identifiers:re-not-inside-class-access
                  "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                  (nil font-lock-variable-name-face tree-sitter-hl-face:variable))))

;; Emacs Lisp
(defun color-identifiers:elisp-declarations-in-sexp (sexp)
  "Extract a list of identifiers declared in SEXP.
For Emacs Lisp support within color-identifiers-mode."
  (pcase sexp
    ((or `(let . ,rest) `(let* . ,rest))
     ;; VARLIST of let/let* could be like ((a 1) b c (d "foo")).
     (append (when (listp (car rest))
               (mapcar (lambda (var) (if (symbolp var) var (car var))) (car rest)))
             (color-identifiers:elisp-declarations-in-sexp rest)))
    ((or `(defun ,_ ,args . ,rest) `(lambda ,args . ,rest))
     (append (when (listp args) args)
             (color-identifiers:elisp-declarations-in-sexp rest)))
    (`nil nil)
    ((pred consp)
     (let ((cons sexp)
           (result nil))
       (while (consp cons)
         (let ((ids (color-identifiers:elisp-declarations-in-sexp (car cons))))
           (when ids
             (setq result (append ids result))))
         (setq cons (cdr cons)))
       (when cons
         ;; `cons' is non-nil but also non-cons
         (let ((ids (color-identifiers:elisp-declarations-in-sexp cons)))
           (when ids
             (setq result (append ids result)))))
       result))))

(defun color-identifiers:elisp-get-declarations ()
  "Extract a list of identifiers declared in the current buffer.
For Emacs Lisp support within color-identifiers-mode."
  (let ((result nil))
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
          (while t
            (condition-case nil
                (let* ((sexp (read (current-buffer)))
                       (ids (color-identifiers:elisp-declarations-in-sexp sexp))
                       (strs (-filter 'identity
                                      (mapcar (lambda (id)
                                                (when (symbolp id) (symbol-name id)))
                                              ids))))
                  (setq result (append strs result)))
              (invalid-read-syntax nil)))
        (end-of-file nil)))
    (delete-dups result)
    result))

(color-identifiers:set-declaration-scan-fn
 'emacs-lisp-mode 'color-identifiers:elisp-get-declarations)

(add-to-list
 'color-identifiers:modes-alist
 `(emacs-lisp-mode . (""
                      "\\_<\\(\\(?:\\s_\\|\\sw\\)+\\)"
                      (nil))))

;; Clojure
(defun color-identifiers:clojure-extract-params (binding-forms)
  "Extracts bound identifiers from a sequence of binding-forms by flattening it.
If BINDING-FORMS is actually a binding-form+exprs, extracts the
binding-form first. For Clojure support within color-identifiers-mode.

See http://clojure.org/special_forms#binding-forms for the syntax
of binding-forms.

TODO: Fails (returns incorrect identifiers) on map binding-forms."
  (cond
   ((and (listp binding-forms)
         (vectorp (car binding-forms)))
    (color-identifiers:clojure-extract-params (car binding-forms)))
   ((sequencep binding-forms)
    (apply 'append (mapcar 'color-identifiers:clojure-extract-params binding-forms)))
   (t (list binding-forms))))

(defun color-identifiers:clojure-contains-binding-forms-p (sexp)
  "Returns t if SEXP could be a binding-form or a binding-form+exprs."
  (or (vectorp sexp)
      (and (listp sexp)
           (vectorp (car sexp)))))

(defun color-identifiers:clojure-declarations-in-sexp (sexp)
  "Extract a list of identifiers declared in SEXP.
For Clojure support within color-identifiers-mode. "
  (pcase sexp
    ;; (let [bindings*] exprs*)
    ;; binding => binding-form init-expr
    ((or `(let . ,rest)
         `(loop . ,rest))
     (append (when (sequencep (car rest))
               (let* ((bindings (append (car rest) nil))
                      (even-indices
                       (-filter 'cl-evenp (number-sequence 0 (1- (length bindings)))))
                      (binding-forms (-select-by-indices even-indices bindings)))
                 (color-identifiers:clojure-extract-params binding-forms)))
             (color-identifiers:clojure-declarations-in-sexp rest)))
    ;; (fn name? [binding-form*] exprs*)
    ;; (fn name? ([binding-form*] exprs*)+)
    (`(fn . ,rest)
     (let* ((binding-forms+exprs (if (symbolp (car rest)) (cdr rest) rest))
            (binding-forms (if (vectorp (car binding-forms+exprs))
                               (elt binding-forms+exprs 0)
                             (mapcar 'car binding-forms+exprs)))
            (params (color-identifiers:clojure-extract-params binding-forms)))
       (append params (color-identifiers:clojure-declarations-in-sexp rest))))
    ;; (defn name doc-string? attr-map? [binding-form*] body)
    ;; (defn name doc-string? attr-map? ([binding-form*] body)+)
    ((or `(defn ,_ . ,rest)
         `(defn- ,_ . ,rest)
         `(defmacro ,_ . ,rest))
     (let ((params (-mapcat (lambda (params+body)
                              (when (color-identifiers:clojure-contains-binding-forms-p params+body)
                                (color-identifiers:clojure-extract-params params+body)))
                            rest)))
       (append params (color-identifiers:clojure-declarations-in-sexp rest))))
    (`nil nil)
    ((pred consp)
     (let ((cons sexp)
           (result nil))
       (while (consp cons)
         (let ((ids (color-identifiers:clojure-declarations-in-sexp (car cons))))
           (when ids
             (setq result (append ids result))))
         (setq cons (cdr cons)))
       (when cons
         ;; `cons' is non-nil but also non-cons
         (let ((ids (color-identifiers:clojure-declarations-in-sexp cons)))
           (when ids
             (setq result (append ids result)))))
       result))
    ((pred arrayp)
     (apply 'append (mapcar 'color-identifiers:clojure-declarations-in-sexp sexp)))))

(defun color-identifiers:clojure-get-declarations ()
  "Extract a list of identifiers declared in the current buffer.
For Clojure support within color-identifiers-mode.

TODO: Fails on top-level sexps containing Clojure syntax that is
incompatible with Emacs Lisp syntax, such as reader macros (#)."
  (let ((result nil))
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
          (while t
            (condition-case nil
                (let* ((sexp (read (current-buffer)))
                       (ids (color-identifiers:clojure-declarations-in-sexp sexp))
                       (strs (-filter (lambda (id) (if (member id '("&" ":as")) nil id))
                                      (mapcar (lambda (id)
                                                (when (symbolp id) (symbol-name id)))
                                              ids))))
                  (setq result (append strs result)))
              (invalid-read-syntax nil)
              (wrong-type-argument nil)))
        (end-of-file nil)))
    (delete-dups result)
    result))

(color-identifiers:set-declaration-scan-fn
 'clojure-mode 'color-identifiers:clojure-get-declarations)

(add-to-list
 'color-identifiers:modes-alist
 `(clojure-mode . (""
                   "\\_<\\(\\(?:\\s_\\|\\sw\\)+\\)"
                   (nil))))

(add-to-list
 'color-identifiers:modes-alist
 `(clojurescript-mode . (""
                         "\\_<\\(\\(?:\\s_\\|\\sw\\)+\\)"
                         (nil))))

(dolist (maj-mode '(tuareg-mode sml-mode))
  (add-to-list
   'color-identifiers:modes-alist
   `(,maj-mode . (""
                  "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\|'\\)*\\)"
                  (nil font-lock-variable-name-face)))))

;; R support in ess-mode
(defun color-identifiers:remove-string-or-comment (str)
  "Remove string or comment in str, based on font lock faces"
  (let ((remove (memq (get-text-property 0 'face str)
                      '(font-lock-string-face font-lock-comment-face)))
        (pos 0)
        (nextpos)
        (result ""))
    (while (setq nextpos (next-single-property-change pos 'face str))
      (unless remove
        (setq result (concat result (substring-no-properties str pos nextpos))))
      (setq pos nextpos)
      (setq remove (memq (get-text-property pos 'face str)
                         '(font-lock-string-face font-lock-comment-face))))
    (unless remove
      (setq result (concat result (substring-no-properties str pos nextpos))))
    result))

(defun color-identifiers:r-get-args (lend)
  "Extract a list of function arg names. LEND is the point at
the left parenthesis, after `function' keyword."
  (let* ((rend (save-excursion
                 (goto-char lend)
                 (forward-sexp)
                 (point)))
         (str (color-identifiers:remove-string-or-comment
               (buffer-substring (1+ lend) (1- rend)))))
    (mapcar (lambda (s) (replace-regexp-in-string "\\s *=.*" "" s))
            (split-string str "," t " "))))

(defun color-identifiers:r-get-declarations ()
  "Extract a list of identifiers declared in the current buffer.
For Emacs Lisp support within color-identifiers-mode."
  (let ((result nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(\\(?:\\w\\|\\s_\\)*\\)\\s *<<?-\\s *\\(function\\s *\\)?" nil t)
        (unless (memq (get-text-property (match-end 0) 'face)
                      '(font-lock-string-face font-lock-comment-face))

          (if (match-string 2)
              (setq result (append (color-identifiers:r-get-args (match-end 2))
                                   result))
            (let ((var-name (match-string-no-properties 1)))
              (unless (string= var-name "")
                (cl-pushnew var-name result)))))))
    (delete-dups result)
    result))

(color-identifiers:set-declaration-scan-fn
 'ess-mode 'color-identifiers:r-get-declarations)

(add-to-list
 'color-identifiers:modes-alist
 `(ess-mode "[^$][[:space:]]*" "\\_<\\(\\(?:\\s_\\|\\sw\\)+\\)"
            (nil)))


;;; PACKAGE INTERNALS ==========================================================

(defvar color-identifiers:colors nil
  "List of generated hex colors for internal use.")

(defun color-identifiers:get-declaration-scan-fn (mode)
  "See `color-identifiers:set-declaration-scan-fn'."
  (let ((entry (assoc mode color-identifiers:mode-to-scan-fn-alist)))
    (if entry
        (cdr entry)
      nil)))

(defun color-identifiers:regenerate-colors ()
  "Generate perceptually distinct colors with the same luminance in HSL space.
Colors are output to `color-identifiers:colors'."
  (interactive)
  (let* ((luminance (or color-identifiers:color-luminance
                        (max 0.35 (min 0.8 (color-identifiers:attribute-luminance :foreground)))))
         (min-saturation (float color-identifiers:min-color-saturation))
         (saturation-range (- (float color-identifiers:max-color-saturation) min-saturation))
         (bgcolor (color-identifiers:attribute-lab :background))
         (avoidlist (mapcar 'color-identifiers:foreground-lab color-identifiers-avoid-faces))
         (candidates '())
         (chosens '())
         (n 8)
         (n-1 (float (1- n))))
    ;; Populate candidates with evenly spaced HSL colors with fixed luminance,
    ;; converted to LAB
    (dotimes (h n)
      (dotimes (s n)
        (cl-pushnew
         (apply 'color-srgb-to-lab
                (color-hsl-to-rgb (/ h n-1)
                                  (+ min-saturation (* (/ s n-1) saturation-range))
                                  luminance))
         candidates)))
    (let ((choose-candidate (lambda (candidate)
                              (delq candidate candidates)
                              (push candidate chosens))))
      (while (and candidates (< (length chosens) color-identifiers:num-colors))
        (let* (;; For each remaining candidate, find the distance to the closest chosen
               ;; color
               (min-dists (-map (lambda (candidate)
                                  (cons candidate
                                        (-min (-map (lambda (chosen)
                                                      (color-cie-de2000 candidate chosen))
                                                    (cons bgcolor (append chosens avoidlist))))))
                                candidates))
               ;; Take the candidate with the highest min distance
               (best (-max-by (lambda (x y) (> (cdr x) (cdr y))) min-dists)))
          (funcall choose-candidate (car best))))
      (setq color-identifiers:colors
            (-map (lambda (lab)
                    (let* ((srgb (apply 'color-lab-to-srgb lab))
                           (rgb (mapcar 'color-clamp srgb)))
                      (apply 'color-rgb-to-hex rgb)))
                  chosens)))))

(defvar-local color-identifiers:color-index-for-identifier (make-hash-table :test 'equal)
  "Hashtable of identifier-index pairs for internal use.
The index refers to `color-identifiers:colors'. Only used when
`color-identifiers-coloring-method' is `sequential'.")

(defvar-local color-identifiers:identifiers nil
  "Set of identifiers in the current buffer.
Only used when `color-identifiers-coloring-method' is `hash' and
a declaration scan function is registered for the current major
mode. This variable memoizes the result of the declaration scan function.")

(defun color-identifiers:attribute-luminance (attribute)
  "Find the HSL luminance of the specified ATTRIBUTE on the default face."
  (let ((rgb (color-name-to-rgb (face-attribute 'default attribute))))
    (if rgb
        (nth 2 (apply 'color-rgb-to-hsl rgb))
      0.5)))

(defun color-identifiers:attribute-lab (attribute)
  "Find the LAB color value of the specified ATTRIBUTE on the default face."
  (let ((rgb (color-name-to-rgb (face-attribute 'default attribute))))
    (if rgb
        (apply 'color-srgb-to-lab rgb)
      '(0.0 0.0 0.0))))

(defun color-identifiers:foreground-lab (face)
  "Find the LAB color value of the foreground attribute on the
specified face."
  (let ((rgb (color-name-to-rgb (face-attribute face :foreground))))
    (if rgb
        (apply 'color-srgb-to-lab rgb)
      '(0.0 0.0 0.0))))

(defun color-identifiers:refresh ()
  "Refresh the set of identifiers in the current buffer.
If `color-identifiers-coloring-method' is `sequential',
identifiers and their corresponding color indexes are saved to
`color-identifiers:color-index-for-identifier'.

If `color-identifiers-coloring-method' is `hash' and a
declaration scan function is registered for the current buffer's
major mode, identifiers are saved to
`color-identifiers:identifiers'."
  (interactive)
  (when color-identifiers-mode
    (cond
     ((eq color-identifiers-coloring-method 'sequential)
      (let ((i 0)
            ;; to make sure subsequently added vars aren't colorized the same add a (point)
            (randomize-subseq-calls (point)))
        (dolist (identifier (color-identifiers:list-identifiers))
          (unless (gethash identifier color-identifiers:color-index-for-identifier)
            (puthash identifier
                     (% (+ randomize-subseq-calls i) color-identifiers:num-colors)
                     color-identifiers:color-index-for-identifier)
            (setq i (1+ i))))))
     ((and (eq color-identifiers-coloring-method 'hash)
           (color-identifiers:get-declaration-scan-fn major-mode))
      (setq color-identifiers:identifiers
            (color-identifiers:list-identifiers))))
    (color-identifiers:refontify)))

(defun color-identifiers:list-identifiers ()
  "Return all identifiers in the current buffer."
  (if (color-identifiers:get-declaration-scan-fn major-mode)
      (funcall (color-identifiers:get-declaration-scan-fn major-mode))
    ;; When no scan function is registered, fall back to
    ;; `color-identifiers:get-declarations', which returns all identifiers
    (color-identifiers:get-declarations)))

(defun color-identifiers:refontify ()
  "Refontify the buffer using font-lock."
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

(defun color-identifiers:color-identifier (identifier)
  "Return the hex color for IDENTIFIER, or nil if it should not
be colored."
  (cond
   ((eq color-identifiers-coloring-method 'sequential)
    (let ((index (gethash identifier color-identifiers:color-index-for-identifier)))
      (when index
        (nth index color-identifiers:colors))))
   ((eq color-identifiers-coloring-method 'hash)
    ;; If there is a declaration scan function for this major mode, the
    ;; candidate identifier should only be colored if it is in the memoized list
    ;; of identifiers. Otherwise, it should be colored unconditionally.
    (when (or (not (color-identifiers:get-declaration-scan-fn major-mode))
              (member identifier color-identifiers:identifiers))
      (color-identifiers:hash-identifier identifier)))))

(defun color-identifiers:hash-identifier (identifier)
  "Return a color for IDENTIFIER based on its hash."
  (nth (% (abs (sxhash identifier)) color-identifiers:num-colors)
       color-identifiers:colors))

(defun color-identifiers:scan-identifiers (fn limit &optional continue-p)
  "Run FN on all candidate identifiers from point up to LIMIT.
Candidate identifiers are defined by
`color-identifiers:modes-alist'. Declaration scan functions are
not applied. If supplied, iteration only continues if CONTINUE-P
evaluates to true."
  (let ((identifier-context-re (nth 1 color-identifiers:colorize-behavior))
        (identifier-re (nth 2 color-identifiers:colorize-behavior))
        (identifier-faces (color-identifiers:curr-identifier-faces))
        (identifier-exclusion-re (nth 4 color-identifiers:colorize-behavior)))
    ;; Skip forward to the next identifier that matches all four conditions
    (condition-case nil
        (while (and (< (point) limit)
                    (if continue-p (funcall continue-p) t))
          (if (or (memq (get-text-property (point) 'face) identifier-faces)
                  (let ((flface-prop (get-text-property (point) 'font-lock-face)))
                    (and flface-prop (memq flface-prop identifier-faces))))
              (if (and (looking-back identifier-context-re (line-beginning-position))
                       (or (not identifier-exclusion-re) (not (looking-at identifier-exclusion-re)))
                       (looking-at identifier-re))
                  (progn
                    ;; Found an identifier. Run `fn' on it
                    (funcall fn (match-beginning 1) (match-end 1))
                    (goto-char (match-end 1)))
                (forward-char)
                (re-search-forward identifier-re limit)
                (goto-char (match-beginning 0)))
            (goto-char (next-property-change (point) nil limit))))
      (search-failed nil))))

(defun color-identifiers:colorize (limit)
  (color-identifiers:scan-identifiers
   (lambda (start end)
     (let* ((identifier (buffer-substring-no-properties start end))
            (hex (color-identifiers:color-identifier identifier)))
       (when hex
         (put-text-property start end 'face `(:foreground ,hex))
         (put-text-property start end 'color-identifiers:fontified t))))
   limit))

(defun color-identifiers-mode-maybe ()
  "Enable `color-identifiers-mode' in the current buffer if desired.
When `major-mode' is listed in `color-identifiers:modes-alist', then
`color-identifiers-mode' will be enabled."
  (when (assoc major-mode color-identifiers:modes-alist)
    (color-identifiers-mode 1)))

(provide 'color-identifiers-mode)

;;; color-identifiers-mode.el ends here
