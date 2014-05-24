;;; color-identifiers-mode.el --- Color identifiers based on their names

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
(require 'python)

(defvar color-identifiers:timer)

;;;###autoload
(define-minor-mode color-identifiers-mode
  "Color the identifiers in the current buffer based on their names."
  :init-value nil
  :lighter " ColorIds"
  (if color-identifiers-mode
      (progn
        (color-identifiers:regenerate-colors)
        (color-identifiers:refresh)
        (add-to-list 'font-lock-extra-managed-props 'color-identifiers:fontified)
        (font-lock-add-keywords nil '((color-identifiers:colorize . default)) t)
        (unless color-identifiers:timer
          (setq color-identifiers:timer
                (run-with-idle-timer 5 t 'color-identifiers:refresh)))
        (ad-activate 'enable-theme))
    (when color-identifiers:timer
      (cancel-timer color-identifiers:timer))
    (setq color-identifiers:timer nil)
    (font-lock-remove-keywords nil '((color-identifiers:colorize . default)))
    (ad-deactivate 'enable-theme))
  (font-lock-fontify-buffer))

;;;###autoload
(define-global-minor-mode global-color-identifiers-mode
  color-identifiers-mode color-identifiers-mode-maybe)

(defadvice enable-theme (after color-identifiers:regen-on-theme-change)
  "Regenerate colors for color-identifiers-mode on theme change."
  (color-identifiers:regenerate-colors))

;;; USER-VISIBLE VARIABLES AND FUNCTIONS =======================================

(defvar color-identifiers:modes-alist nil
  "Alist of major modes and the ways to distinguish identifiers in those modes.
The value of each cons cell provides four constraints for finding identifiers.
A word must match all four constraints to be colored as an identifier.  The
value has the form (IDENTIFIER-CONTEXT-RE IDENTIFIER-RE IDENTIFIER-FACES
IDENTIFIER-EXCLUSION-RE).

IDENTIFIER-CONTEXT-RE is a regexp matching the text that must precede an
identifier.
IDENTIFIER-RE is a regexp whose first capture group matches identifiers.
IDENTIFIER-FACES is a list of faces with which the major mode decorates
identifiers or a function returning such a list.  If the list includes nil,
unfontified words will be considered.
IDENTIFIER-EXCLUSION-RE is a regexp that must not match identifiers,
or nil.")

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
Modify this using `color-identifiers:set-declaration-scan-fn'.")

(defun color-identifiers:set-declaration-scan-fn (mode scan-fn)
  "Register SCAN-FN as the declaration scanner for MODE.
SCAN-FN must scan the entire current buffer and return the
identifiers to highlight as a list of strings. See
`color-identifiers:elisp-get-declarations' for an example."
  (let ((entry (assoc mode color-identifiers:mode-to-scan-fn-alist)))
    (if entry
        (setcdr entry scan-fn)
      (add-to-list 'color-identifiers:mode-to-scan-fn-alist
                   (cons mode scan-fn)))))

;;; MAJOR MODE SUPPORT =========================================================

;; Scala
(add-to-list
 'color-identifiers:modes-alist
 `(scala-mode . ("[^.][[:space:]]*"
                 "\\_<\\([[:lower:]]\\([_]??[[:lower:][:upper:]\\$0-9]+\\)*\\(_+[#:<=>@!%&*+/?\\\\^|~-]+\\|_\\)?\\)"
                 (nil scala-font-lock:var-face font-lock-variable-name-face))))

;; C/C++
(defun color-identifiers:cc-mode-get-declarations ()
  "Extract a list of identifiers declared in the current buffer.
For cc-mode support within color-identifiers-mode."
  (let ((result nil))
    ;; Variables that cc-mode highlighted with font-lock-variable-name-face
    (save-excursion
      (goto-char (point-min))
      (catch 'end-of-file
        (while t
          (let ((next-change (next-property-change (point))))
            (if (not next-change)
                (throw 'end-of-file nil)
              (goto-char next-change)
              (when (or (eq (get-text-property (point) 'face) 'font-lock-variable-name-face)
                        ;; If we fontified it in the past, assume it should
                        ;; continue to be fontified. This avoids alternating
                        ;; between fontified and unfontified.
                        (get-text-property (point) 'color-identifiers:fontified))
                (push (substring-no-properties (symbol-name (symbol-at-point))) result)))))))
    (delete-dups result)
    result))

(dolist (maj-mode '(c-mode c++-mode java-mode))
  (color-identifiers:set-declaration-scan-fn
   maj-mode 'color-identifiers:cc-mode-get-declarations)
  (add-to-list
   'color-identifiers:modes-alist
   `(,maj-mode . (""
                  "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                  (nil font-lock-variable-name-face)))))

;;; JavaScript
(add-to-list
 'color-identifiers:modes-alist
 `(js-mode . ("[^.][[:space:]]*"
              "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
              (nil font-lock-variable-name-face))))

(add-to-list
 'color-identifiers:modes-alist
 `(js2-mode . ("[^.][[:space:]]*"
               "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
               (nil font-lock-variable-name-face js2-function-param))))

(add-to-list
 'color-identifiers:modes-alist
 `(js3-mode . ("[^.][[:space:]]*"
               "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
               (nil font-lock-variable-name-face js3-function-param-face))))

;; CoffeeScript
;; May need to add the @ to the symbol syntax
;; (add-hook 'coffee-mode-hook (lambda () (modify-syntax-entry ?\@ "_"))) in .emacs
(add-to-list
 'color-identifiers:modes-alist
 `(coffee-mode . ("[^.][[:space:]]*" "\\_<\\([a-zA-Z_$@]\\(?:\\s_\\|\\sw\\)*\\)" (nil font-lock-variable-name-face))))

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
 `(ruby-mode . ("[^.][[:space:]]*" "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)" (nil))))

;; Groovy
(add-to-list
 'color-identifiers:modes-alist
 `(groovy-mode . ("[^.][[:space:]]*"
                  "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                  (nil font-lock-variable-name-face))))

;; Objective-C
(add-to-list
 'color-identifiers:modes-alist
 `(objc-mode . (nil
                "\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                (nil font-lock-variable-name-face)
                "[a-zA-Z_$]\\(\\s_\\|\\sw\\)*\\s-*[(:]")))

;; Python
(when (fboundp 'python-nav-forward-defun)
  (defun color-identifiers:python-get-declarations ()
    "Extract a list of identifiers declared in the current buffer.
For Python support within color-identifiers-mode.  Supports
function arguments and variable assignment, but not yet lambda
arguments, loops (for .. in), or for comprehensions."
    (let ((result nil))
      ;; Function arguments
      (save-excursion
        (goto-char (point-min))
        (while (python-nav-forward-defun)
          (let ((arglist (sexp-at-point)))
            (when (and arglist (listp arglist))
              (let* ((first-arg (car arglist))
                     (rest (cdr arglist))
                     (rest-args
                      (-map (lambda (token) (cadr token))
                            (-filter (lambda (token) (and (listp token) (eq (car token) '\,))) rest)))
                     (args-filtered (cons first-arg rest-args))
                     (params (-map (lambda (token)
                                     (car (split-string (symbol-name token) "=")))
                                   args-filtered)))
                (setq result (append params result)))))))
      ;; Variables that python-mode highlighted with font-lock-variable-name-face
      (save-excursion
        (goto-char (point-min))
        (catch 'end-of-file
          (while t
            (let ((next-change (next-property-change (point))))
              (if (not next-change)
                  (throw 'end-of-file nil)
                (goto-char next-change)
                (when (or (eq (get-text-property (point) 'face) 'font-lock-variable-name-face)
                          ;; If we fontified it in the past, assume it should
                          ;; continue to be fontified. This avoids alternating
                          ;; between fontified and unfontified.
                          (get-text-property (point) 'color-identifiers:fontified))
                  (push (substring-no-properties (symbol-name (symbol-at-point))) result)))))))
      (delete-dups result)
      result))
  (color-identifiers:set-declaration-scan-fn
   'python-mode 'color-identifiers:python-get-declarations))

(add-to-list
 'color-identifiers:modes-alist
 `(python-mode . ("[^.][[:space:]]*"
                  "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                  (nil font-lock-type-face font-lock-variable-name-face))))

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
    ((or `(defun ,- ,args . ,rest) `(lambda ,args . ,rest))
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
       result))
    (other-object nil)))

(defun color-identifiers:elisp-get-declarations ()
  "Extract a list of identifiers declared in the current buffer.
For Emacs Lisp support within color-identifiers-mode."
  (let ((result nil))
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let* ((sexp (read (current-buffer)))
                   (ids (color-identifiers:elisp-declarations-in-sexp sexp))
                   (strs (-filter 'identity
                                  (mapcar (lambda (id)
                                            (when (symbolp id) (symbol-name id)))
                                          ids))))
              (setq result (append strs result))))
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
                       (-filter 'evenp (number-sequence 0 (1- (length bindings)))))
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
    ((or `(defn ,- . ,rest)
         `(defn- ,- . ,rest)
         `(defmacro ,- . ,rest))
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
     (apply 'append (mapcar 'color-identifiers:clojure-declarations-in-sexp sexp)))
    (other-object nil)))

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
              (invalid-read-syntax nil)))
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


;;; PACKAGE INTERNALS ==========================================================

(defvar color-identifiers:timer nil
  "Timer for running `color-identifiers:refresh'.")

(defvar color-identifiers:identifiers nil
  "The set of identifiers in the current buffer, for internal use.")
(make-variable-buffer-local 'color-identifiers:identifiers)

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
         (candidates '())
         (chosens '())
         (n 8)
         (n-1 (float (1- n))))
    ;; Populate candidates with evenly spaced HSL colors with fixed luminance,
    ;; converted to LAB
    (dotimes (h n)
      (dotimes (s n)
        (add-to-list
         'candidates
         (apply 'color-srgb-to-lab
                (color-hsl-to-rgb (/ h n-1)
                                  (+ min-saturation (* (/ s n-1) saturation-range))
                                  luminance)))))
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
                                                    (cons bgcolor chosens)))))
                                candidates))
               ;; Take the candidate with the highest min distance
               (best (-max-by (lambda (x y) (> (cdr x) (cdr y))) min-dists)))
          (funcall choose-candidate (car best))))
      (setq color-identifiers:colors
            (-map (lambda (lab)
                    (apply 'color-rgb-to-hex (apply 'color-lab-to-srgb lab)))
                  chosens)))))

(defvar color-identifiers:color-index-for-identifier nil
  "Alist of identifier-index pairs for internal use.
The index refers to `color-identifiers:colors'.")
(make-variable-buffer-local 'color-identifiers:color-index-for-identifier)

(defvar color-identifiers:current-index 0
  "Current color index for new identifiers, for internal use.
The index refers to `color-identifiers:colors'.")
(make-variable-buffer-local 'color-identifiers:current-index)

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

(defun color-identifiers:refresh ()
  "Refresh `color-identifiers:color-index-for-identifier' from current buffer."
  (interactive)
  (when color-identifiers-mode
    (if (color-identifiers:get-declaration-scan-fn major-mode)
        (progn
          (setq color-identifiers:identifiers
                (funcall (color-identifiers:get-declaration-scan-fn major-mode)))
          (setq color-identifiers:color-index-for-identifier
                (-map-indexed (lambda (i identifier)
                                (cons identifier (% i color-identifiers:num-colors)))
                              color-identifiers:identifiers)))
      (save-excursion
        (goto-char (point-min))
        (catch 'input-pending
          (let ((i 0)
                (n color-identifiers:num-colors)
                (result nil))
            (color-identifiers:scan-identifiers
             (lambda (start end)
               (let ((identifier (buffer-substring-no-properties start end)))
                 (unless (assoc-string identifier result)
                   (push (cons identifier (% i n)) result)
                   (setq i (1+ i)))))
             (point-max)
             (lambda () (if (input-pending-p) (throw 'input-pending nil) t)))
            (setq color-identifiers:color-index-for-identifier result)))))
    (font-lock-fontify-buffer)))

(defun color-identifiers:color-identifier (identifier)
  "Look up or generate the hex color for IDENTIFIER.
IDENTIFIER is looked up in `color-identifiers:color-index-for-identifier' and
generated if not present there."
  (unless (and (color-identifiers:get-declaration-scan-fn major-mode)
               (not (member identifier color-identifiers:identifiers)))
    (let ((entry (assoc-string identifier color-identifiers:color-index-for-identifier)))
      (if entry
          (nth (cdr entry) color-identifiers:colors)
        ;; If not present, make a temporary color using the rotating index
        (push (cons identifier (% color-identifiers:current-index
                                  (length color-identifiers:colors)))
              color-identifiers:color-index-for-identifier)
        (setq color-identifiers:current-index
              (1+ color-identifiers:current-index))))))

(defun color-identifiers:scan-identifiers (fn limit &optional continue-p)
  "Run FN on all identifiers from point up to LIMIT.
Identifiers are defined by `color-identifiers:modes-alist'.
If supplied, iteration only continues if CONTINUE-P evaluates to true."
  (let ((entry (assoc major-mode color-identifiers:modes-alist)))
    (when entry
      (let ((identifier-context-re (nth 1 entry))
            (identifier-re (nth 2 entry))
            (identifier-faces
             (if (functionp (nth 3 entry))
                 (funcall (nth 3 entry))
               (nth 3 entry)))
            (identifier-exclusion-re (nth 4 entry)))
        ;; Skip forward to the next identifier that matches all four conditions
        (condition-case nil
            (while (and (< (point) limit)
                        (if continue-p (funcall continue-p) t))
              (if (not (or (memq (get-text-property (point) 'face) identifier-faces)
                           (let ((flface-prop (get-text-property (point) 'font-lock-face)))
                             (and flface-prop (memq flface-prop identifier-faces)))
                           (get-text-property (point) 'color-identifiers:fontified)))
                  (goto-char (next-property-change (point) nil limit))
                (if (not (and (looking-back identifier-context-re)
                              (or (not identifier-exclusion-re) (not (looking-at identifier-exclusion-re)))
                              (looking-at identifier-re)))
                    (progn
                      (forward-char)
                      (re-search-forward identifier-re limit)
                      (goto-char (match-beginning 0)))
                  ;; Found an identifier. Run `fn' on it
                  (funcall fn (match-beginning 1) (match-end 1))
                  (goto-char (match-end 1)))))
          (search-failed nil))))))

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
