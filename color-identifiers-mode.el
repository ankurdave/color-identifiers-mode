;;; color-identifiers-mode.el --- Color identifiers based on their names

;; Copyright (C) 2014 Ankur Dave

;; Author: Ankur Dave <ankurdave@gmail.com>
;; Url: https://github.com/ankurdave/color-identifiers-mode
;; Created: 24 Jan 2014
;; Version: 1.1
;; Keywords: faces, languages

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

;; Currently it only supports js-mode and scala-mode2, but support for other
;; modes is forthcoming.  You can add support for your favorite mode by modifying
;; `color-identifiers:modes-alist'.

;;; Code:

(require 'color)

;;;###autoload
(define-minor-mode color-identifiers-mode
  "Color the identifiers in the current buffer based on their names."
  :init-value nil
  :lighter " ColorIds"
  (if color-identifiers-mode
      (progn
        (font-lock-add-keywords nil '((color-identifiers:colorize . default)) t)
        (unless color-identifiers:timer
          (setq color-identifiers:timer
                (run-with-idle-timer 0.5 t 'color-identifiers:refresh))))
    (cancel-timer color-identifiers:timer)
    (setq color-identifiers:timer nil)
    (font-lock-remove-keywords nil '((color-identifiers:colorize . default))))
  (font-lock-fontify-buffer))

(add-to-list 'font-lock-extra-managed-props 'color-identifiers:fontified)

(defvar color-identifiers:timer nil)

(defvar color-identifiers:modes-alist nil
  "Alist of major modes and the ways to distinguish identifiers in those modes.
The value of each cons cell provides three constraints for finding identifiers.
A word must match all three constraints to be colored as an identifier.  The
value has the form (IDENTIFIER-CONTEXT-RE IDENTIFIER-RE IDENTIFIER-FACES).

IDENTIFIER-CONTEXT-RE is a regexp matching the text that must precede an
identifier.
IDENTIFIER-RE is a regexp whose first capture group matches identifiers.
IDENTIFIER-FACES is a list of faces with which the major mode decorates
identifiers or a function returning such a list.  If the list includes nil,
unfontified words will be considered.")

(add-to-list
 'color-identifiers:modes-alist
 `(scala-mode . ("[^.][[:space:]]*"
                 "\\b\\([[:lower:]]\\([_]??[[:lower:][:upper:]\\$0-9]+\\)*\\(_+[#:<=>@!%&*+/?\\\\^|~-]+\\|_\\)?\\)\\b"
                 (nil scala-font-lock:var-face font-lock-variable-name-face))))

(add-to-list
 'color-identifiers:modes-alist
 `(js-mode . (nil
              "\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
              (nil font-lock-variable-name-face))))

(add-to-list
 'color-identifiers:modes-alist
 `(ruby-mode . ("[^.][[:space:]]*" "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)" (nil))))

(defvar color-identifiers:num-colors 8
  "The number of different colors to generate.")

(defvar color-identifiers:color-index-for-identifier nil
  "Alist of identifier-index pairs for internal use.
The index refers to `color-identifiers:permanent-colors'.")

(defun color-identifiers:refresh ()
  "Refresh `color-identifiers:color-index-for-identifier' from current buffer."
  (interactive)
  (when color-identifiers-mode
    (save-excursion
      (goto-char (point-min))
      (setq color-identifiers:color-index-for-identifier nil)
      (let ((i 0)
            (n color-identifiers:num-colors))
        (color-identifiers:scan-identifiers
         (lambda (start end)
           (let ((identifier (buffer-substring-no-properties start end)))
             (unless (assoc-string identifier color-identifiers:color-index-for-identifier)
               (push (cons identifier (% i n))
                     color-identifiers:color-index-for-identifier)
               (setq i (1+ i)))))
         (point-max)
         (lambda () (not (input-pending-p)))))
      (font-lock-fontify-buffer))))

(defun color-identifiers:attribute-luminance (attribute)
  "Find the luminance of the specified ATTRIBUTE on the default face."
  (nth 0
       (apply 'color-srgb-to-lab
              (color-name-to-rgb
               (face-attribute 'default attribute)))))

(defun color-identifiers:make-color (i)
  "Generate a hex color for color number i of `color-identifiers:num-colors'."
  (let* ((foreground-luminance (color-identifiers:attribute-luminance :foreground))
         (background-luminance (color-identifiers:attribute-luminance :background))
         (L (min 80 (max 45 foreground-luminance)))
         (contrast (abs (- foreground-luminance background-luminance)))
         (chroma (min 60 (max 30 background-luminance)))
         (hue (* (/ i (float color-identifiers:num-colors)) pi))
         (a (* chroma (cos hue)))
         (b (* chroma (sin hue))))
    (apply 'color-rgb-to-hex (color-lab-to-srgb L a b))))

(defun color-identifiers:color-identifier (identifier)
  "Look up or generate the hex color for IDENTIFIER.
IDENTIFIER is looked up in `color-identifiers:color-index-for-identifier' and
generated if not present there."
  (let ((entry (assoc-string identifier color-identifiers:color-index-for-identifier)))
    (if entry
        (color-identifiers:make-color (cdr entry))
      nil)))

(defun color-identifiers:scan-identifiers (fn limit &optional continue-p)
  "Run FN on all identifiers from point up to LIMIT.
Identifiers are defined by `color-identifiers:modes-alist'.
If supplied, iteration only continues if CONTINUE-P evaluates to true."
  (let ((entry (assoc major-mode color-identifiers:modes-alist)))
    (when entry
      (let ((identifier-context-re (cadr entry))
            (identifier-re (caddr entry))
            (identifier-faces
             (if (functionp (cadddr entry))
                 (funcall (cadddr entry))
               (cadddr entry))))
        ;; Skip forward to the next identifier that matches all three conditions
        (condition-case nil
            (while (and (< (point) limit)
                        (if continue-p (funcall continue-p) t))
              (if (not (or (memq (get-text-property (point) 'face) identifier-faces)
                           (get-text-property (point) 'color-identifiers:fontified)))
                  (goto-char (next-property-change (point) nil limit))
                (if (not (and (looking-back identifier-context-re)
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

(provide 'color-identifiers-mode)

;;; color-identifiers-mode.el ends here
