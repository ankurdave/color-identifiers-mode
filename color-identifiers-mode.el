;;; color-identifiers-mode.el --- Color identifiers based on their names

;; Copyright (C) 2014 Ankur Dave

;; Author: Ankur Dave <ankurdave@gmail.com>
;; Url: https://github.com/ankurdave/color-identifiers-mode
;; Created: 24 Jan 2014
;; Version: 1.0
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
;; `color-identifiers:modes-alist`.

(require 'color)

;;; Code:

;;;###autoload
(define-minor-mode color-identifiers-mode
  "Color the identifiers in the current buffer based on their names."
  :init-value nil
  :lighter " ColorIds"
  (if color-identifiers-mode
      (font-lock-add-keywords nil '((color-identifiers:colorize . default)) t)
    (font-lock-remove-keywords nil '((color-identifiers:colorize . default))))
  (font-lock-fontify-buffer))

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

(defun color-identifiers:color-identifier (identifier)
  "Generate the hex color for IDENTIFIER."
  (let* ((hash (sxhash identifier))
         (hue (/ (% (abs hash) 100) 100.0)))
    (apply 'color-rgb-to-hex (color-hsl-to-rgb hue 0.8 0.8))))

(defun color-identifiers:colorize (limit)
  "Color identifiers in the current buffer from point to LIMIT.
Identifiers to color are specified by `color-identifiers:modes-alist`."
  (let ((entry (assoc major-mode color-identifiers:modes-alist)))
    (when entry
      (let ((identifier-context-re (cadr entry))
            (identifier-re (caddr entry))
            (identifier-faces
             (if (functionp (cadddr entry))
                 (funcall (cadddr entry))
               (cadddr entry))))
        ;; Skip forward to the next appropriate text to colorize
        (condition-case nil
            (while (< (point) limit)
              (if (not (memq (get-text-property (point) 'face) identifier-faces))
                  (goto-char (next-property-change (point) nil limit))
                (if (not (and (looking-back identifier-context-re)
                              (looking-at identifier-re)))
                    (progn
                      (forward-char)
                      (re-search-forward identifier-re limit)
                      (goto-char (match-beginning 0)))
                  ;; Colorize the text according to its name
                  (let* ((string (buffer-substring
                                  (match-beginning 1) (match-end 1)))
                         (hex (color-identifiers:color-identifier string)))
                    (put-text-property (match-beginning 1) (match-end 1)
                                       'face `(:foreground ,hex)))
                  (goto-char (match-end 1)))))
          (search-failed nil))))))

(provide 'color-identifiers-mode)

;;; color-identifiers-mode.el ends here
