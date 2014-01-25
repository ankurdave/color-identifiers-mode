;;; color-identifiers-mode.el --- Color identifiers based on their names

;; Copyright (C) 2014 Ankur Dave

;; Author: Ankur Dave <ankurdave@gmail.com>
;; Created: 24 Jan 2014
;; Version: 1.0
;; Keywords: faces, languages

;;; Commentary:

;; This package provides a minor mode to color identifiers based on their
;; names, inspired by Evan Brooks: https://medium.com/p/3a6db2743a1e/. To
;; activate it, type M-x color-identifiers-mode in a code buffer.  Currently
;; only scala-mode is supported.

(require 'color)

;;;###autoload
(define-minor-mode color-identifiers-mode
  "Color the identifiers in the current buffer based on their names."
  :init-value nil
  :lighter " ColorIds"
  (if color-identifiers-mode
      (font-lock-add-keywords nil '((color-identifiers:colorize . color-identifiers:no-op-face)) t)
    (font-lock-remove-keywords nil '((color-identifiers:colorize . color-identifiers:no-op-face))))
  (font-lock-fontify-buffer))

(defface color-identifiers:no-op-face nil nil)

(defvar color-identifiers:modes-alist nil
  "Alist of major modes and the ways to distinguish identifiers in those modes.

The value of each cons cell has the form (IDENTIFIER-RE (IDENTIFIER-FACE...)).
IDENTIFIER-RE is a regexp whose first capture group matches identifiers.
If a major mode decorates identifiers with a particular face, include it as an
IDENTIFIER-FACE for that mode. An IDENTIFIER-FACE of nil means to include all
unfontified words.

A word must match IDENTIFIER-RE and be decorated with an IDENTIFIER-FACE to be
colored as an identifier.")

(when (require 'scala-mode2 nil t)
  (add-to-list
   'color-identifiers:modes-alist
   `(scala-mode . (,(concat "[^.][[:space:]]*\\b\\(" scala-syntax:varid-re "\\)\\b[[:space:]]*[^(]")
                   (nil scala-font-lock:var-face font-lock-variable-name-face)))))

(when (require 'js nil t)
  (add-to-list
   'color-identifiers:modes-alist
   `(js-mode . (,(concat "\\(" js--name-re "\\)") (nil font-lock-variable-name-face)))))

(defun color-identifiers:color-identifier (str)
  (let* ((hash (sxhash str))
         (hue (/ (% (abs hash) 100) 100.0)))
    (apply 'color-rgb-to-hex (color-hsl-to-rgb hue 0.8 0.8))))

(defun color-identifiers:colorize (limit)
  "Colorize all unfontified identifiers from point to LIMIT."
  (let ((entry (assoc major-mode color-identifiers:modes-alist)))
    (when entry
      (let ((identifier-re (cadr entry))
            (identifier-faces (caddr entry)))
        ;; Skip forward to the next appropriate text to colorize
        (condition-case nil
            (while (< (point) limit)
              (if (not (memq (get-text-property (point) 'face) identifier-faces))
                  (goto-char (next-property-change (point) nil limit))
                (if (not (looking-at identifier-re))
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
