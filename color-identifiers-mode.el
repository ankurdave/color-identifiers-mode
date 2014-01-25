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

(defun color-identifiers:colorize (limit)
  "Colorize all unfontified identifiers from point to LIMIT."
  ;; Skip forward to the next appropriate text to colorize
  (condition-case nil
      (while (< (point) limit)
        (if (not (memq (get-text-property (point) 'face) '(nil scala-font-lock:var-face)))
            (goto-char (next-property-change (point) nil limit))
          (if (not (looking-at scala-syntax:varid-re))
              (progn
                (forward-char)
                (re-search-forward (concat "\\b[" scala-syntax:lower-group "]") limit)
                (goto-char (match-beginning 0)))
            ;; Colorize the text according to its name
            (let* ((hash (sxhash (buffer-substring
                                  (match-beginning 0) (match-end 0))))
                   (hue (/ (% (abs hash) 100) 100.0))
                   (hex (apply 'color-rgb-to-hex (color-hsl-to-rgb hue 0.8 0.8))))
              (put-text-property (match-beginning 0) (match-end 0)
                                 'face `(:foreground ,hex)))
            (goto-char (match-end 0)))))
    (search-failed nil)))
