(require 'ert)
(require 'color-identifiers-mode)

(defvar color-identifiers:c-mode-text
      ["struct {
    int struct_a;
    char struct_b;
} MyStruct;

int main() {
    int main_a = 1;
    main_a = 7;
    int *main_p = &main_a;
}" (("struct_a" . 1) ("struct_b" . 1) ("MyStruct" . 1) ("main_a" . 3) ("main_p" . 1))])

(defvar color-identifiers:elisp-mode-text
      ["(defun f (var1 var2)
  (+ var1 var2)
  (let ((var3 1))
   (1+ var3)))" (("var1" . 2) ("var2" . 2) ("var3" . 2))])

(defvar color-identifiers:python-mode-text
      ["def f(arg1, arg2: int):
    arg3 = arg1 + arg2" (("arg1" . 2) ("arg2" . 2) ("arg3" . 1))])

(defun color-identifiers:init-hash-table (list)
  "Initializes a hash-table with (key . val) pairs from list"
  (let ((table (make-hash-table :test 'equal)))
    ;; TODO: can't seem to find an easier way to initialize a hash-table
    (dolist (elem list)
      (puthash (car elem) (cdr elem) table))
    table))

(defun color-identifiers:we-fontified-point ()
  (get-text-property (point) 'color-identifiers:fontified))

(defun color-identifiers:all-identifiers-highlighted (ids)
  "Test that all identifiers in `ids' are highlighted in the buffer"
  (let ((highlights (make-hash-table :test 'equal))
        (next-change (next-property-change (point-min)))
        (identifier-context-re   (nth 1 color-identifiers:colorize-behavior))
        (identifier-re           (nth 2 color-identifiers:colorize-behavior))
        (identifier-exclusion-re (nth 4 color-identifiers:colorize-behavior)))
    (while next-change
      (goto-char next-change)
      (let ((context-matches
             (and (looking-back identifier-context-re (line-beginning-position))
                  (or (not identifier-exclusion-re) (not (looking-at identifier-exclusion-re)))))
            (maybe-id (progn
                        (if (looking-at identifier-re)
                            (buffer-substring-no-properties (match-beginning 1)
                                                            (match-end 1))
                          nil))))
        (if (color-identifiers:we-fontified-point)
            (progn
              (should context-matches)
              (should maybe-id)
              (should (gethash maybe-id ids))
              ;; increase match counter of the identifier found
              (let ((maybe-id-count (gethash maybe-id highlights)))
                (if maybe-id-count
                    (puthash maybe-id (1+ maybe-id-count) highlights)
                  (puthash maybe-id 1 highlights))))
          (when (and context-matches maybe-id)
            (should (null (gethash maybe-id ids))))))
      (setq next-change (next-property-change (point))))
    ;; Now test that the amount of identifiers highlighted is as expected
    (maphash (lambda (expected-id expected-value)
               (let ((curr-highlight (gethash expected-id highlights)))
                 (should curr-highlight)
                 (should (= curr-highlight expected-value))))
             ids)))

(defun color-identifiers:test-mode (mode-func text-to-test)
  "Creates a buffer with the text, enables a major mode with
`mode-func', enables `color-identifers-mode', then checks that
identifers are highlighted as expected"
  (let* ((buffer-content (aref text-to-test 0))
         (expected-ids (aref text-to-test 1))
         (expected-ids-table (color-identifiers:init-hash-table expected-ids)))
    (with-temp-buffer
      (insert buffer-content)
      (funcall mode-func)
      (goto-char 1)
      ;; most modes require (font-lock-fontify-buffer) for highlight to appear
      (font-lock-fontify-buffer)
      (color-identifiers-mode 1)
      ;; color-identifiers:scan-identifiers is called by font-lock when it considers
      ;; appropriate, so force it.
      (font-lock-fontify-buffer)
      (color-identifiers:all-identifiers-highlighted expected-ids-table))))

(ert-deftest test-c-mode-sequential ()
  (setq color-identifiers-coloring-method 'sequential)
  (color-identifiers:test-mode #'c-mode color-identifiers:c-mode-text))

(ert-deftest test-emacs-lisp-mode-sequential ()
  (setq color-identifiers-coloring-method 'sequential)
  (color-identifiers:test-mode
   #'emacs-lisp-mode color-identifiers:elisp-mode-text))

(ert-deftest test-python-mode-sequential ()
  (setq color-identifiers-coloring-method 'sequential)
  (color-identifiers:test-mode #'python-mode color-identifiers:python-mode-text))

(ert-deftest test-c-mode-hash ()
  (setq color-identifiers-coloring-method 'hash)
  (color-identifiers:test-mode #'c-mode color-identifiers:c-mode-text))

(ert-deftest test-emacs-lisp-mode-hash ()
  (setq color-identifiers-coloring-method 'hash)
  (color-identifiers:test-mode
   #'emacs-lisp-mode color-identifiers:elisp-mode-text))

(ert-deftest test-python-mode-hash ()
  (setq color-identifiers-coloring-method 'hash)
  (color-identifiers:test-mode #'python-mode color-identifiers:python-mode-text))
