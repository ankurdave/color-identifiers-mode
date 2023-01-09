# Color Identifiers Mode
Color Identifiers is a minor mode for Emacs that highlights each source code identifier uniquely based on its name. It is inspired by a [post by Evan Brooks](https://medium.com/p/3a6db2743a1e/).

Currently it supports Scala (scala-mode2), JavaScript (js-mode and js2-mode), Ruby, Python, Emacs Lisp, Clojure, C, C++, Rust, Java, and Go. You can add support for your favorite mode by modifying `color-identifiers:modes-alist` and optionally calling `color-identifiers:set-declaration-scan-fn`.

[Check out the demo.](http://youtu.be/g4qsiAo2aac)

![Screenshot of Color Identifiers Mode on Scala](https://raw.github.com/ankurdave/color-identifiers-mode/gh-pages/demo-static.png)

It picks colors adaptively to fit the theme:

![Different Themes](https://raw.github.com/ankurdave/color-identifiers-mode/gh-pages/themes.png)

Use `M-x color-identifiers:regenerate-colors` after a theme change.

## Installation
Color Identifiers is in [MELPA](https://github.com/milkypostman/melpa/pull/1416). First [set up MELPA](https://github.com/milkypostman/melpa#usage):

```lisp
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents)
```

Then install it:

```lisp
(package-install 'color-identifiers-mode)
```

Finally, visit a supported file and type `M-x color-identifiers-mode`.

If you like it, enable it for all supported files by adding the following to your init file:

```lisp
(add-hook 'after-init-hook 'global-color-identifiers-mode)
```

## Configuration

* Recoloring delay: the time before recoloring newly appeared identifiers is `5` seconds by default. To change it e.g. to `2` seconds add to your config `(setq color-identifiers:recoloring-delay 2 )`
* To make the variables stand out, you can turn off highlighting for all other keywords in supported modes using a code like:
    ```lisp
    (defun myfunc-color-identifiers-mode-hook ()
      (let ((faces '(font-lock-comment-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-type-face font-lock-function-name-face font-lock-variable-name-face font-lock-keyword-face font-lock-string-face font-lock-builtin-face font-lock-preprocessor-face font-lock-warning-face font-lock-doc-face font-lock-negation-char-face font-lock-regexp-grouping-construct font-lock-regexp-grouping-backslash)))
        (dolist (face faces)
          (face-remap-add-relative face '((:foreground "" :weight normal :slant normal)))))
      (face-remap-add-relative 'font-lock-keyword-face '((:weight bold)))
      (face-remap-add-relative 'font-lock-comment-face '((:slant italic)))
      (face-remap-add-relative 'font-lock-builtin-face '((:weight bold)))
      (face-remap-add-relative 'font-lock-preprocessor-face '((:weight bold)))
      (face-remap-add-relative 'font-lock-function-name-face '((:slant italic)))
      (face-remap-add-relative 'font-lock-string-face '((:slant italic)))
      (face-remap-add-relative 'font-lock-constant-face '((:weight bold))))
    (add-hook 'color-identifiers-mode-hook 'myfunc-color-identifiers-mode-hook)
    ```

    ![Other Keywords Dimmed](https://raw.github.com/ankurdave/color-identifiers-mode/gh-pages/dim-other-keywords.png)
