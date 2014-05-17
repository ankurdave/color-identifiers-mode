# Color Identifiers Mode
Color Identifiers is a minor mode for Emacs that highlights each source code identifier uniquely based on its name. It is inspired by a [post by Evan Brooks](https://medium.com/p/3a6db2743a1e/).

Currently it supports Scala (scala-mode2), JavaScript (js-mode and js2-mode), Ruby, Python, Emacs Lisp, Clojure, C, C++, and Java. You can add support for your favorite mode by modifying `color-identifiers:modes-alist` and optionally calling `color-identifiers:set-declaration-scan-fn`.

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
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
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

## Extras

To make the variables stand out, you can turn off highlighting for all other keywords using code similar to the following:
```lisp
(let ((faces '(font-lock-comment-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-type-face font-lock-function-name-face font-lock-variable-name-face font-lock-keyword-face font-lock-string-face font-lock-builtin-face font-lock-preprocessor-face font-lock-warning-face font-lock-doc-face)))
  (dolist (face faces)
    (set-face-attribute face nil :foreground nil :weight 'normal :slant 'normal)))

(set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-doc-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
(set-face-attribute 'font-lock-builtin-face nil :weight 'bold)
(set-face-attribute 'font-lock-preprocessor-face nil :weight 'bold)
```

![Other Keywords Dimmed](https://raw.github.com/ankurdave/color-identifiers-mode/gh-pages/dim-other-keywords.png)
