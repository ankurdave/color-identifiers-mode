# Color Identifiers Mode
Color Identifiers is a minor mode for Emacs that highlights each source code identifier uniquely based on its name. It is inspired by a [post by Evan Brooks](https://medium.com/p/3a6db2743a1e/).

Currently it supports scala-mode2, js-mode, js2-mode, ruby-mode, python-mode, emacs-lisp-mode, and clojure-mode. You can add support for your favorite mode by modifying `color-identifiers:modes-alist` and optionally calling `color-identifiers:set-declaration-scan-fn`.

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

If you like it, enable it for all supported files by setting `global-color-identifiers-mode`:

```
M-x customize-save-variable RET global-color-identifiers-mode RET y
```
