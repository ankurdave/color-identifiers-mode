# Color Identifiers Mode
Color Identifiers is a minor mode for Emacs that highlights each source code identifier uniquely based on its name. It is inspired by a [post by Evan Brooks](https://medium.com/p/3a6db2743a1e/).

Currently it only supports js-mode and [scala-mode2](https://github.com/hvesalai/scala-mode2), but support for other modes is forthcoming. You can add support for your favorite mode by modifying `color-identifiers:modes-alist`.

![Demo of Color Identifiers Mode on Scala](https://raw.github.com/ankurdave/color-identifiers-mode/gh-pages/demo.gif)

## Installation
Color Identifiers is not in ELPA yet, so install it by cloning the repo and loading it in your Emacs init file:

    $ git clone https://github.com/ankurdave/color-identifiers-mode.git
    $ echo '(load "'$PWD'/color-identifiers-mode/color-identifiers-mode.el")' >> ~/.emacs

Then run Emacs, visit a Scala buffer, and type `M-x color-identifiers-mode`.
