# persist-text-scale.el
![Build Status](https://github.com/jamescherti/persist-text-scale.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/persist-text-scale.el)
![](https://raw.githubusercontent.com/jamescherti/persist-text-scale.el/main/.images/made-for-gnu-emacs.svg)

Persist and restore text scale.

## Installation

### Emacs: Install with straight (Emacs version < 30)

To install *persist-text-scale* with `straight.el`:

1. It if hasn't already been done, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package persist-text-scale
  :ensure t
  :straight (persist-text-scale
             :type git
             :host github
             :repo "jamescherti/persist-text-scale.el"))
```

### Emacs: Installing with use-package and :vc (Built-in feature in Emacs version >= 30)

To install *persist-text-scale* with `use-package` and `:vc` (Emacs >= 30):

``` emacs-lisp
(use-package persist-text-scale
  :ensure t
  :vc (:url "https://github.com/jamescherti/persist-text-scale.el"
       :rev :newest))
```

## Doom Emacs

Here is how to install *compile-angel* on Doom Emacs:

1. Add to the `~/.doom.d/packages.el` file:
```elisp
(package! persist-text-scale
 :recipe
 (:host github :repo "jamescherti/persist-text-scale.el"))
```

2. Add to `~/.doom.d/config.el`:
```elisp
;; TODO: Load the mode here
(after! persist-text-scale
 ;; TODO: setq options
 )
```

3. Run the `doom sync` command:
```
doom sync
```

## Author and License

The *persist-text-scale* Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2025 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [persist-text-scale.el @GitHub](https://github.com/jamescherti/persist-text-scale.el)
