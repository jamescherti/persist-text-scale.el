# persist-text-scale.el - Persist and Restore the Text Scale for Files and Special Buffers
![Build Status](https://github.com/jamescherti/persist-text-scale.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/persist-text-scale.el)
![](https://raw.githubusercontent.com/jamescherti/persist-text-scale.el/main/.images/made-for-gnu-emacs.svg)

The **persist-text-scale** is an Emacs package that persists and restores the text scale for files and special buffers. By default, it saves the text scale individually for each file and applies a shared text scale for categories of special buffers. This behavior ensures a consistent and personalized reading experience across sessions.

You can customize how buffer categories are determined by setting using a function. This function should return a category identifier based on the buffer context. Buffers within the same category will share the same text scale.

## Features

- Automatically persists the text scale for every file buffer.
- Applies a shared text scale for categories of special buffers.
- Fully customizable categorization logic for text scale grouping.
- Lightweight and efficient with minimal configuration.

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
             :repo "jamescherti/persist-text-scale.el")
  :config
  (persist-text-scale-mode))
```

### Emacs: Installing with use-package and :vc (Built-in feature in Emacs version >= 30)

To install *persist-text-scale* with `use-package` and `:vc` (Emacs >= 30):

``` emacs-lisp
(use-package persist-text-scale
  :ensure t
  :vc (:url "https://github.com/jamescherti/persist-text-scale.el"
       :rev :newest)
  :config
  (persist-text-scale-mode))
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
  (persist-text-scale-mode))
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

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el): An Emacs package that prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking their execution if they would break the parenthetical structure.
