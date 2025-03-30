# persist-text-scale.el - Persist and Restore the Text Scale
![Build Status](https://github.com/jamescherti/persist-text-scale.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/persist-text-scale.el)
![](https://raw.githubusercontent.com/jamescherti/persist-text-scale.el/main/.images/made-for-gnu-emacs.svg)

The **persist-text-scale** Emacs package provides `persist-text-scale-mode`, which ensures that all adjustments made with `text-scale-increase` and `text-scale-decrease` are persisted and restored across sessions. As a result, the text size in each buffer remains consistent, even after restarting Emacs.

This package also facilitates grouping buffers into categories, allowing buffers within the same category to share a consistent text scale. This ensures uniform font sizes when adjusting text scaling. By default:
- Each file-visiting buffer has its own independent text scale.
- Special buffers, identified by their buffer names, each retain their own text scale setting.
- All Dired buffers maintain the same font size, treating Dired as a unified "file explorer" where the text scale remains consistent across different buffers.

This category-based behavior can be further customized by assigning a function to the `persist-text-scale-buffer-category-function` variable. The function determines how buffers are categorized by returning a category identifier (string) based on the buffer's context. Buffers within the same category will share the same text scale.

## Features

- Lightweight and efficient, requiring minimal configuration.
- Automatically saves and restores the text scale for all buffer types, including file, indirect, dired, and special buffers.
- Periodically saves text scale data at intervals defined by `persist-text-scale-autosave-interval`, which can be set to `nil` to disable or specified in seconds to enable.
- Provides unified text scaling across buffer categories, with fully customizable logic for categorizing buffers based on text scale. Users can customize categorization by specifying a function for the `persist-text-scale-buffer-category-function` variable, ensuring that groups of buffers maintain consistent text scale persistence and restoration.
- The user can define the maximum number of retained entries using `persist-text-scale-history-length`.

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
  :custom
  ;; Time interval, in seconds, between automatic saves of text scale data.
  ;; If set to an integer value, enables periodic autosaving of persisted text
  ;; scale information at the specified interval.
  ;; If set to nil, disables timer-based autosaving entirely.
  (persist-text-scale-autosave-interval (* 7 60))
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
  :custom
  ;; Time interval, in seconds, between automatic saves of text scale data.
  ;; If set to an integer value, enables periodic autosaving of persisted text
  ;; scale information at the specified interval.
  ;; If set to nil, disables timer-based autosaving entirely.
  (persist-text-scale-autosave-interval (* 7 60))
  :config
  (persist-text-scale-mode))
```

## Doom Emacs

Here is how to install *persist-text-scale* on Doom Emacs:

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
  ;; Time interval, in seconds, between automatic saves of text scale data.
  ;; If set to an integer value, enables periodic autosaving of persisted text
  ;; scale information at the specified interval.
  ;; If set to nil, disables timer-based autosaving entirely.
  (setq persist-text-scale-autosave-interval (* 7 60))

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
- [Text Scale (Emacs documentation)](https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Scale.html)

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
