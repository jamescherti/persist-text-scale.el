# persist-text-scale.el - Persist and Restore the Text Scale
![Build Status](https://github.com/jamescherti/persist-text-scale.el/actions/workflows/ci.yml/badge.svg)
[![MELPA](https://melpa.org/packages/persist-text-scale-badge.svg)](https://melpa.org/#/persist-text-scale)
[![MELPA Stable](https://stable.melpa.org/packages/persist-text-scale-badge.svg)](https://stable.melpa.org/#/persist-text-scale)
![License](https://img.shields.io/github/license/jamescherti/persist-text-scale.el)
![](https://raw.githubusercontent.com/jamescherti/persist-text-scale.el/main/.images/made-for-gnu-emacs.svg)

The **persist-text-scale** Emacs package provides `persist-text-scale-mode`, which ensures that all adjustments made with `text-scale-increase` and `text-scale-decrease` are persisted and restored across sessions. As a result, the text size in each buffer remains consistent, even after restarting Emacs.

This package also facilitates grouping buffers into categories, allowing buffers within the same category to share a consistent text scale. This ensures uniform font sizes when adjusting text scaling. By default:
- Each file-visiting buffer has its own independent text scale.
- Special buffers, identified by their buffer names, each retain their own text scale setting.
- All Dired buffers maintain the same font size, treating Dired as a unified "file explorer" where the text scale remains consistent across different buffers.

This category-based behavior can be further customized by assigning a function to the `persist-text-scale-buffer-category-function` variable. The function determines how buffers are categorized by returning a category identifier (string) based on the buffer's context. Buffers within the same category will share the same text scale.

If this enhances your workflow, please show your support by **⭐ starring persist-text-scale on GitHub** to help more Emacs users discover its benefits.

## Features

- Lightweight and efficient, requiring minimal configuration.
- Automatically saves and restores the text scale for all buffer types, including file, indirect, dired, and special buffers.
- Periodically saves text scale data at intervals defined by `persist-text-scale-autosave-interval`, which can be set to `nil` to disable or specified in seconds to enable.
- Provides unified text scaling across buffer categories, with fully customizable logic for categorizing buffers based on text scale. Users can customize categorization by specifying a function for the `persist-text-scale-buffer-category-function` variable, ensuring that groups of buffers maintain consistent text scale persistence and restoration.
- The user can define the maximum number of retained entries using `persist-text-scale-history-length`.

## Installation

### Emacs

To install *persist-text-scale* from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).

2. Add the following code to your Emacs init file to install *persist-text-scale* from MELPA:

```emacs-lisp
(use-package persist-text-scale
  :ensure t
  :custom
  ;; Time interval, in seconds, between automatic saves of text scale data.
  ;; If set to an integer value, enables periodic autosaving of persisted text
  ;; scale information at the specified interval.
  ;; If set to nil, disables timer-based autosaving entirely.
  (persist-text-scale-autosave-interval (* 7 60))
  :config
  (persist-text-scale-mode))
```

### Alternative: Doom Emacs

Here is how to install *persist-text-scale* on Doom Emacs:

1. Add to the `~/.doom.d/packages.el` file:
```elisp
(package! persist-text-scale)
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

## Customizations

### `persist-text-scale-autosave-interval`

- **Type:** Integer or `nil`
- **Default:** `(* 11 60)` seconds (11 minutes)

Defines the time interval, in seconds, between automatic saves of text scale data.

- Setting an integer enables periodic autosaving at the specified interval.
- Setting it to `nil` disables timer-based autosaving entirely.

This ensures that text scale adjustments are preserved automatically without requiring manual saving.

### `persist-text-scale-history-length`

- **Type:** Integer or `nil`
- **Default:** `100`

Specifies the maximum number of entries to retain. Each entry corresponds to a buffer category (e.g., file-visiting buffers, special buffers).

- If set to an integer, older entries are deleted once the limit is reached.
- If set to `nil`, cleanup is disabled and no entries are removed. **(not recommended)**

This helps manage memory usage and prevents the data file from growing indefinitely.

### `persist-text-scale-buffer-category-function`

- **Type:** Function or `nil`
- **Default:** `nil`

Allows custom classification of buffers for text scale persistence. When provided, this function overrides the default classification.

Here is an example:
```elisp
(defun my-persist-text-scale-function ()
    (let ((buffer-name (buffer-name)))
      (cond
       ((string-prefix-p "*Embark Export:" buffer-name)
        "category:embark-export")

       ((string-prefix-p "*sdcv:" buffer-name)
        "category:sdcv"))))

(setq persist-text-scale-buffer-category-function 'my-persist-text-scale-function)
```

The function must return one of:

* A string or symbol representing the buffer category (for grouping purposes),
* `:ignore` to exclude the buffer from persistence,
* `nil` to defer to the default *persist-text-scale* classification.

This option provides flexibility in defining how text scale settings are grouped and applied across different types of buffers.

### `persist-text-scale-restore-once`

- **Type:** Boolean
- **Default:** `nil`

Controls whether the text scale is restored only once per buffer.

* When non-nil, the text scale is applied either when the buffer is first loaded or when it is displayed in a window for the first time.
* Subsequent window changes or re-displays of the buffer do not trigger additional restorations.

If you are unsure, it is recommended to leave this option as `nil` to allow normal repeated restoration behavior.

### `persist-text-scale-handle-file-renames`

- **Type:** Boolean
- **Default:** `t`

Determines whether text scale settings are preserved when a buffer’s underlying file is renamed.

* When enabled, the buffer association is updated to the new file path, ensuring that the previously configured text scale remains applied.
* When disabled, renaming a file resets its text scale to the default value.

These options give users control over messaging, restoration frequency, and resilience to file renames, improving both usability and reliability of text scale persistence.

### `persist-text-scale-fallback-to-previous-scale`

* **Type:** boolean
* **Default:** `t`

The `persist-text-scale-fallback-to-previous-scale` option allows `persist-text-scale-mode` to use the last used text scale when a buffer category does not yet have a defined scale (i.e., the text scale for this category has never been changed).

This is useful if you frequently switch between buffers or modes that have not been explicitly assigned a text scale, maintaining readability without manual adjustment.

### `persist-text-scale-verbose`

- **Type:** Boolean
- **Default:** `nil`

When enabled (`t`), `persist-text-scale` displays informative messages during text scale restoration. These messages indicate when and how the text scale was restored, which is useful for debugging or monitoring the package's behavior.

## Author and License

The *persist-text-scale* Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2025 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [persist-text-scale.el @GitHub](https://github.com/jamescherti/persist-text-scale.el)
- [persist-text-scale.el @MELPA](https://melpa.org/#/persist-text-scale)
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
- [stripspace.el](https://github.com/jamescherti/stripspace.el): Ensure Emacs Automatically removes trailing whitespace before saving a buffer, with an option to preserve the cursor column.
- [pathaction.el](https://github.com/jamescherti/pathaction.el): Execute the pathaction command-line tool from Emacs. The pathaction command-line tool enables the execution of specific commands on targeted files or directories. Its key advantage lies in its flexibility, allowing users to handle various types of files simply by passing the file or directory as an argument to the pathaction tool. The tool uses a .pathaction.yaml rule-set file to determine which command to execute. Additionally, Jinja2 templating can be employed in the rule-set file to further customize the commands.
