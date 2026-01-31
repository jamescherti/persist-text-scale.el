;;; persist-text-scale.el --- Persist and restore text scale -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.4
;; URL: https://github.com/jamescherti/persist-text-scale.el
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; The persist-text-scale Emacs package provides persist-text-scale-mode, which
;; ensures that all adjustments made with text-scale-increase and
;; text-scale-decrease are persisted and restored across sessions. As a result,
;; the text size in each buffer remains consistent, even after restarting Emacs.
;;
;; This package also facilitates grouping buffers into categories, allowing
;; buffers within the same category to share a consistent text scale. This
;; ensures uniform font sizes when adjusting text scaling. By default:
;; - Each file-visiting buffer has its own independent text scale.
;; - Special buffers, identified by their buffer names, each retain their own
;;   text scale setting.
;; - All Dired buffers maintain the same font size, treating Dired as a unified
;;   "file explorer" where the text scale remains consistent across different
;;   buffers.
;;
;; This category-based behavior can be further customized by assigning a
;; function to the persist-text-scale-buffer-category-function variable. The
;; function determines how buffers are categorized by returning a category
;; identifier (string) based on the buffer's context. Buffers within the same
;; category will share the same text scale.

;;; Code:

;;; Require

(require 'face-remap)
(require 'cl-lib)

;;; Defcustom

(defgroup persist-text-scale nil
  "Non-nil if persist-text-scale mode is enabled."
  :group 'persist-text-scale
  :prefix "persist-text-scale-")

(defcustom persist-text-scale-file (expand-file-name "persist-text-scale"
                                                     user-emacs-directory)
  "File where the persist-text-scale data is stored.
This file holds the data for persisting the text scale across sessions.
It can be customized to a different file path as needed."
  :type 'file
  :group 'persist-text-scale)

(defcustom persist-text-scale-autosave-interval (* 11 60)
  "Time interval, in seconds, between automatic saves of text scale data.
If set to an integer value, enables periodic autosaving of persisted text scale
information at the specified interval.
If set to nil, disables timer-based autosaving entirely."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'persist-text-scale)

(defcustom persist-text-scale-history-length 100
  "Maximum number of entries to retain.
Entries represent categories such as file-visiting buffers, special buffers,
etc. If set to nil, cleanup is disabled and no entries will be deleted."
  :type '(choice (integer :tag "Maximum number of entries")
                 (const :tag "Disable cleanup" nil))
  :group 'persist-text-scale)

(defcustom persist-text-scale-buffer-category-function nil
  "Function to determine the buffer category for text scale persistence.
If non-nil, this function overrides `persist-text-scale--buffer-category'
and is called to classify the buffer for text scaling purposes.

It must return one of the following values:
- A string or symbol representing the buffer category (for grouping),
- :ignore to exclude the buffer from persistence,
- nil to defer to the default classification via
`persist-text-scale--buffer-category'.

This allows customization of how buffers are grouped when persisting text scale
settings."
  :type '(choice (const :tag "None" nil) function)
  :group 'persist-text-scale)

(defcustom persist-text-scale-verbose nil
  "If non-nil, display informative messages during text scale restoration.
These messages will indicate when and how the text scale was restored, aiding
in debugging or monitoring behavior."
  :type 'boolean
  :group 'persist-text-scale)

(defcustom persist-text-scale-restore-once nil
  "If non-nil, restore text scale only once per buffer.
When non-nil, the text scale will be restored either when the buffer is loaded
or when the buffer is displayed in a window for the first time. Subsequent
window changes will not trigger additional restoration.

When unsure, leave this value as nil."
  :type 'boolean
  :group 'persist-text-scale)

(defcustom persist-text-scale-handle-file-renames t
  "If non-nil, preserve text scale settings when a buffer's file is renamed.
Updates the buffer association to the new file path to maintain consistency.
Without this, renaming a file resets the text scale."
  :type 'boolean
  :group 'persist-text-scale)

(defcustom persist-text-scale-fallback-to-previous-scale t
  "When non-nil, use the previous text scale amount.
The previous text scale amount is applied when no scale is defined for the
current category."
  :type 'boolean
  :group 'persist-text-scale)

;;; Variables

(defvar persist-text-scale-depth-window-buffer-change-functions -99)
(defvar persist-text-scale-depth-text-scale-mode -99)

(defvar persist-text-scale-default-text-scale-amount nil
  "Fallback text scale amount used when no value is available for restoration.")

;;; Internal variables

(defvar persist-text-scale--data nil
  "Alist mapping buffer identifiers to their corresponding text scale amount.")

(defvar persist-text-scale--last-text-scale-amount nil
  "Most recent text scale amount selected by the user.
This value reflects the numeric text scale adjustment applied in the last
interactive text scale change and is used internally to support restoration.")

(defvar-local persist-text-scale--update-last-text-scale-amount t)

(defvar-local persist-text-scale--restored-amount nil
  "Non-nil indicates that the buffer text scale has been restored.
This value is set by `persist-text-scale-restore'")

(defvar-local persist-text-scale--persisted-amount nil
  "Non-nil indicates that the buffer text scale has been persisted.
This value is set by `persist-text-scale-persist'.")

(defvar-local persist-text-scale--filename nil
  "This is used to handle renames.")

(defvar-local persist-text-scale--indirect-buffer-initialized nil)

(defvar persist-text-scale--timer nil)

;;; Internal functions and macros

(defmacro persist-text-scale--verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  (declare (indent 0) (debug t))
  `(progn
     (when persist-text-scale-verbose
       (message (concat "[persist-text-scale] " ,(car args)) ,@(cdr args)))))

(defun persist-text-scale--cancel-timer ()
  "Cancel `persist-text-scale-autosave' timer, if set."
  (when (timerp persist-text-scale--timer)
    (cancel-timer persist-text-scale--timer))
  (setq persist-text-scale--timer nil))

(defun persist-text-scale--manage-timer ()
  "Set or cancel an invocation of `persist-text-scale-autosave' on a timer.
If `persist-text-scale-mode' is enabled, set the timer, otherwise cancel the
timer."
  (persist-text-scale--cancel-timer)
  (if (and (bound-and-true-p persist-text-scale-mode)
           persist-text-scale-autosave-interval
           (null persist-text-scale--timer))
      (setq persist-text-scale--timer
            (run-with-timer persist-text-scale-autosave-interval
                            persist-text-scale-autosave-interval
                            #'persist-text-scale-save-file))))

(defun persist-text-scale--buffer-category ()
  "Generate a unique name for the current buffer.
Returns a unique identifier string based."
  (let (result)
    (when persist-text-scale-buffer-category-function
      (setq result (funcall persist-text-scale-buffer-category-function)))

    (unless result
      (setq result (let* ((base-buffer (buffer-base-buffer))
                          (file-name (buffer-file-name base-buffer))
                          (buffer-name (buffer-name)))
                     (cond
                      ;; Ignore old buffers
                      ((or (string-prefix-p " *Old buffer" buffer-name)
                           (string-prefix-p " *corfu" buffer-name))
                       :ignore)

                      ;; File visiting indirect buffers
                      ((and base-buffer file-name)
                       (format
                        "fib%s:%s"
                        (persist-text-scale--buffer-name-suffix-number
                         buffer-name)
                        (file-truename file-name)))

                      ;; Mini buffers
                      ((and (not file-name)
                            (or (string-prefix-p " *Minibuf" buffer-name)))
                       "sp: *Minibuf")

                      ;; Special modes whose major-modes are in the same
                      ;; category
                      ((and (boundp 'major-mode)
                            (or (derived-mode-p 'woman-mode)
                                (derived-mode-p 'help-mode)))
                       (let ((major-mode-symbol (symbol-name major-mode)))
                         (format "mm:%s" major-mode-symbol)))

                      ;; File visiting buffers
                      (file-name
                       (format "f:%s" (file-truename file-name)))

                      ;; Special buffers
                      ((and (not file-name)
                            (or (string-prefix-p "*" buffer-name)
                                (string-prefix-p " " buffer-name)
                                (derived-mode-p 'special-mode)
                                (minibufferp (current-buffer))))
                       (format "s%s:%s"
                               (if base-buffer "ib" "")
                               buffer-name))

                      ;; Indirect buffers
                      (base-buffer
                       (format "ib:%s"
                               buffer-name))

                      ;; Major-modes
                      ((and (boundp 'major-mode) major-mode)
                       (let ((major-mode-symbol (symbol-name major-mode)))
                         (format "mm:%s" major-mode-symbol)))

                      ;; Other
                      (t
                       (format "o:%s" buffer-name))))))

    ;; Return result
    (if (eq result :ignore)
        nil
      result)))

(defun persist-text-scale--get-amount (&optional category)
  "Return the text scale amount for the current buffer category.
CATEGORY is the buffer category.
If the buffer category is nil or no scale amount has been stored, return nil."
  (unless category
    (setq category (persist-text-scale--buffer-category)))
  (when category
    (let ((cat-data (or (cdr (assoc category persist-text-scale--data))
                        ;; TODO store this per category intead
                        ;; (when persist-text-scale-fallback-to-previous-scale
                        ;;   persist-text-scale--last-text-scale-amount)
                        (when (integerp
                               persist-text-scale-default-text-scale-amount)
                          persist-text-scale-default-text-scale-amount))))
      (cond
       ((listp cat-data)
        (cdr (assoc 'text-scale-amount cat-data)))

       ((integerp cat-data)
        cat-data)

       (t
        nil)))))

(defun persist-text-scale--restore-all-windows ()
  "Restore the text scale on all windows."
  (let ((current-window (selected-window))
        (current-buffer (current-buffer)))
    ;; Current window/buffer
    (persist-text-scale-restore)

    ;; Other windows/buffers
    (walk-windows
     (lambda (window)
       (unless (eq window current-window)
         (with-selected-window window
           (when-let* ((buffer (window-buffer)))
             (unless (eq current-buffer buffer)
               (with-current-buffer buffer
                 (persist-text-scale-restore)))))))
     ;; Minibuffer
     t
     ;; All frames
     t)))

(defun persist-text-scale--window-buffer-change-functions (&optional object)
  "Function called by `window-buffer-change-functions'.
OBJECT can be a frame or a window."
  (when (bound-and-true-p persist-text-scale-mode)
    (let* ((is-frame (frame-live-p object))
           (frame (if is-frame
                      object
                    (selected-frame)))
           (window (cond
                    ;; Frame
                    (is-frame
                     (with-selected-frame object
                       (selected-window)))
                    ;; Window
                    ((window-live-p object)
                     object)
                    ;; Current window
                    (t
                     (selected-window)))))

      (when (and frame window)
        (with-selected-frame frame
          (with-selected-window window
            (when-let* ((buffer (window-buffer)))
              (with-current-buffer buffer
                ;; Restore all windows
                (persist-text-scale--restore-all-windows)))))))))

(defun persist-text-scale--text-scale-mode-hook ()
  "Hook function triggered by `text-scale-mode-hook'.
Persists the current text scale and updates all relevant windows, including
indirect buffers or buffers within the same category."
  (persist-text-scale-persist)

  (when persist-text-scale--update-last-text-scale-amount
    (setq persist-text-scale--last-text-scale-amount
          text-scale-mode-amount))

  ;; Remove the function from text-scale-mode-hook to avoid infinite recursion
  (let ((text-scale-mode-hook (delq 'persist-text-scale--text-scale-mode-hook
                                    (copy-sequence text-scale-mode-hook))))
    ;; Ensure other windows are updated (e.g., indirect buffers
    ;; or other buffers of the same category)
    (persist-text-scale--restore-all-windows)))

(defun persist-text-scale--handle-file-renames ()
  "Handle file renames."
  (when persist-text-scale-handle-file-renames
    (when-let* ((filename (buffer-file-name (buffer-base-buffer))))
      (cond
       (persist-text-scale--filename
        (let ((new-filename (file-truename filename)))
          (unless (string= persist-text-scale--filename filename)
            (persist-text-scale--verbose-message
              "Persisting text scale settings due to file rename: %s -> %s"
              persist-text-scale--filename new-filename)
            (setq persist-text-scale--persisted-amount nil)
            (persist-text-scale-persist))))

       (t
        (setq persist-text-scale--filename (file-truename filename)))))))

(defun persist-text-scale--sort ()
  "Sort `persist-text-scale--data' using atime."
  (setq persist-text-scale--data
        (sort persist-text-scale--data
              (lambda (entry1 entry2)
                (setq entry1 (when (consp entry1)
                               (cdr entry1)))
                (setq entry2 (when (consp entry2)
                               (cdr entry2)))
                (let ((atime1 (when (listp entry1)
                                (let ((value (cdr (assoc 'atime entry1))))
                                  (unless value
                                    ;; Backward compatibility
                                    (setq value (cdr (assoc 'mtime entry1)))
                                    (when (consp value)
                                      (setq value (float-time value))))
                                  value)))
                      (atime2 (when (listp entry2)
                                (let ((value (cdr (assoc 'atime entry2))))
                                  (unless value
                                    ;; Backward compatibility
                                    (setq value (cdr (assoc 'mtime entry2)))
                                    (when (consp value)
                                      (setq value (float-time value))))
                                  value))))
                  (cond
                   ;; Compare atime1 and atime2
                   ((and atime1 atime2)
                    (< atime1 atime2))
                   ;; If atime1 is nil, put entry1 after entry2
                   ((not atime1)
                    t)
                   ;; If atime2 is nil, put entry2 after entry1
                   ((not atime2)
                    nil)))))))

(defun persist-text-scale--buffer-name-suffix-number (buffer-name)
  "Extract the number at the end of BUFFER-NAME (e.g., \='name<2>\=').
Return an empty string if no number is found."
  (if (string-match "<\\([0-9]+\\)>$" buffer-name)
      (match-string 1 buffer-name)
    ""))

;;; Functions

(defun persist-text-scale-persist ()
  "Save the current text scale for the current buffer.
If the buffer's identifier already has a stored text scale, it updates the
existing value. Otherwise, it adds a new cons cell (category . scale) to the
alist."
  (when (bound-and-true-p persist-text-scale-mode)
    (cond
     ((not (bound-and-true-p text-scale-mode-amount))
      (persist-text-scale--verbose-message
        "IGNORE (text-scale-mode-disabled): Persist '%s': %s"
        (buffer-name) text-scale-mode-amount))

     ((and (bound-and-true-p persist-text-scale--persisted-amount)
           (= text-scale-mode-amount persist-text-scale--persisted-amount))
      (persist-text-scale--verbose-message
        "IGNORE (up-to-date): Persist '%s': %s"
        (buffer-name) text-scale-mode-amount))

     (t
      (let ((buffer-category (persist-text-scale--buffer-category)))
        (if (not buffer-category)
            ;; No category
            (persist-text-scale--verbose-message
              "IGNORE (:ignore category): Persist '%s': %s: %s"
              (buffer-name) buffer-category text-scale-mode-amount)
          ;; Category found
          (persist-text-scale--verbose-message
            "Persist '%s': %s: %s"
            (buffer-name) buffer-category text-scale-mode-amount)

          (let ((cons-value (when (and persist-text-scale--data
                                       buffer-category)
                              (assoc buffer-category
                                     persist-text-scale--data)))
                (new-data (list (cons 'text-scale-amount text-scale-mode-amount)
                                (cons 'atime (float-time (current-time))))))
            (if cons-value
                (setcdr cons-value new-data)
              (push (cons buffer-category new-data) persist-text-scale--data))

            (setq persist-text-scale--persisted-amount text-scale-mode-amount))))))))

(defun persist-text-scale-restore ()
  "Restore the text scale for the current buffer."
  (let ((persist-text-scale--update-last-text-scale-amount nil))
    ;; Handle renames
    (persist-text-scale--handle-file-renames)

    (when (or (not persist-text-scale-restore-once)
              (not persist-text-scale--restored-amount))
      (when-let* ((buffer-category (persist-text-scale--buffer-category)))
        (when-let* ((amount (persist-text-scale--get-amount buffer-category)))
          (if (and (bound-and-true-p text-scale-mode-amount)
                   (= amount text-scale-mode-amount))
              ;; Ignore
              (persist-text-scale--verbose-message
                (concat "IGNORED (up-to-date): Restore '%s': %s: %s")
                (buffer-name) buffer-category amount)
            ;; Restore
            (persist-text-scale--verbose-message
              "Restore '%s': %s: %s" (buffer-name) buffer-category amount)
            (text-scale-set amount)
            (setq persist-text-scale--restored-amount amount)

            ;; Update atime after restore
            (let ((cons-value (when persist-text-scale--data
                                (assoc buffer-category
                                       persist-text-scale--data)))
                  (new-data (list (cons 'text-scale-amount amount)
                                  (cons 'atime (float-time (current-time))))))
              (when cons-value
                (setcdr cons-value new-data)))))))))

(defun persist-text-scale-reset ()
  "Reset the text scale for all buffer categories."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when persist-text-scale--restored-amount
          (setq persist-text-scale--persisted-amount nil)
          (setq persist-text-scale--restored-amount nil)))))

  (setq persist-text-scale--data nil))

(defun persist-text-scale-save-file ()
  "Save the current text scale data to `persist-text-scale-file'.

This function writes the text scale data to the file specified by
`persist-text-scale-file', preserving the state for future sessions."
  (persist-text-scale-cleanup)
  (with-temp-buffer
    (insert
     ";; -*- mode: emacs-lisp; lexical-binding: t; coding: utf-8-unix -*-\n")
    (insert ";; Persist Text Scale file, automatically generated "
            "by ‘persist-text-scale’.\n")

    (insert "(setq persist-text-scale--data ")
    (when persist-text-scale--data
      (insert "'"))
    (prin1 persist-text-scale--data (current-buffer))
    (insert ")\n\n")

    (insert "(setq persist-text-scale--last-text-scale-amount ")
    (prin1 persist-text-scale--last-text-scale-amount (current-buffer))
    (insert ")\n\n")

    (let ((coding-system-for-write 'utf-8-emacs)
          (write-region-annotate-functions nil)
          (write-region-post-annotation-function nil))
      (write-region
       (point-min) (point-max) persist-text-scale-file nil 'silent))))

(defun persist-text-scale-load-file ()
  "Load data from `persist-text-scale-file'."
  (load persist-text-scale-file t t t))

(defun persist-text-scale-cleanup ()
  "Delete old entries."
  (when persist-text-scale-history-length
    (persist-text-scale--sort)
    (setq persist-text-scale--data
          (cl-subseq persist-text-scale--data
                     (max 0 (- (length persist-text-scale--data)
                               persist-text-scale-history-length))))))

;;; Mode

;;;###autoload
(define-minor-mode persist-text-scale-mode
  "Toggle `persist-text-scale-mode'."
  :global t
  :lighter " PTScale"
  :group 'persist-text-scale
  (if persist-text-scale-mode
      (progn
        (persist-text-scale-load-file)
        (persist-text-scale--manage-timer)
        (add-hook 'kill-emacs-hook #'persist-text-scale-save-file)

        (add-hook 'minibuffer-setup-hook #'persist-text-scale-restore)

        (add-hook 'window-buffer-change-functions
                  #'persist-text-scale--window-buffer-change-functions
                  persist-text-scale-depth-window-buffer-change-functions)

        ;; Hook: when text scale is changed
        (add-hook 'text-scale-mode-hook
                  #'persist-text-scale--text-scale-mode-hook
                  persist-text-scale-depth-text-scale-mode))
    (persist-text-scale--cancel-timer)
    (remove-hook 'kill-emacs-hook #'persist-text-scale-save-file)

    (remove-hook 'window-buffer-change-functions
                 #'persist-text-scale--window-buffer-change-functions)

    (remove-hook 'text-scale-mode-hook #'persist-text-scale--text-scale-mode-hook)

    (persist-text-scale-reset)))

(provide 'persist-text-scale)
;;; persist-text-scale.el ends here
