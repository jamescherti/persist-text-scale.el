;;; persist-text-scale.el --- Persist and restore text scale -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
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
;; The persist-text-scale Emacs package provides persist-text-scale-mode,
;; which ensures that all adjustments made with text-scale-increase and
;; text-scale-decrease are persisted and restored across sessions. As a
;; result, the text size in each buffer remains consistent, even after
;; restarting Emacs.
;;
;; (By default, persist-text-scale-mode saves the text scale individually for
;; each file-visiting buffer and applies a custom text scale for each special
;; buffer. This behavior can be further customized by assigning a function to
;; the persist-text-scale-buffer-category-function variable. The function
;; determines how buffers are categorized by returning a category identifier
;; based on the buffer's context. Buffers within the same category will share
;; the same text scale.)

;;; Code:

;;; Require

(require 'face-remap)

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

(defcustom persist-text-scale-autosave-interval (* 7 60)
  "Time interval, in seconds, between automatic saves of text scale data.
If set to an integer value, enables periodic autosaving of persisted text scale
information at the specified interval.
If set to nil, disables timer-based autosaving entirely."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'persist-text-scale)

(defcustom persist-text-scale-buffer-category-function nil
  "Optional function to customize buffer category classification.
If non-nil, this function overrides the `persist-text-scale--buffer-category'
function and is invoked to determine the buffer category identifier used for
text scale grouping. It must return a string or symbol representing the buffer
category, or nil to fall back to the default classification."
  :type '(choice (const :tag "None" nil) function)
  :group 'persist-text-scale)

(defcustom persist-text-scale-verbose nil
  "If non-nil, display informative messages during text scale restoration.
These messages will indicate when and how the text scale was restored, aiding
in debugging or monitoring behavior."
  :type 'boolean
  :group 'persist-text-scale)

(defcustom persist-text-scale-restore-once t
  "If non-nil, restore text scale only once per buffer.
When non-nil, the text scale will be restored either when the buffer is loaded
or when the buffer is displayed in a window for the first time. Subsequent
window changes will not trigger additional restoration.
When nil, text-scale will always be restored. This is useful for debugging."
  :type 'boolean
  :group 'persist-text-scale)

;;; Variables

(defvar persist-text-scale-depth-window-buffer-change-functions -99)
(defvar persist-text-scale-depth-find-file-hook -99)
(defvar persist-text-scale-depth-text-scale-mode 99)

(defvar persist-text-scale--data nil
  "Alist mapping buffer identifiers to their corresponding text scale amount.
Each entry associates either a file path or a buffer category name with the
numeric value of the text scale applied to that buffer or group.")

(defvar persist-text-scale--last-text-scale-amount nil
  "Most recent text scale amount selected by the user.
This value reflects the numeric text scale adjustment applied in the last
interactive text scale change and is used internally to support restoration.")

;; Internal variables

(defvar persist-text-scale--timer nil)
(defvar-local persist-text-scale--amount nil)

;;; Defun

(defun persist-text-scale--cancel-timer ()
  "Cancel `persist-text-scale-autosave' timer, if set."
  (when (timerp persist-text-scale--timer)
    (cancel-timer persist-text-scale--timer))
  (setq persist-text-scale--timer nil))

(defun persist-text-scale--manage-timer ()
  "Set or cancel an invocation of `persist-text-scale-autosave' on a timer.
If `persist-text-scale-mode' is enabled, set the timer, otherwise cancel the
timer."
  (when (boundp 'persist-text-scale-autosave-interval)
    (if (and (bound-and-true-p persist-text-scale-mode)
             persist-text-scale-autosave-interval
             (null persist-text-scale--timer))
        (setq persist-text-scale--timer
              (run-with-timer persist-text-scale-autosave-interval
                              persist-text-scale-autosave-interval
                              #'persist-text-scale-save))
      (persist-text-scale--cancel-timer))))

(defun persist-text-scale--buffer-category ()
  "Generate a unique name for the current buffer.
Returns a unique identifier string based."
  (let ((result nil))
    (when persist-text-scale-buffer-category-function
      (setq result (funcall persist-text-scale-buffer-category-function)))

    (when (or (not result)
              (not (eq result :ignore)))
      (let ((file-name (buffer-file-name (buffer-base-buffer)))
            (buffer-name (buffer-name)))
        (cond
         ;; Special buffers
         ((and (not file-name)
               (or (string-prefix-p "*" buffer-name)
                   (string-prefix-p " " buffer-name)
                   (derived-mode-p 'special-mode)))
          (setq result (concat "special:" buffer-name)))

         (file-name
          (setq result (format "file:%s" (file-truename file-name))))

         ((boundp 'major-mode)
          (let ((major-mode-symbol (symbol-name major-mode)))
            (setq result (concat "major-mode:" major-mode-symbol))))

         (t
          (setq result "unknown")))))
    ;; Return result
    (if (eq result :ignore)
        nil
      result)))

(defun persist-text-scale-get-amount ()
  "Retrieve the text scale factor for the current buffer category.
Returns nil when the buffer category is nil."
  (let ((mode (persist-text-scale--buffer-category)))
    (when (and mode persist-text-scale--data)
      (let (scale
            (mode-data (or (cdr (assoc mode persist-text-scale--data))
                           persist-text-scale--last-text-scale-amount)))
        (cond
         ((integerp mode-data)
          (setq scale mode-data))

         ((listp mode-data)
          (setq scale (cdr (assoc 'text-scale-amount mode-data)))))

        (when scale
          (let ((amount scale))
            amount))))))

(defun persist-text-scale-persist (&rest _)
  "Save the current text scale for the current buffer.
If the buffer's identifier already has a stored text scale, it updates the
existing value. Otherwise, it adds a new cons cell (mode . scale) to the
alist."
  (when (bound-and-true-p persist-text-scale-mode)
    (cond
     ((not (bound-and-true-p text-scale-mode-amount))
      (when persist-text-scale-verbose
        (message
         "[persist-text-scale] IGNORE (text-scale-mode-disabled): Persist '%s': %s"
         (buffer-name) text-scale-mode-amount)))

     ((and (bound-and-true-p persist-text-scale--amount)
           (= text-scale-mode-amount persist-text-scale--amount))
      (when persist-text-scale-verbose
        (message "[persist-text-scale] IGNORE (up-to-date): Persist '%s': %s"
                 (buffer-name) text-scale-mode-amount)))

     (t
      (let ((buffer-category (persist-text-scale--buffer-category)))
        (when persist-text-scale-verbose
          (message "[persist-text-scale] Persist '%s': %s: %s"
                   (buffer-name) buffer-category text-scale-mode-amount))
        (let ((cons-value (when (and persist-text-scale--data
                                     buffer-category)
                            (assoc buffer-category
                                   persist-text-scale--data)))
              (new-data (list (cons 'text-scale-amount text-scale-mode-amount)
                              (cons 'mtime (current-time)))))
          (if cons-value
              (setcdr cons-value new-data)
            (push (cons buffer-category new-data) persist-text-scale--data))

          (setq persist-text-scale--amount text-scale-mode-amount)

          ;; TODO: Move to a separate function
          (setq persist-text-scale--last-text-scale-amount
                text-scale-mode-amount)))))))

(defun persist-text-scale-restore (&rest _)
  "Restore the text scale for the current buffer."
  (when (or (not persist-text-scale-restore-once)
            (not persist-text-scale--amount))
    (when-let* ((amount (persist-text-scale-get-amount)))
      (setq persist-text-scale--amount amount)
      (if (and (bound-and-true-p text-scale-mode-amount)
               (= amount text-scale-mode-amount))
          ;; Ignore
          (when persist-text-scale-verbose
            (message (concat "[persist-text-scale] IGNORED "
                             "(up-to-date): Restore '%s': %s: %s")
                     (buffer-name)
                     (persist-text-scale--buffer-category)
                     amount))
        ;; Restore
        (when persist-text-scale-verbose
          (message "[persist-text-scale] Restore '%s': %s: %s"
                   (buffer-name)
                   (persist-text-scale--buffer-category)
                   amount))
        (text-scale-set amount)))))

(defun persist-text-scale--restore-all-windows ()
  "Restore the text scale on all windows in the current frame."
  (walk-windows
   (lambda (window)
     (let ((buffer (window-buffer window)))
       (with-current-buffer buffer
         (persist-text-scale-restore))))

   ;; Exclude the mini buffer
   ;; TODO: Include?
   nil

   ;; nil = current frame only | t = all frames
   nil))

(defun persist-text-scale--window-buffer-change-functions (&optional object)
  "Function called by `window-buffer-change-functions'.
OBJECT can be a frame or a window."
  (when (bound-and-true-p persist-text-scale-mode)
    (let (;; A frame is passed as an argument when functions specified by the
          ;; default value are called for each frame, but only if at least one
          ;; window on that frame has been added, deleted, or had its buffer
          ;; changed since the last redisplay.
          ;; See `window-buffer-change-functions'
          (frame (if (frame-live-p object)
                     object
                   (selected-frame)))
          ;; A window is passed as an argument when functions specified
          ;; buffer-locally are called for each window displaying the
          ;; corresponding buffer. This occurs only if the window has been added
          ;; or its buffer has changed since the last redisplay.
          ;; See `window-buffer-change-functions'
          (window (cond
                   ((frame-live-p object)
                    (with-selected-frame object
                      (selected-window)))

                   ((window-live-p object)
                    object)

                   (t
                    (selected-window)))))

      (with-selected-frame frame
        (with-selected-window window
          ;; Iterate over all windows to ensure none are missed (e.g., consult
          ;; preview windows that don't trigger
          ;; `window-buffer-change-functions` or other hooks used by
          ;; `persist-text-scale`).
          (persist-text-scale--restore-all-windows))))))

(defun persist-text-scale-reset ()
  "Reset the text scale for all buffer categories."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when persist-text-scale--amount
          (setq persist-text-scale--amount nil)))))

  (setq persist-text-scale--data nil))

(defun persist-text-scale-save ()
  "Save the current text scale data to `persist-text-scale-file'.

This function writes the text scale data to the file specified by
`persist-text-scale-file', preserving the state for future sessions."
  (with-temp-buffer
    (insert ";; -*- mode: emacs-lisp; coding: utf-8-unix -*-\n")
    (insert (concat ";; Persist Text Scale file, automatically generated "
                    "by ‘persist-text-scale’.\n"))

    (insert "(setq persist-text-scale--data ")
    (unless (eq persist-text-scale--data nil)
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

(defun persist-text-scale-load ()
  "Load data from `persist-text-scale-file'."
  (load persist-text-scale-file t t t))

;;; Mode

;;;###autoload
(define-minor-mode persist-text-scale-mode
  "Toggle `persist-text-scale-mode'."
  :global t
  :lighter " PTScale"
  :group 'persist-text-scale
  (if persist-text-scale-mode
      (progn
        (persist-text-scale-load)
        (persist-text-scale--manage-timer)
        (add-hook 'kill-emacs-hook #'persist-text-scale-save)
        (add-hook 'window-buffer-change-functions
                  #'persist-text-scale--window-buffer-change-functions
                  persist-text-scale-depth-window-buffer-change-functions)
        (add-hook 'find-file-hook #'persist-text-scale-restore
                  persist-text-scale-depth-find-file-hook)
        ;; Hook: when text scale is changed
        (add-hook 'text-scale-mode-hook #'persist-text-scale-persist
                  persist-text-scale-depth-text-scale-mode))
    (persist-text-scale--cancel-timer)
    (remove-hook 'kill-emacs-hook #'persist-text-scale-save)
    (remove-hook 'window-buffer-change-functions #'persist-text-scale-restore)
    (remove-hook 'text-scale-mode-hook #'persist-text-scale-persist)
    (remove-hook 'find-file-hook #'persist-text-scale-restore)
    (persist-text-scale-reset)))

(provide 'persist-text-scale)
;;; persist-text-scale.el ends here
