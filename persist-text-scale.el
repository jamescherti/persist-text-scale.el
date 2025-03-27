;;; persist-text-scale.el --- Persist and restore text scale -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/persist-text-scale.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24.3"))
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
;; Persist and restore text scale.

;;; Code:

;;; Require

(require 'face-remap)

;;; Defcustom

(defgroup persist-text-scale nil
  "Non-nil if persist-text-scale mode mode is enabled."
  :group 'persist-text-scale
  :prefix "persist-text-scale-")

(defcustom persist-text-scale-file (expand-file-name "persist-text-scale"
                                                     user-emacs-directory)
  "File where the persist-text-scale data is stored.
This file holds the data for persisting the text scale across sessions.
It can be customized to a different file path as needed."
  :type 'file
  :group 'persist-text-scale)

(defcustom persist-text-scale-restore-once t
  "If non-nil, restore text scale only once per buffer.
When non-nil, the text scale will be restored either when the buffer is loaded
or when the buffer is displayed in a window for the first time. Subsequent
window changes will not trigger additional restoration.

When nil, text-scale will always be restored."
  :type 'boolean
  :group 'persist-text-scale)

(defcustom persist-text-scale-verbose nil
  "If non-nil, display informative messages during text scale restoration.
These messages will indicate when and how the text scale was restored, aiding
in debugging or monitoring behavior."
  :type 'boolean
  :group 'persist-text-scale)

(defcustom persist-text-scale-buffer-type-function nil
  "Optional function to customize buffer type classification.
If non-nil, this function overrides `persist-text-scale--buffer-type' and is
invoked to determine the buffer type identifier used for text scale grouping.
It must return a string or symbol representing the buffer type, or nil to fall
back to the default classification."
  :type '(choice (const :tag "None" nil) function)
  :group 'persist-text-scale)

(defcustom persist-text-scale-autosave-interval (* 7 60)
  "Time interval, in seconds, between automatic saves of text scale data.
If set to an integer value, enables periodic autosaving of persisted text scale
information at the specified interval.
If set to nil, disables timer-based autosaving entirely."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'persist-text-scale)

;;; Variables

(defvar persist-text-scale--data nil
  "Alist mapping buffer identifiers to their corresponding text scale amount.
Each entry associates either a file path or a buffer type name with the numeric
value of the text scale applied to that buffer or group.")

(defvar persist-text-scale--previous-text-scale-amount 0
  "Most recent text scale amount selected by the user.
This value reflects the numeric text scale adjustment applied in the last
interactive text scale change and is used internally to support restoration.")

(defvar persist-text-scale--timer nil)

(defvar-local persist-text-scale--restored-p nil)
(defvar-local persist-text-scale--restored-amount nil)

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

(defun persist-text-scale--buffer-type ()
  "Generate a unique name for the current buffer.
Returns a unique identifier string based."
  (let ((result nil))
    (when persist-text-scale-buffer-type-function
      (setq result (funcall persist-text-scale-buffer-type-function)))

    (when (or (not result)
              (not (eq result :ignore)))
      (let ((file-name (buffer-file-name (buffer-base-buffer)))
            (buffer-name (buffer-name)))
        (cond
         ;; Corfu adjusts the text size based on the size of the window from
         ;; which the text completion is triggered.
         ((string-prefix-p " *corfu" buffer-name)
          :ignore)

         ((string-prefix-p "*pathaction:" buffer-name)
          "type-pathaction")

         ((string-equal buffer-name "*scratch*")
          "scratch")

         (file-name
          (setq result (format "file:%s" (file-truename file-name)))
          ;; (setq result "file")
          )

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
  "Retrieve the text scale factor for the current buffer type.
Returns nil when the buffer type is nil."
  (let ((mode (persist-text-scale--buffer-type)))
    (when (and mode persist-text-scale--data)
      (let ((scale (or (cdr (assoc mode persist-text-scale--data))
                       persist-text-scale--previous-text-scale-amount)))
        (when scale
          (let ((amount scale))
            amount))))))

(defun persist-text-scale-persist (&rest _)
  "Save the current text scale for the current buffer.
If the buffer's identifier already has a stored text scale, it updates the
existing value. Otherwise, it adds a new cons cell (mode . scale) to the
alist."
  (when (bound-and-true-p persist-text-scale-mode)
    (let ((buffer-type (persist-text-scale--buffer-type)))
      (when persist-text-scale-verbose
        (message "[persist-text-scale] Persist %s: %s: %s"
                 (buffer-name) buffer-type text-scale-mode-amount))
      (let ((cons-value (and persist-text-scale--data
                             (when buffer-type
                               (assoc buffer-type persist-text-scale--data)))))
        (if cons-value
            (setcdr cons-value text-scale-mode-amount)
          (push (cons buffer-type text-scale-mode-amount)
                persist-text-scale--data))

        (setq persist-text-scale--previous-text-scale-amount text-scale-mode-amount)))))

(defun persist-text-scale-restore (&optional object &rest _)
  "Restore the text scale for the current buffer .
OBJECT is a window or a frame.
If a text scale value is found, it sets the text scale using `text-scale-set'."
  (when (bound-and-true-p persist-text-scale-mode)
    (let ((frame (if (frame-live-p object)
                     object
                   (selected-frame)))
          (window (cond ((not object)
                         nil)

                        ((frame-live-p object)
                         (with-selected-frame object
                           (selected-window)))

                        ((window-live-p object)
                         object)

                        (t
                         (selected-window)))))

      (when (and window (window-live-p window))
        (with-selected-frame frame
          ;; Iterate over all windows to ensure none are missed (e.g., consult
          ;; preview windows that don't trigger `window-buffer-change-functions`
          ;; or other hooks used by `persist-text-scale`).
          (walk-windows
           (lambda (window)
             (with-selected-window window
               (let ((buffer (window-buffer)))
                 (let ((amount (persist-text-scale-get-amount)))
                   (when (and amount
                              (or (not persist-text-scale-restore-once)
                                  (and persist-text-scale-restore-once
                                       (not persist-text-scale--restored-p))))
                     (when persist-text-scale-verbose
                       (message "[persist-text-scale] Restore %s: %s: %s"
                                (buffer-name buffer)
                                (persist-text-scale--buffer-type)
                                amount))
                     (setq persist-text-scale--restored-p t)
                     (setq persist-text-scale--restored-amount amount)
                     (text-scale-set amount))))))
           ;; Do not exclude the mini buffer
           nil
           ;; nil = current frame only | t = all frames
           nil))))))

(defun persist-text-scale-reset ()
  "Reset the text scale for all buffer categories."
  (setq persist-text-scale--data nil))

(defun persist-text-scale-save ()
  "Save data to `persist-text-scale-file'."
  (with-temp-buffer
    (insert ";; -*- mode: emacs-lisp; coding: utf-8-unix -*-\n")
    (insert (concat ";; Persist Text Scale file, automatically generated "
                    "by ‘persist-text-scale’.\n"))

    (insert "(setq persist-text-scale--data '")
    (prin1 persist-text-scale--data (current-buffer))
    (insert ")\n\n")

    (let ((coding-system-for-write 'utf-8-emacs)
          (write-region-annotate-functions nil)
          (write-region-post-annotation-function nil))
      (write-region
       (point-min) (point-max) persist-text-scale-file nil 'silent)
      (indent-region (point-min) (point-max)))))

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
        (add-hook 'window-buffer-change-functions #'persist-text-scale-restore 99)
        (add-hook 'find-file-hook #'persist-text-scale-restore 99)
        (add-hook 'text-scale-mode-hook #'persist-text-scale-persist 99))
    (persist-text-scale--cancel-timer)
    (remove-hook 'kill-emacs-hook #'persist-text-scale-save)
    (remove-hook 'window-buffer-change-functions #'persist-text-scale-restore)
    (remove-hook 'text-scale-mode-hook #'persist-text-scale-persist)
    (remove-hook 'find-file-hook #'persist-text-scale-restore)))

(provide 'persist-text-scale)
;;; persist-text-scale.el ends here
