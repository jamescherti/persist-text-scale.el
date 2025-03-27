;;; persist-text-scale.el --- Persist and restore text scale -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/persist-text-scale.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24.1"))
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

(defgroup persist-text-scale nil
  "Persist and restore text scale"
  :group 'persist-text-scale
  :prefix "persist-text-scale-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/persist-text-scale.el"))

(defcustom persist-text-scale-verbose nil
  "Enable displaying verbose messages."
  :type 'boolean
  :group 'persist-text-scale)

(defun persist-text-scale--message (&rest args)
  "Display a message with the same ARGS arguments as `message'."
  (apply #'message (concat "[persist-text-scale] " (car args)) (cdr args)))

(defmacro persist-text-scale--verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  `(progn
     (when persist-text-scale-verbose
       (persist-text-scale--message
        (concat ,(car args)) ,@(cdr args)))))

;;;###autoload
(define-minor-mode persist-text-scale-mode
  "Toggle `persist-text-scale-mode'."
  :global t
  :lighter " persist-text-scale"
  :group 'persist-text-scale
  (if persist-text-scale-mode
      t
    t))

(provide 'persist-text-scale)
;;; persist-text-scale.el ends here
