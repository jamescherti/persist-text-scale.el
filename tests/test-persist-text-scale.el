;;; test-persist-text-scale.el --- Test persist-text-scale -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.3
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
;; Test persist-text-scale.

;;; Code:

(require 'ert)
(require 'persist-text-scale)

;; (with-no-warnings
;;   (when (require 'undercover nil t)
;;     (undercover "persist-text-scale"
;;                 (:report-file ".coverage")
;;                 (:report-format 'text)
;;                 (:send-report nil))))

(ert-deftest test-persist-text-scale ()
  "Test persist-text-scale."
  (interactive)
  (should t))

(provide 'test-persist-text-scale)
;;; test-persist-text-scale.el ends here
