;;; symbols-outline-follow-mode.el --- Tree like view for symbols  -*- lexical-binding: t; -*-

;; Author: Shihao Liu
;; Keywords: outline symbols
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; --------------------------------------

;;; Usage:
;;
;; --------------------------------------

;;; Code:

(require 'symbols-outline)

(defvar symbols-outline-follow-delay 0.01)

(defvar symbols-outline--follow-timer nil
  "The idle timer object for `symbols-outline-follow-mode'.
Active while follow mode is enabled and nil/cancelled otherwise.")

(defun symbols-outline--setup-follow-mode ()
  "Setup symbols outline follow mode."
  (setq symbols-outline--follow-timer
        (run-with-idle-timer symbols-outline-follow-delay t
                             #'symbols-outline--follow))
  ;; (add-hook 'buffer-list-update-hook #'symbols-outline--follow)
  (add-hook 'after-save-hook #'symbols-outline-refresh)
  (add-hook 'window-selection-change-functions #'symbols-outline--follow))

(defun symbols-outline-tear-down-follow-mode ()
  "Tear down symbols outline follow mode."
  (when symbols-outline--follow-timer
    (cancel-timer symbols-outline--follow-timer))
  ;; (remove-hook 'buffer-list-update-hook #'symbols-outline--follow)
  (remove-hook 'after-save-hook #'symbols-outline-refresh)
  (remove-hook 'window-selection-change-functions #'symbols-outline--follow))

;;;###autoload
(define-minor-mode symbols-outline-follow-mode
  "Follow the symbol at point in symbols-outline window."
  :global t
  :group 'symbols-outline
  (if symbols-outline-follow-mode
      (symbols-outline--setup-follow-mode)
    (symbols-outline-tear-down-follow-mode)))

(provide 'symbols-outline-follow-mode)

;;; symbols-outline-follow-mode.el ends here
