;;; symbols-outline-follow-mode.el --- Symbols outline follow mode  -*- lexical-binding: t; -*-

;; Author: Shihao Liu
;; Keywords: outlines
;; Version: 1.0.0
;; URL: https://github.com/liushihao456/symbols-outline.el

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
;; When enabled, the cursor in the original buffer will follow the movement in
;; the outline buffer.

;;; Code:

(require 'symbols-outline)

(defvar symbols-outline-follow-mode-delay 0.01)

(defvar symbols-outline-follow-mode--timer nil
  "The idle timer object for `symbols-outline-follow-mode'.
Active while follow mode is enabled and nil/cancelled otherwise.")

(defun symbols-outline-follow-mode--setup ()
  "Setup symbols outline follow mode."
  (setq symbols-outline-follow-mode--timer
        (run-with-idle-timer symbols-outline-follow-mode-delay t
                             #'symbols-outline--follow))
  (add-hook 'after-save-hook #'symbols-outline-refresh)
  (add-hook 'window-selection-change-functions #'symbols-outline--follow))

(defun symbols-outline-follow-mode--tear-down ()
  "Tear down symbols outline follow mode."
  (when symbols-outline-follow-mode--timer
    (cancel-timer symbols-outline-follow-mode--timer))
  (remove-hook 'after-save-hook #'symbols-outline-refresh)
  (remove-hook 'window-selection-change-functions #'symbols-outline--follow))

;;;###autoload
(define-minor-mode symbols-outline-follow-mode
  "Follow the symbol at point in symbols-outline window."
  :global t
  :group 'symbols-outline
  (if symbols-outline-follow-mode
      (symbols-outline-follow-mode--setup)
    (symbols-outline-follow-mode--tear-down)))

(provide 'symbols-outline-follow-mode)

;;; symbols-outline-follow-mode.el ends here
