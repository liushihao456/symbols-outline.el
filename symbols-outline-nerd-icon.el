;;; symbols-outline-nerd-icon.el --- Nerd icons for symbols outline  -*- lexical-binding: t; -*-

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

(defvar symbols-outline-nerd-icon-alist
  '(
    ;; C, C++, java, python
    ("file"          . " ")
    ("function"      . " ")
    ("method"        . " ")
    ("prototype"     . " ")
    ("annotation"    . " ")
    ("constructor"   . " ")
    ("class"         . " ")
    ("struct"        . " ")
    ("interface"     . " ")
    ("union"         . " ")
    ("enum"          . " ")
    ("enumerator"    . " ")
    ("enummember"    . " ")
    ("using"         . " ")
    ("namespace"     . " ")
    ("variable"      . " ")
    ("member"        . " ")
    ("field"         . " ")
    ("externvar"     . " ")
    ("local"         . " ")
    ("macro"         . " ")
    ("string"        . " ")
    ("boolean"       . " ")
    ("array"         . " ")
    ("number"        . " ")
    ("object"        . " ")
    ("misc"          . " ")
    ("operator"      . " ")
    ("parameter"     . " ")
    ("macroparam"    . " ")
    ("typeparameter" . " ")
    ("tparam"        . " ")
    ("event"         . " ")
    ("typedef"       . " ")
    ("package"       . " ")
    ("module"        . " ")
    ("key"           . " ")
    ("null"          . " ")

    ;; Elisp
    ("derivedMode"   . " ")
    ("majorMode"     . " ")
    ("minorMode"     . " ")
    ("inline"        . " ")
    ("subst"         . " ")
    ("group"         . " ")
    ("error"         . " ")
    ("custom"        . " ")
    ("face"          . " ")
    ("const"         . " ")
    ("alias"         . " ")
    ("unknown"       . " ")

    ;; JavaScript, TypeScript
    ("constant"      . " ")
    ("property"      . " ")

    ;; Markdown
    ("chapter"       . " ")
    ("section"       . "ﮝ ")
    ("subsection"    . "ﮝ ")

    ;; Org
    ("part"          . " ")

    ;; Chevrons
    ("chevron-down"  . " ")
    ("chevron-right" . " ")
    ))

(defun symbols-outline-nerd-icon-get (icon-name)
  "Return the nerd font icon for ICON-NAME."
  (cdr (assoc icon-name symbols-outline-nerd-icon-alist)))

(provide 'symbols-outline-nerd-icon)

;;; symbols-outline-nerd-icon.el ends here
