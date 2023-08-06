;;; symbols-outline-lsp.el --- Lsp backend for symbols-outline  -*- lexical-binding: t; -*-

;; Author: Shihao Liu
;; Keywords: outlines
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
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
;; --------------------------------------

;;; Usage:
;;
;; --------------------------------------

;;; Code:

(require 'symbols-outline-node)
(require 'lsp-mode)

(defun symbols-outline-lsp--kind-name (kind-no)
  (pcase kind-no
	(1 "file")
	(2 "module")
	(3 "namespace")
	(4 "package")
	(5 "class")
	(6 "method")
	(7 "property")
	(8 "field")
	(9 "constructor")
	(10 "enum")
	(11 "interface")
	(12 "function")
	(13 "variable")
	(14 "constant")
	(15 "string")
	(16 "number")
	(17 "boolean")
	(18 "array")
	(19 "object")
	(20 "key")
	(21 "null")
	(22 "enummember")
	(23 "struct")
	(24 "event")
	(25 "operator")
	(26 "typeparameter")
    (_ "misc")))

(defun symbols-outline-lsp--convert-internal (ht-symbols tree)
  (mapc (lambda (symbol)
          (let ((node (make-symbols-outline-node
                       :name (gethash "name" symbol)
                       :kind (symbols-outline-lsp--kind-name
                              (gethash "kind" symbol))
                       :line (thread-last symbol
                                          (gethash "range")
                                          (gethash "start")
                                          (gethash "line")
                                          (1+))
                       :signature (gethash "detail" symbol)
                       :parent tree))
                (ht-children (gethash "children" symbol)))
            (push node (symbols-outline-node-children tree))
            (when (length> ht-children 0)
              (symbols-outline-lsp--convert-internal ht-children node))))
        ht-symbols)
  (setf (symbols-outline-node-children tree)
        (nreverse (symbols-outline-node-children tree))))

(defun symbols-outline-lsp--convert (ht-symbols)
  (let ((root (make-symbols-outline-node)))
    (symbols-outline-lsp--convert-internal ht-symbols root)
    (symbols-outline-node--sort-children root)
    root))

;;;###autoload
(defun symbols-outline-lsp-fetch (refresh-fn)
  (when (lsp--find-workspaces-for "textDocument/documentSymbol")
    (lsp-request-async "textDocument/documentSymbol"
                       (lsp-make-document-symbol-params
                        :text-document (lsp--text-document-identifier))
                       (lambda (document-symbols)
                         (thread-last document-symbols
                                      (symbols-outline-lsp--convert)
                                      (funcall refresh-fn)))
                       :mode 'alive)))

(provide 'symbols-outline-lsp)

;;; symbols-outline-lsp.el ends here
