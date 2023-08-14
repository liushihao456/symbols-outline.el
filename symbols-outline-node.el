;;; symbols-outline-node.el --- Symbols tree structure node  -*- lexical-binding: t; -*-

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
;; Tree data structure for document symbols.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(cl-defstruct symbols-outline-node
  name
  kind
  line
  signature
  parent
  children
  collapsed
  ov)

(defun symbols-outline-node-size (node)
  "Return the total number of nodes of tree NODE."
  (if-let (children (symbols-outline-node-children node))
      (thread-last children
                   (mapcar #'symbols-outline-node-size)
                   (apply #'+)
                   (1+))
    1))

(defun symbols-outline-node-size-parents (node)
  "Return the total number of nodes of tree NODE that have children."
  (if-let (children (symbols-outline-node-children node))
      (thread-last children
                   (mapcar #'symbols-outline-node-size-parents)
                   (apply #'+)
                   (1+))
    0))

(defun symbols-outline-node-depth-non-collpased (node)
  "Return the depth of NODE ignoring collapsed children."
  (if-let ((children (symbols-outline-node-children node))
           ((not (symbols-outline-node-collapsed node))))
      (thread-last children
                   (mapcar #'symbols-outline-node-depth-non-collpased)
                   (seq-max)
                   (1+))
    1))

(defun symbols-outline-node-foreach (node fn)
  "Iterate over all the subnodes of NODE with function FN.

FN is a function that takes one argument: node."
  (funcall fn node)
  (when-let (children (symbols-outline-node-children node))
    (dolist (c children)
      (symbols-outline-node-foreach c fn))))

(defun symbols-outline-node-foreach-non-collapsed (node fn)
  "Iterate over all the subnodes of NODE with function FN.

FN is a function that takes one argument: node."
  (funcall fn node)
  (when-let ((children (symbols-outline-node-children node))
             ((not (symbols-outline-node-collapsed node))))
    (dolist (c children)
      (symbols-outline-node-foreach-non-collapsed c fn))))

(defun symbols-outline-node-foreach-at-depth (node depth fn &optional depth0)
  "Iterate over NODE's children at DEPTH with function FN.

FN is a function that takes one argument: node.

The fourth argument DEPTH0 is for internal recursive use only."
  (let ((depth1 (or depth0 0)))
    (when (= depth1 depth)
      (funcall fn node))
    (when-let (((< depth1 depth))
               (children (symbols-outline-node-children node)))
      (dolist (c children)
        (symbols-outline-node-foreach-at-depth c depth fn (1+ depth1))))))

(defun symbols-outline-node-find (node pred)
  "Iterate over NODE and NODE's children to find the node where PRED returns t.
If none were found, return nil.

PRED is a function that takes one argument: node."
  (if (funcall pred node)
      node
    (when-let (children (symbols-outline-node-children node))
      (cl-loop for c in children
               for res = (symbols-outline-node-find c pred)
               if res return res))))

(defun symbols-outline-node-find-symbol-at-line (node ln)
  "Find the symbol in NODE that corresponds to original buffer's line LN."
  (let ((line 0) res)
    (symbols-outline-node-foreach-non-collapsed
     node
     (lambda (node1)
       (when-let* ((line1 (symbols-outline-node-line node1))
                   ((and (<= line1 ln)
                         (> line1 line))))
         (setq line line1)
         (setq res node1))))
    res))

(defun symbols-outline-node--prune-pseudo-nodes (node)
  "Prune pseudo nodes from the tree NODE.

If NODE is a pseudo node, i.e., it has no entry property, move
its children to its parent, and delete this node."
  (when-let* (((not (symbols-outline-node-line node)))
              (parent (symbols-outline-node-parent node))
              (parent-children (symbols-outline-node-children parent))
              (children (symbols-outline-node-children node)))
    (mapc (lambda (c)
            (setf (symbols-outline-node-name c)
                  (format "(%s) %s"
                          (symbols-outline-node-name node)
                          (symbols-outline-node-name c))))
          children)
    (setf (symbols-outline-node-children parent)
          (append (delq node parent-children) children)))
  (when-let (children (symbols-outline-node-children node))
    (mapc #'symbols-outline-node--prune-pseudo-nodes children)))

(defun symbols-outline-node--sort-children (node)
  "Sort NODE's children based on their line numbers."
  (setf (symbols-outline-node-children node)
        (sort (nreverse (symbols-outline-node-children node))
              (lambda (a b) (if-let ((line-a (symbols-outline-node-line a))
                                     (line-b (symbols-outline-node-line b)))
                                (< line-a line-b)
                              t))))
  (when-let (children (symbols-outline-node-children node))
    (mapc #'symbols-outline-node--sort-children children)))

(defun symbols-outline-node--copy-collapse-state (from to)
  "Copy the collapse state from FROM to TO."
  (let ((collapsed-table (make-hash-table :size (symbols-outline-node-size-parents
                                                 from)
                                          :test 'equal)))
    (symbols-outline-node-foreach
     from
     (lambda (node)
       (when-let ((name (symbols-outline-node-name node))
                  (kind (symbols-outline-node-kind node))
                  (signature (symbols-outline-node-signature node))
                  (collapsed (symbols-outline-node-collapsed node)))
         (puthash (concat name kind signature) collapsed collapsed-table))))
    (symbols-outline-node-foreach
     to
     (lambda (node)
       (when-let* ((name (symbols-outline-node-name node))
                   (kind (symbols-outline-node-kind node))
                   (signature (symbols-outline-node-signature node))
                   ((symbols-outline-node-children node))
                   (orig-collapsed (gethash (concat name kind signature) collapsed-table)))
         (setf (symbols-outline-node-collapsed node) t))))))

(provide 'symbols-outline-node)

;;; symbols-outline-node.el ends here
