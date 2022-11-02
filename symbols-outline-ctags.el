;;; symbols-outline-ctags.el --- Ctags backend for symbols-outline  -*- lexical-binding: t; -*-

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

(require 'symbols-outline-node)

(defcustom symbols-outline-ctags-executable "ctags"
  "The executable of ctags."
  :type 'string
  :group 'symbols-outline)

(defvar symbols-outline--origin)
(defvar symbols-outline-max-symbols-threshold)

(defun symbols-outline-ctags--parse-output (buf)
  "Parse ctags output in buffer BUF."
  (let (linestr tags)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (not (eq (point) (point-max)))
        (setq linestr
              (buffer-substring-no-properties (point) (line-end-position)))
        (when (and (string-prefix-p "{" linestr) (string-suffix-p "}" linestr))
          (push (json-parse-string linestr) tags))
        (forward-line 1)))
    (reverse tags)))

(defun symbols-outline--parse-entries-into-tree (entries)
  "Parse ENTRIES into a tree structure."
  (let ((root (make-symbols-outline-node)))
    (dolist (e entries)
      (let* ((name (gethash "name" e))
             (parent (gethash "scope" e))
             (parent-kind (gethash "scopeKind" e))
             (line (gethash "line" e))
             (kind (gethash "kind" e))
             (signature (gethash "signature" e))
             node parent-node)
        (when parent
          (if (string-match-p "::" parent)
              (setq parent (car (last (split-string parent "::"))))
            (setq parent (car (last (split-string parent "\\."))))))
        ;; (when (and parent (eq major-mode 'c++-mode))
        ;;   (setq parent (car (last (split-string parent "::")))))
        ;; (when (and parent (eq major-mode 'python-mode))
        ;;   (setq parent (car (last (split-string parent "\\.")))))
        ;; Current node
        (if (and (setq node
                       (seq-find (lambda (n) (and (equal name (symbols-outline-node-name n))
                                             (equal kind (symbols-outline-node-kind n))))
                                 (symbols-outline-node-children root)))
                 (not (symbols-outline-node-line node)))
            ;; If it exists as a pseudo node (has only name and kind
            ;; properties), meaning it has been added as a pseudo parent node by
            ;; its children, we complete its properties.
            (progn
              (setf (symbols-outline-node-kind node) kind)
              (setf (symbols-outline-node-signature node) signature)
              (setf (symbols-outline-node-line node) line))
          ;; Else create it.
          (setq node (make-symbols-outline-node :name name
                                                :kind kind
                                                :signature signature
                                                :line line)))
        ;; Parent node
        (if parent
            ;; Pseudo parent node. It may have already been added by other
            ;; siblings; if not, add it.
            (progn
              (unless (setq parent-node
                            (symbols-outline-node-find
                             root
                             (lambda (n) (and (equal parent (symbols-outline-node-name n))
                                         (equal parent-kind (symbols-outline-node-kind n))))))
                (setq parent-node (make-symbols-outline-node :name parent
                                                             :kind parent-kind
                                                             :parent root))
                ;; Parent's parent defaults to root first
                (push parent-node (symbols-outline-node-children root)))
              ;; Node has parent, therefore delete node from the children list of root
              (setf (symbols-outline-node-children root)
                    (delq node (symbols-outline-node-children root))))
          (setq parent-node root))
        (setf (symbols-outline-node-parent node) parent-node)
        ;; Add to parent's children list
        (push node (symbols-outline-node-children parent-node))))
    (symbols-outline-node--prune-pseudo-nodes root)
    (symbols-outline-node--sort-children root)
    root))

;;;###autoload
(defun symbols-outline-ctags-fetch ()
  (let* ((buf (get-buffer-create "*symbols-outline-ctags-output*"))
         (existing-process (get-buffer-process buf))
         (default-directory (with-current-buffer symbols-outline--origin
                              default-directory)))
    (when existing-process (kill-process existing-process))
    (with-current-buffer buf
      (erase-buffer)
      (setq buffer-undo-list t))
    (let* ((process (start-file-process "symbols-outline-ctags"
                                        buf
                                        symbols-outline-ctags-executable
                                        "--output-format=json"
                                        "--pseudo-tags={TAG_KIND_SEPARATOR}"
                                        "--kinds-all=*"
                                        "--fields=NznsS"
                                        "--sort=no"
                                        (expand-file-name
                                         (buffer-file-name
                                          symbols-outline--origin)))))
      (set-process-sentinel
       process
       (lambda (proc status)
         (unless (string-match-p "hangup\\|killed" status)
           (if-let* ((n (with-current-buffer buf (count-lines (point-min) (point-max))))
                     ((< n symbols-outline-max-symbols-threshold)))
               (thread-last buf
                            (symbols-outline-ctags--parse-output)
                            (symbols-outline--parse-entries-into-tree)
                            (symbols-outline--refresh-tree))
             (message "Too many symbols (%s)" n))))))))

(provide 'symbols-outline-ctags)

;;; symbols-outline-ctags.el ends here
