;;; symbols-outline-ctags.el --- Ctags backend for symbols-outline  -*- lexical-binding: t; -*-

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
;; Ctags backend that retrieves the document symbols with ctags.

;;; Code:

(require 'symbols-outline-node)

(defcustom symbols-outline-ctags-executable "ctags"
  "The executable of ctags."
  :type 'string
  :group 'symbols-outline)

(defvar symbols-outline-ctags-lang-separator-alist
  '(("h" . "::")
    ("hpp" . "::")
    ("cc" . "::")
    ("cpp" . "::")
    ("cxx" . "::")
    ("md" . "\"\""))
  "Alist that maps languages (file extensions) to separators.")

(defvar symbols-outline--origin)
(defvar symbols-outline-max-symbols-threshold)

(defun symbols-outline-ctags--parse-output (buf)
  "Parse ctags output in buffer BUF."
  (let (linestr tags)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (not (eobp))
        (setq linestr
              (buffer-substring-no-properties (point) (line-end-position)))
        (when (and (string-prefix-p "{" linestr) (string-suffix-p "}" linestr))
          (push (json-parse-string linestr) tags))
        (forward-line 1)))
    (reverse tags)))

(defun symbols-outline-ctags--parse-entries-into-tree (entries)
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
          (let* ((ext (file-name-extension
                       (buffer-file-name symbols-outline--origin)))
                 (separator
                  (or (cdr (assoc ext
                                  symbols-outline-ctags-lang-separator-alist))
                      "\\.")))
            (setq parent (split-string parent separator))))
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
        ;; Parent nodes
        (if parent
            (progn
              ;; Node has parent, therefore delete node from the children list of root
              (setf (symbols-outline-node-children root)
                    (delq node (symbols-outline-node-children root)))
              ;; Pseudo parent nodes. They may have already been added by other
              ;; siblings; if not, add them.
              (let ((last-parent root))
                (cl-loop for p in parent
                         for lastp = (equal p (car (last parent)))
                         do
                         (unless (setq parent-node
                                       (symbols-outline-node-find
                                        root
                                        (lambda (n) (and
                                                     (equal p (symbols-outline-node-name n))
                                                     (if lastp
                                                         (equal parent-kind
                                                                (symbols-outline-node-kind n))
                                                       t)))))
                           (setq parent-node (if lastp
                                                 (make-symbols-outline-node :name p
                                                                            :kind parent-kind
                                                                            :parent last-parent)
                                               (make-symbols-outline-node :name p
                                                                          :parent last-parent)))
                           ;; Parent's parent set to last-parent
                           (push parent-node (symbols-outline-node-children last-parent)))
                         (setq last-parent parent-node))))
          (setq parent-node root))
        (setf (symbols-outline-node-parent node) parent-node)
        ;; Add to parent's children list
        (push node (symbols-outline-node-children parent-node))))
    (symbols-outline-node--sort-children root)
    root))

;;;###autoload
(defun symbols-outline-ctags-fetch (refresh-fn)
  "Retrieve symbols using ctags.
Argument REFRESH-FN should be called upon the retrieved symbols tree."
  (let* ((buf (get-buffer-create "*symbols-outline-ctags-output*"))
         (existing-process (get-buffer-process buf))
         (default-directory (buffer-local-value 'default-directory symbols-outline--origin)))
    (when existing-process (kill-process existing-process))
    (with-current-buffer buf
      (erase-buffer)
      (setq buffer-undo-list t))
    (let* ((process (start-file-process "symbols-outline-ctags"
                                        buf
                                        symbols-outline-ctags-executable
                                        "--output-format=json"
                                        "--kinds-all=*"
                                        "--fields=NznsS"
                                        "--sort=no"
                                        (expand-file-name
                                         (buffer-file-name
                                          symbols-outline--origin)))))
      (set-process-sentinel
       process
       (lambda (_proc status)
         (unless (string-match-p "hangup\\|killed" status)
           (if-let* ((n (with-current-buffer buf (count-lines (point-min) (point-max))))
                     ((< n symbols-outline-max-symbols-threshold)))
               (when (> n 0)
                 (thread-last buf
                              (symbols-outline-ctags--parse-output)
                              (symbols-outline-ctags--parse-entries-into-tree)
                              (funcall refresh-fn)))
             (message "Too many symbols (%s)" n))))))))

(provide 'symbols-outline-ctags)

;;; symbols-outline-ctags.el ends here
