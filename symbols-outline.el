;;; symbols-outline.el --- Display symbols (functions, variables, etc) in outline view  -*- lexical-binding: t; -*-

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
;; This package allows the user to display a tree-like view of the document
;; symbols.

;;; Code:

(require 'cl-lib)
(require 'symbols-outline-node)
(require 'symbols-outline-ctags)
(require 'symbols-outline-nerd-icon)
(require 'symbols-outline-svg-icon)

(defgroup symbols-outline nil
  "Minor mode to display symbols outline on a side window."
  :group 'imenu
  :prefix "Outline")

(defcustom symbols-outline-window-position 'right
  "Position of symbols outline side window."
  :type '(choice
          (const right)
          (const left))
  :group 'symbols-outline)

(defcustom symbols-outline-window-width 30
  "Width of symbols outline side window."
  :type 'integer
  :group 'symbols-outline)

(defcustom symbols-outline-no-other-window t
  "Make `other-window' ignore the symbols-outline window."
  :type 'boolean
  :group 'symbols-outline)

(defcustom symbols-outline-no-delete-other-window t
  "Make `delete-other-windows' ignore the symbols-outline window."
  :type 'boolean
  :group 'symbols-outline)

(defcustom symbols-outline-collapse-functions-on-startup t
  "Whether to collapse function nodes on startup to hide the parameter nodes."
  :type 'boolean
  :group 'symbols-outline)

(defcustom symbols-outline-ignore-variable-symbols t
  "Whether to ignore variable symbols."
  :type 'boolean
  :group 'symbols-outline)

(defcustom symbols-outline-current-symbol-indicator "›"
  "Indicator string that marks the current symbol at point in the outline window.

Its length has to be 1."
  :type 'string
  :group 'symbols-outline)

(defcustom symbols-outline-max-symbols-threshold 10000
  "When the number of symbols exceeds this threshold, don't render."
  :type 'number
  :group 'symbols-outline)

(defcustom symbols-outline-use-nerd-icon-in-gui nil
  "Whether use nerd font icons in GUI mode."
  :type 'boolean
  :group 'symbols-outline)

(defcustom symbols-outline-use-nerd-icon-in-tui t
  "Whether use nerd font icons in TUI mode."
  :type 'boolean
  :group 'symbols-outline)

(defvar symbols-outline-buffer-name "*Outline*"
  "Buffer name for symbols outline side window.")

(defvar symbols-outline--origin nil
  "Original source buffer whose symbols outline is being shown.")

(defvar symbols-outline--margin-spec-cache (cons nil nil)
  "Cache the expanded/collapsed indicators on the margin.
It's a cons cell whose car/cdr is the expanded/collapsed indicator margin spec.")

(defvar-local symbols-outline--entries-tree nil)

(defun symbols-outline--get-kind-icon (kind)
  "Get icon for KIND."
  (let ((face (symbols-outline--get-kind-face kind)))
    (if (display-graphic-p)
        (if symbols-outline-use-nerd-icon-in-gui
            (symbols-outline-nerd-icon-str kind :face face)
          (symbols-outline-svg-icon-str kind :face face))
      (when symbols-outline-use-nerd-icon-in-tui
        (symbols-outline-nerd-icon-str kind :face face)))))

(defun symbols-outline--get-collapse-indicator (collapsed)
  "Get the indicator for whether the symbol node is COLLAPSED."
  (let* ((icon (if collapsed "chevron-right" "chevron-down"))
         (face (symbols-outline--get-kind-face icon)))
    (if (display-graphic-p)
        (if symbols-outline-use-nerd-icon-in-gui
            (symbols-outline-nerd-icon-str icon :face face)
          (symbols-outline-svg-icon icon :face face))
      (if symbols-outline-use-nerd-icon-in-tui
          (symbols-outline-nerd-icon-str icon :face face)
        (if collapsed "+" "-")))))

(defun symbols-outline--get-margin-spec-cache (collapsed)
  "Get the cached margin spec for whether the symbol node is COLLAPSED."
  (if-let (spec (funcall
                 (if collapsed #'cdr #'car) symbols-outline--margin-spec-cache))
      spec
    (funcall (if collapsed #'setcdr #'setcar)
             symbols-outline--margin-spec-cache
             (propertize " " 'display
                         `((margin right-margin)
                           ,(symbols-outline--get-collapse-indicator collapsed))))))

(defvar symbols-outline--kind-face-alist
  '(("function" tree-sitter-hl-face:function font-lock-function-name-face)
    ("method" tree-sitter-hl-face:function font-lock-function-name-face)
    ("prototype" tree-sitter-hl-face:function font-lock-function-name-face)
    ("annotation" tree-sitter-hl-face:function font-lock-function-name-face)
    ("constructor" tree-sitter-hl-face:constructor font-lock-function-name-face)
    ("class" tree-sitter-hl-face:type font-lock-type-face)
    ("struct" tree-sitter-hl-face:type font-lock-type-face)
    ("interface" tree-sitter-hl-face:type font-lock-type-face)
    ("union" tree-sitter-hl-face:type font-lock-type-face)
    ("enum" tree-sitter-hl-face:type font-lock-type-face)
    ("enumerator" tree-sitter-hl-face:property.definition font-lock-variable-name-face)
    ("enummember" tree-sitter-hl-face:property.definition font-lock-variable-name-face)
    ("using" font-lock-constant-face)
    ("namespace" font-lock-constant-face)
    ("variable" tree-sitter-hl-face:variable font-lock-variable-name-face)
    ("externvar" tree-sitter-hl-face:variable font-lock-variable-name-face)
    ("local" tree-sitter-hl-face:variable font-lock-variable-name-face)
    ("member" tree-sitter-hl-face:property.definition font-lock-variable-name-face)
    ("field" tree-sitter-hl-face:property.definition font-lock-variable-name-face)
    ("macro" font-lock-variable-name-face)
    ("string" tree-sitter-hl-face:string font-lock-variable-name-face)
    ("boolean" tree-sitter-hl-face:variable.special font-lock-variable-name-face)
    ("array" tree-sitter-hl-face:variable.special font-lock-variable-name-face)
    ("number" tree-sitter-hl-face:number font-lock-variable-name-face)
    ("object" tree-sitter-hl-face:variable font-lock-variable-name-face)
    ("misc" tree-sitter-hl-face:variable.special font-lock-variable-name-face)
    ("operator" tree-sitter-hl-face:operator font-lock-variable-name-face)
    ("parameter" tree-sitter-hl-face:variable.parameter font-lock-variable-name-face)
    ("macroparam" tree-sitter-hl-face:type.parameter font-lock-variable-name-face)
    ("typeparameter" tree-sitter-hl-face:type.parameter font-lock-variable-name-face)
    ("tparam" tree-sitter-hl-face:type.parameter font-lock-variable-name-face)
    ("event" tree-sitter-hl-face:variable font-lock-variable-name-face)
    ("typedef" tree-sitter-hl-face:type font-lock-type-face)
    ("package" font-lock-constant-face)
    ("module" font-lock-constant-face)
    ("key" tree-sitter-hl-face:keyword font-lock-variable-name-face)
    ("null" tree-sitter-hl-face:variable font-lock-variable-name-face)
    ("derivedMode" font-lock-function-name-face)
    ("majorMode" font-lock-function-name-face)
    ("minorMode" font-lock-function-name-face)
    ("inline" font-lock-function-name-face)
    ("subst" font-lock-function-name-face)
    ("group" font-lock-type-face)
    ("error" error)
    ("custom" font-lock-variable-name-face)
    ("face" font-lock-variable-name-face)
    ("const" font-lock-variable-name-face)
    ("alias" font-lock-function-name-face)
    ("unknown" warning)
    ("constant" tree-sitter-hl-face:constant font-lock-variable-name-face)
    ("property" tree-sitter-hl-face:property font-lock-variable-name-face)
    ("chapter" markdown-header-face-1)
    ("section" markdown-header-face-2 org-level-2)
    ("subsection" markdown-header-face-3)
    ("part" org-level-1)))

(defun symbols-outline--get-kind-face (kind)
  "Get face for node of KIND."
  (when-let (faces (cdr (assoc kind symbols-outline--kind-face-alist)))
    (seq-find #'facep faces)))

(defun symbols-outline--get-symbol-face-from-origin-buf (name line)
  "Get face for node from the origin buffer given NAME and LINE."
  (with-current-buffer symbols-outline--origin
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (search-forward name (line-end-position))
      (get-text-property (1- (point)) 'face))))

(defun symbols-outline--display-symbol-in-origin ()
  "Locate the symbol at point in the original buffer."
  (when-let (line (get-text-property (line-beginning-position) 'line))
    (with-selected-window (get-buffer-window symbols-outline--origin)
      (goto-char (point-min))
      (forward-line (1- line))
      (recenter))))

(defun symbols-outline--before-move ()
  "Delete the indicator for current symbol before movement."
  (aset (get-text-property (line-beginning-position) 'line-prefix) 0 ?\s))

(defun symbols-outline--after-move ()
  "Set the indicator for current symbol after movement."
  (aset (get-text-property (line-beginning-position) 'line-prefix) 0
        (aref symbols-outline-current-symbol-indicator 0)))

(defun symbols-outline-next (n)
  "Move to the next symbol.
Argument N means number of symbols to move."
  (interactive "P")
  (symbols-outline--before-move)
  (forward-line (or n 1))
  (beginning-of-line)
  (symbols-outline--display-symbol-in-origin)
  (symbols-outline--after-move))

(defun symbols-outline-prev (n)
  "Move to the previous symbol.
Argument N means number of symbols to move."
  (interactive "P")
  (symbols-outline-next (- (or n 1))))

(defun symbols-outline-next-same-level ()
  "Move to the next symbol on the same level/depth."
  (interactive)
  (symbols-outline--before-move)
  (let ((current-level (get-text-property (line-beginning-position) 'depth))
        (orig-pos (point))
        line0)
    (cl-loop do
             (setq line0 (line-number-at-pos))
             (forward-line 1)
             until (or (eq current-level
                           (get-text-property (line-beginning-position) 'depth))
                       (eq (line-number-at-pos) line0))
             finally (beginning-of-line))
    (if (eq (line-number-at-pos) line0)
        (progn
          (message "No more symbols.")
          (goto-char orig-pos))
      (symbols-outline--display-symbol-in-origin)))
  (symbols-outline--after-move))

(defun symbols-outline-prev-same-level ()
  "Move to the previous symbol on the same level/depth."
  (interactive)
  (symbols-outline--before-move)
  (let ((current-level (get-text-property (line-beginning-position) 'depth))
        (orig-pos (point))
        line0)
    (cl-loop do
             (setq line0 (line-number-at-pos))
             (forward-line -1)
             until (or (eq current-level
                           (get-text-property (line-beginning-position) 'depth))
                       (eq (line-number-at-pos) line0))
             finally (beginning-of-line))
    (if (eq (line-number-at-pos) line0)
        (progn
          (message "No more symbols.")
          (goto-char orig-pos))
      (symbols-outline--display-symbol-in-origin)))
  (symbols-outline--after-move))

(defun symbols-outline-move-depth-up ()
  "Move to the parent of the symbol at point."
  (interactive)
  (symbols-outline--before-move)
  (if-let* ((current-level (get-text-property (line-beginning-position) 'depth))
            ((> current-level 0))
            (target-level (1- current-level)))
      (cl-loop do
               (forward-line -1)
               until (eq target-level
                         (get-text-property (line-beginning-position) 'depth))
               finally
               (beginning-of-line)
               (symbols-outline--display-symbol-in-origin))
    (message "No parent symbols."))
  (symbols-outline--after-move))

(defun symbols-outline-move-depth-down ()
  "Move to the children of the symbol at point."
  (interactive)
  (symbols-outline--before-move)
  (let* ((current-level (get-text-property (line-beginning-position) 'depth))
         (target-level (1+ current-level))
         (orig-pos (point))
         line0)
    (cl-loop do
             (setq line0 (line-number-at-pos))
             (forward-line 1)
             until (or (eq target-level
                           (get-text-property (line-beginning-position) 'depth))
                       (eq (line-number-at-pos) line0))
             finally (beginning-of-line))
    (if (eq (line-number-at-pos) line0)
        (progn
          (message "No children symbols.")
          (goto-char orig-pos))
      (symbols-outline--display-symbol-in-origin)))
  (symbols-outline--after-move))

(defun symbols-outline-visit ()
  "Visit symbol under cursor."
  (interactive)
  (let ((line (get-text-property (line-beginning-position) 'line)))
    (pop-to-buffer symbols-outline--origin)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun symbols-outline-visit-and-quit ()
  "Visit symbol under cursor and quit the symbols-outline window."
  (interactive)
  (let ((line (get-text-property (line-beginning-position) 'line)))
    (quit-window)
    (pop-to-buffer symbols-outline--origin)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun symbols-outline-toggle-node ()
  "Collapse or expand the node at point."
  (interactive)
  (if-let* ((node (get-text-property (line-beginning-position) 'node))
            (children (symbols-outline-node-children node))
            (depth (get-text-property (line-beginning-position) 'depth))
            (ov (symbols-outline-node-ov node))
            (inhibit-read-only t))
      (save-excursion
        (if (symbols-outline-node-collapsed node)
            (progn
              (setf (symbols-outline-node-collapsed node) nil)
              (overlay-put ov 'before-string (symbols-outline--get-margin-spec-cache nil))
              ;; Insert children
              (end-of-line)
              (insert "\n")
              (dolist (child children)
                (symbols-outline--insert-node child (1+ depth)))
              (delete-char -1))
          (setf (symbols-outline-node-collapsed node) t)
          (overlay-put ov 'before-string (symbols-outline--get-margin-spec-cache t))
          ;; Delete children
          (beginning-of-line)
          (let ((pos1 (line-beginning-position 2))
                (pos2 (cl-loop do (forward-line)
                               while (and (not (eobp))
                                          (> (or (get-text-property (point) 'depth) -1) depth))
                               finally return (point))))
            (mapc #'delete-overlay (overlays-in pos1 pos2))
            (delete-region pos1 pos2))))
    (message "No children at the current node.")))

(defun symbols-outline-cycle-visibility-globally ()
  "Globally cycle through visibility levels like `org-mode' shift-tab."
  (interactive)
  (goto-char (point-min))
  (let* ((tree (with-current-buffer symbols-outline--origin
                 symbols-outline--entries-tree))
         (deepest-depth (symbols-outline-node-depth-non-collpased tree)))
    (if (> deepest-depth 2)
        ;; Hide nodes of deepest depth
        (symbols-outline-node-foreach-at-depth
         tree (- deepest-depth 2)
         (lambda (node) (setf (symbols-outline-node-collapsed node) t)))
      ;; Expand all nodes
      (symbols-outline-node-foreach
       tree
       (lambda (node) (setf (symbols-outline-node-collapsed node) nil)))))
  (symbols-outline--render))

(defvar symbols-outline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'symbols-outline-refresh)
    (define-key map (kbd "n") 'symbols-outline-next)
    (define-key map (kbd "p") 'symbols-outline-prev)
    (define-key map (kbd "f") 'symbols-outline-next-same-level)
    (define-key map (kbd "b") 'symbols-outline-prev-same-level)
    (define-key map (kbd "u") 'symbols-outline-move-depth-up)
    (define-key map (kbd "d") 'symbols-outline-move-depth-down)
    (define-key map (kbd "TAB") 'symbols-outline-toggle-node)
    (define-key map [tab] 'symbols-outline-toggle-node)
    (define-key map (kbd "S-TAB") 'symbols-outline-cycle-visibility-globally)
    (define-key map [backtab] 'symbols-outline-cycle-visibility-globally)
    (define-key map (kbd "RET") 'symbols-outline-visit)
    (define-key map (kbd "M-RET") 'symbols-outline-visit-and-quit)
    map)
  "Keymap for `symbols-outline-mode'.")

;;;###autoload
(define-derived-mode symbols-outline-mode special-mode "Symbols outline"
  "Mode showing symbols outline."
  :group 'symbols-outline
  (if (display-graphic-p)
      (setq-local right-margin-width 2)
    (setq-local right-margin-width 1))
  ;; (set-window-buffer (selected-window) (current-buffer))
  )

(defun symbols-outline--insert-line (node depth)
  "Insert a line of NODE at DEPTH."
  (let* ((name (symbols-outline-node-name node))
         (kind (symbols-outline-node-kind node))
         (face (symbols-outline--get-kind-face kind))
         (line (symbols-outline-node-line node))
         (lp (concat
              " "
              (make-string depth ?\s)  ; indentation
              (when (or (display-graphic-p)  ; icon
                        symbols-outline-use-nerd-icon-in-tui)
                (concat (symbols-outline--get-kind-icon (or kind "null"))
                        " ")))))

    ;; Add chevrons indicating whether the node is collapsed
    (when (symbols-outline-node-children node)
      (if-let (ov (symbols-outline-node-ov node))
          (unless (eq (overlay-start ov) (point))
            (move-overlay ov (point) (point)))
        (setq ov (make-overlay (point) (point)))
        (setf (symbols-outline-node-ov node) ov))
      (overlay-put (symbols-outline-node-ov node) 'before-string
                   (symbols-outline--get-margin-spec-cache
                    (symbols-outline-node-collapsed node))))

    (insert (propertize name
                        'line line
                        'depth depth
                        'face face
                        'node node
                        'line-prefix lp))))

(defcustom symbols-outline-function-node-kinds
  '("function" "method" "prototype" "annotation" "inline" "subst" "member")
  "Tag kinds that denote a function node."
  :type '(repeat string)
  :group 'symbols-outline)

(defcustom symbols-outline-variable-node-kinds
  '("variable" "externvar" "local" "member" "field" "parameter" "macroparam"
    "typeparameter" "tparam" "constant" "property")
  "Tag kinds that denote a variable node."
  :type '(repeat string)
  :group 'symbols-outline)

(defun symbols-outline--collapse-function-nodes (tree)
  "Set the `collapsed' property to t for function nodes of TREE."
  (symbols-outline-node-foreach
   tree
   (lambda (node) (when (and (symbols-outline-node-children node)
                             (member (symbols-outline-node-kind node)
                                     symbols-outline-function-node-kinds))
                    (setf (symbols-outline-node-collapsed node) t)))))

(defun symbols-outline--prune-variable-nodes (tree)
  "Prune variable nodes from TREE."
  (symbols-outline-node-foreach
   tree (lambda (node)
          (when (member (symbols-outline-node-kind node)
                        symbols-outline-variable-node-kinds)
            (setf (symbols-outline-node-children (symbols-outline-node-parent node))
                  (delq node
                        (symbols-outline-node-children (symbols-outline-node-parent node))))))))

(defun symbols-outline--insert-node (node depth)
  "Insert NODE at DEPTH."
  (let ((children-depth depth))
    ;; Insert current node
    (when (symbols-outline-node-name node)
      (symbols-outline--insert-line node depth)
      (insert "\n")
      (setq children-depth (1+ children-depth)))
    ;; Insert children
    (when (and (symbols-outline-node-children node)
               (not (symbols-outline-node-collapsed node)))
      (dolist (child (symbols-outline-node-children node))
        (symbols-outline--insert-node child children-depth)))))

(defvar symbols-outline--refreshing nil)

(defun symbols-outline--follow-symbol ()
  "Follow the symbol the cursor is currently at in the symbols-outline window."
  ;; This function costs around 0.0001s
  (with-current-buffer symbols-outline--origin
    (when-let* (((not symbols-outline--refreshing))
                symbols-outline--entries-tree
                (ln (line-number-at-pos))
                ((not (eq ln (with-current-buffer symbols-outline-buffer-name
                               (get-text-property (line-beginning-position) 'line)))))
                (win (get-buffer-window symbols-outline-buffer-name))
                (at-node (symbols-outline-node-find-symbol-at-line
                          symbols-outline--entries-tree ln)))
      (with-selected-window win
        (symbols-outline--before-move)
        (goto-char (point-min))
        (cl-loop for pos = (search-forward (symbols-outline-node-name at-node) nil t)
                 until (or (null pos)
                           (eq (get-text-property (line-beginning-position) 'line)
                               (symbols-outline-node-line at-node)))
                 finally (beginning-of-line))
        (symbols-outline--after-move)))))

(defun symbols-outline--follow (&optional _)
  "Follow the cursor in original buffer."
  (when-let (buffer-file-name
             ((not (eq last-command 'self-insert-command)))
             (win (get-buffer-window symbols-outline-buffer-name))
             (selected-buf (window-buffer (selected-window))))
    (if (eq symbols-outline--origin selected-buf)
        ;; Same buffer -> just follow symbol under point
        (symbols-outline--follow-symbol)
      ;; Changed buffer -> refresh symbols-outline buffer
      (setq symbols-outline--origin selected-buf)
      (symbols-outline-refresh))))

(defun symbols-outline--render ()
  "Render the symbols outline buffer."
  (with-current-buffer symbols-outline-buffer-name
    (let* ((tree (with-current-buffer symbols-outline--origin
                   symbols-outline--entries-tree))
           (symbols-outline--refreshing nil)
           (inhibit-read-only t))
      (delete-all-overlays)
      (erase-buffer)
      ;; when the tree is not empty
      (when (symbols-outline-node-children tree)
        (symbols-outline--insert-node tree 0)
        (delete-char -1)
        (goto-char (point-min))
        (symbols-outline--after-move)
        (symbols-outline--follow-symbol)))))

(defun symbols-outline--refresh-tree (tree)
  "Refresh symbols outline buffer content given TREE."
  (if symbols-outline-ignore-variable-symbols
      (symbols-outline--prune-variable-nodes tree))
  (with-current-buffer symbols-outline--origin
    (if symbols-outline--entries-tree
        ;; There exists previous tree -> reuse its collapse states
        (symbols-outline-node--copy-collapse-state
         symbols-outline--entries-tree tree)
      ;; Else -> maybe collapse function nodes
      (when symbols-outline-collapse-functions-on-startup
        (symbols-outline--collapse-function-nodes tree)))
    (setq symbols-outline--entries-tree tree))
  ;; Render the symbols
  (with-current-buffer symbols-outline-buffer-name
    (let* ((inhibit-read-only t))
      (symbols-outline--render)
      (setq symbols-outline--refreshing nil))))

(defvar symbols-outline-fetch-fn #'symbols-outline-ctags-fetch
  "Function to fetch symbols.
By async design, after it got the symbols as a tree, it should
take an argument of refresh-fn and funcall it upon the symbols
tree in order to refresh the symbols outline buffer.

The tree should be of type `symbols-outline-node'.  The root of
the tree will be a pseudo node and the rendering will start from
its children.")

(defun symbols-outline-refresh ()
  "Refresh symbols outline buffer."
  (interactive)
  ;; Only refresh when the origin buffer has file name or in the outline buffer
  (when (or (buffer-file-name symbols-outline--origin)
            (equal buffer-file-name symbols-outline-buffer-name))
    (with-current-buffer symbols-outline-buffer-name
      (setq-local default-directory
                  (buffer-local-value 'default-directory symbols-outline--origin)))
    (setq symbols-outline--refreshing t)
    (funcall symbols-outline-fetch-fn #'symbols-outline--refresh-tree)))

;;;###autoload
(defun symbols-outline-show ()
  "Show symbols outline in side window."
  (interactive)
  (setq symbols-outline--origin (current-buffer))
  (let ((buf (get-buffer-create symbols-outline-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'symbols-outline-mode)
        (symbols-outline-mode)))
    (if-let ((window (get-buffer-window buf)))
        (select-window window)
      (symbols-outline-refresh)
      (let ((win (display-buffer-in-side-window
                  buf `((side . ,(if (eq symbols-outline-window-position 'left)
                                     'left 'right))
                        (slot . 1)
                        (window-width . ,symbols-outline-window-width)))))
        (select-window win)
        (set-window-start win 1)
        (set-window-dedicated-p win t)
        (set-window-parameter win 'no-other-window symbols-outline-no-other-window)
        (set-window-parameter win 'no-delete-other-windows symbols-outline-no-delete-other-window)))))

(provide 'symbols-outline)

;;; symbols-outline.el ends here
