# Introduction

This package displays all the symbols (including classes, functions, variables, etc., depending on what the backend outputs), in an outline window.

# Features

1. List the symbols in a well-organized window.
2. The outline window position follows your cursor in the original buffer.
3. It's easy to navigate between symbols via moving in the outline window.
4. Allows various and custom backends. Currently ctags and lsp-mode backends are provided.

# Demo

<img src="./screen_record.gif">

# Requirements

Symbols-outline.el relies on a backend to get the symbols. It works best if ctags is installed. Or if `lsp-mode` is active, you can use the lsp backend. See `symbols-outline-fetch-fn`.

# Example configuration

``` emacs-lisp
(global-set-key (kbd "C-c i") 'symbols-outline-show)
(with-eval-after-load 'symbols-outline
  ;; By default the ctags backend is selected
  (unless (executable-find "ctags")
    (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch))
  (setq symbols-outline-window-position 'left)
  (symbols-outline-follow-mode))
```

# Keybindings in the outline window

| Key   | Command                                   | Description                                                                       |
|:------|:------------------------------------------|:----------------------------------------------------------------------------------|
| g     | symbols-outline-refresh                   | Refresh                                                                           |
| n     | symbols-outline-next                      | Go to the next symbol                                                             |
| p     | symbols-outline-prev                      | Go to the previous symbol                                                         |
| f     | symbols-outline-next-same-level           | Go the the next symbol of the same level                                          |
| b     | symbols-outline-prev-same-level           | Go the the previous symbol of the same level                                      |
| u     | symbols-outline-move-depth-up             | Go the the parent symbol                                                          |
| d     | symbols-outline-move-depth-down           | Go the the child symbol                                                           |
| TAB   | symbols-outline-toggle-node               | Fold/unfold the children symbols                                                  |
| S-TAB | symbols-outline-cycle-visibility-globally | Fold/unfold globally                                                              |
| RET   | symbols-outline-visit                     | Go to the symbol under cursor in the original buffer                              |
| M-RET | symbols-outline-visit-and-quit            | Go to the symbol under cursor in the original buffer and close the outline window |
