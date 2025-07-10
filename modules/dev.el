;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  General options for dev
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :hook
  ((prog-mode . electric-pair-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tree-sitter
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ttps://www.masteringemacs.org/article/how-to-get-started-tree-sitter

;; To install all grammars, either:
;; - run (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
;; -  use the command `M-x treesit-install-language-grammar' on each language

;; The language sources are versioned in order to avoid problems with new
;; tree-sitter features not supported by emacs 29 (mid 2023).

(setq-local timsta--use-tree-sitter nil) ; tree-sitter toggle

(when timsta--use-tree-sitter
  (use-package emacs
    :config
    (setq major-mode-remap-alist
          '((yaml-mode . yaml-ts-mode)
            (bash-mode . bash-ts-mode)
            (json-mode . json-ts-mode)
            (js-json-mode . json-ts-mode)
            (css-mode . css-ts-mode)
            (python-mode . python-ts-mode)
            (toml-mode . toml-ts-mode)
            (c-mode . c-ts-mode)
            (c++-mode . c++-ts-mode)
            (c-or-c++-mode . c-or-c++-ts-mode)))
    (setq treesit-language-source-alist
          '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
            (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
            (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
            (lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua" "v0.2.0"))
            (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
            (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
            (toml "https://github.com/tree-sitter/tree-sitter-toml")
            (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
            (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.4"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Documentation eldoc
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setopt eldoc-echo-area-use-multiline-p nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("C-t v" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Language specific packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lisp (including dialects such as scheme) stuff
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package sicp
  :ensure t)

(use-package racket-mode
  :ensure t)

;; python virtual environment management
(use-package pyvenv
  :ensure t)

;; ESS (R, SAS etc)
(use-package ess
  :ensure t)

;; Lua (config depends on tree-sitter toggle)
(if timsta--use-tree-sitter
  (use-package lua-ts-mode
    :mode ("\\.lua\\'" . lua-ts-mode)
    :custom
    (lua-ts-indent-offset 3)
    :bind
    (:map lua-ts-mode-map
          ("C-c C-l" . lua-ts-send-buffer)
          ("C-c C-c" . compile)))
  (use-package lua-mode
    :ensure t
    :custom
    (lua-indent-level 3)
    (lua-indent-nested-block-content-align nil)
    :bind
    (:map lua-mode-map
          ("C-c C-c" . compile)
          ("C-c C-e" . lua-send-defun)
          ("C-c C-r" . lua-send-region))))
    
