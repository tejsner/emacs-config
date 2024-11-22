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

(use-package emacs
  :config
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
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
          (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.20.4"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.4")))))

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
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :hook
  (((python-ts-mode c-ts-mode c++-ts-mode c-or-c++-ts-mode lua-mode) . eglot-ensure))
  
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  (eldoc-echo-area-use-multiline-p nil)

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  (add-to-list 'eglot-server-programs
               '(lua-mode . ("lua-language-server"))))

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

;; Lua (install lua-language-server)
(use-package lua-mode
  :ensure t
  :custom
  (lua-indent-nested-block-content-align nil))

;; love2d (lua game framework) instructions
;; ----------------------------------------
;; To get proper completions with eglot create a .luarc.json file in
;; your working diretoty and set the workspace.library to the path of
;; the love2d addon found at https://github.com/LuaCATS/love2d
