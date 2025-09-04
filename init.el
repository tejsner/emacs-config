;;; init.el --- Tim's init.el -*- lexical-binding: t; -*-

;; TODO: Consider packages: cape, embark, wgrep
;; TODO: Set up tree-sitter
;; TODO: Set up eglot
;; TODO: Check out denote-explore after building a note library
;; TODO: Set up directories (denote, citar, org) in central location/register as variables

;;; BASIC SETTINGS

;; Package and use-package settings
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package use-package
  :custom
  (package-native-compile t)
  (warning-minimum-level :emergency))

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

(setopt display-time-default-load-average nil)   ; Remove load-average when displaying time
(setopt use-short-answers t)                     ; Answer y/n to prompts
(savehist-mode)                                  ; Save history of minibuffer between restarts
(setopt ring-bell-function 'ignore)              ; remove bell
(when (display-graphic-p) (context-menu-mode))   ; Contextual menu with right-click
(recentf-mode t)                                 ; Save list of recently opened files
(savehist-mode)                                  ; Minibuffer history persists over restarts

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;;; KEYBINDS

;; Use C-t as a leader-key for custom commands, overrides transpose-chars
;; NOTE: Certain keys will not be available in dired-mode, due to image-dired C-t prefix
(defvar-keymap timsta-leader-key)
(keymap-set global-map "C-t" timsta-leader-key)

;; which-key: Show popup of keybinds when typing key sequences
(use-package which-key :config (which-key-mode))

;; move between windows with ctrl+arrows
(windmove-default-keybindings 'control) 

;; Use C-z to switch windows, overrides suspend-frame
(use-package emacs
  :bind (("C-z" . other-window)))

;;; LOOK AND FEEL

;; theme: modus-operandi (light) or modus-vivendi (dark)
(use-package emacs
   :config (load-theme 'modus-operandi t))

(setopt inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)                                 ; Steady cursor
(pixel-scroll-precision-mode)                          ; Smooth scrolling
(add-hook 'prog-mode-hook 'display-line-numbers-mode)  ; line numbers in prog-mode
(setopt display-line-numbers-width 3)                  ; minimum width of line numbers
(setopt line-number-mode t)                            ; Show current line in modeline
(setopt column-number-mode t)                          ; Show current column in modeline
(setopt indicate-buffer-boundaries 'left)              ; Show buffer top and bottom in the margin

;;; COMPLETION/MINIBUFFER

;; Vertico: Vertical completion in minibuffer
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Orderless: completion style that matches on words in any order
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

;; Consult: Search and navigation based on completion
(use-package consult
  :ensure t
  :bind
  (("C-t g" . consult-ripgrep)
   :map org-mode-map
   ("C-c h" . consult-org-heading)))

;;; FILE MANAGEMENT

(setopt dired-kill-when-opening-new-dired-buffer t)

;; backups
(setq-default backup-directory-alist
              `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
              version-control t
              delete-old-versions t
              create-lockfiles nil)

;;; EDITOR

;; Spaces over tabs
(setopt indent-tabs-mode nil)
(setopt tab-width 4)

;; Avy: Motion aids
(use-package avy
  :ensure t
  :bind (("C-t j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

;; Common file types unsupported by stock Emacs
(use-package markdown-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package json-mode :ensure t)

;;; LANGUAGE: Text

(use-package text-mode
  :hook
  (text-mode . visual-line-mode)
  (text-mode . abbrev-mode)
  :custom
  (sentence-end-double-space nil)
  (dictionary-server "dict.org")
  :bind
  (("C-t t d" . dictionary-lookup-definition)))

;; Writegood
(use-package writegood-mode
  :ensure t
  :hook
  (text-mode . writegood-mode)
  :bind
  (("C-t t r" . writegood-reading-ease)))

;; Titlecasing
(use-package titlecase
  :ensure t
  :bind
  (("C-t t t" . titlecase-dwim)))

;; Lorem Ipsum generator
(use-package lorem-ipsum
  :ensure t
  :custom
  (lorem-ipsum-list-bullet "- ") ;; Org mode bullets
  :init
  (setq lorem-ipsum-sentence-separator
        (if sentence-end-double-space "  " " "))
  :bind
  (("C-t t i" . lorem-ipsum-insert-paragraphs)))

;; Spell-checking
(use-package flyspell
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_US")
  :hook
  (text-mode . flyspell-mode))

;; Distraction-free writing
(use-package olivetti
  :ensure t
  :bind
  (("C-t t o" . olivetti-mode)))

;; Script writing with Fountain Mode
(use-package fountain-mode :ensure t)

;;; LANGUAGE: Org

(use-package org
  :custom
  (org-startup-indented t)
  (org-image-actual-width '(450))
  (org-pretty-entities t)
  (org-id-link-to-org-use-id t)
  (org-fold-catch-invisible-edits 'show)
  :bind
  (("C-t a" . org-agenda)))

;; Hide emphasis markers (bold, underline, ...) unless cursor is on word
(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :custom (org-hide-emphasis-markers t))

;; Insert link, view and capture web pages from org mode
(use-package org-web-tools
  :ensure t)

;; LaTeX previews
(use-package org-fragtog
  :ensure t
  :after org
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-startup-with-latex-preview nil)
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

;; Add danish holidays to agenda
(defvar holiday-denmark-holidays
  '((holiday-fixed 1 1 "Nytårsdag")
    (holiday-easter-etc -3 "Skærtorsdag")
    (holiday-easter-etc -2 "Langfredag")
    (holiday-easter-etc 0 "Påskedag")
    (holiday-easter-etc 1 "Anden påskedag")
    (holiday-easter-etc 26 "Stor bededag")
    (holiday-easter-etc 39 "Kristi himmelfartsdag")
    (holiday-easter-etc 49 "Pinsedag")
    (holiday-easter-etc 50 "Anden Pinsedag")
    (holiday-fixed 12 24 "Juleaften")
    (holiday-fixed 12 25 "Juledag")
    (holiday-fixed 12 26 "Anden juledag")
    (holiday-fixed 12 31 "Nytårsaften"))
  "Danish holidays.")

(setq calendar-holidays holiday-denmark-holidays)
(setq org-agenda-include-diary t)

;;; NOTE TAKING

(use-package org
  :bind
  (("C-t c" . org-capture))
  (:map org-mode-map ("C-c l" . org-store-link))
  :custom
  (org-capture-templates
   '(("f" "Fleeting note"
      item
      (file+headline org-default-notes-file "Notes")
      "- %?")
     ("p" "Permanent note" plain
      (file denote-last-path)
      #'denote-org-capture
      :no-save t
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)
     ("t" "New task" entry
      (file+headline org-default-notes-file "Tasks")
      "* TODO %i%?"))))

(use-package denote
  :ensure t
  :defer t
  :custom
  (denote-directory "/home/tim/wrk/notes")
  :bind
  (("C-t n n" . denote)
   ("C-t n b" . denote-find-backlink)
   ("C-t n l" . denote-find-link)
   ("C-t n i" . denote-link-or-create)
   ("C-t n o" . denote-open-or-create)
   ("C-t n d" . denote-date)
   ("C-t n r" . denote-rename-file)
   ("C-t n R" . denote-rename-file-using-front-matter)))

(use-package denote-journal
  :ensure t
  :bind
  (("C-t n j" . denote-journal-new-or-existing-entry)))

(use-package denote-org
  :ensure t
  :bind
  (("C-t n h" . denote-org-link-to-heading)))

(use-package consult-notes
  :ensure t
  :custom
  (consult-notes-denote-display-keywords-indicator "_")
  :bind
  (("C-t n f" . consult-notes)
   ("C-t n g" . consult-notes-search-in-all-notes))
  :init
  (consult-notes-denote-mode))

;;; LANGUAGE: Git

(use-package magit
  :ensure t
  :bind (("C-t v" . magit-status)))

;;; LANGUAGE: Python

(use-package pyvenv :ensure t)

;;; LANGUAGE: Common Lisp

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))

;;; LANGUAGE: Lua

(use-package lua-mode
  :ensure t
  :custom
  (lua-indent-level 3)
  (lua-indent-nested-block-content-align nil)
  :bind
  (:map lua-mode-map
        ("C-c C-c" . compile)
        ("C-c C-e" . lua-send-defun)
        ("C-c C-r" . lua-send-region)))

;;; TOOLS

;; Citation Management
(use-package citar
  :ensure t
  :hook
  (org-mode . citar-capf-setup)
  :bind (("C-t b i" . citar-insert-citation)
         ("C-t b f" . citar-open)
         ("C-t b o" . citar-open-entry)
         ("C-t b r" . citar-insert-reference))
  :custom
  (citar-file-open-functions '(("html" . citar-file-open-external)
                               ("pdf" . citar-file-open-external)
                               (t . find-file)))
  (citar-bibliography '("~/data/literature/books.bib"
                        "~/data/literature/articles.bib"))
  (citar-library-paths '("~/data/literature/books/"
                         "~/data/literature/articles")))

;; use/export citar citations in org-mode
(require 'oc-natbib)
(require 'oc-csl)
(setq org-cite-global-bibliography '("~/data/literature/books.bib"
                                     "~/data/literature/articles.bib")
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar)

(use-package citar-denote
  :ensure t
  :custom
  (citar-open-always-create-notes t)
  :init
  (citar-denote-mode)
  :bind
  (:map org-mode-map
        ("C-t b k" . citar-denote-add-reference)
        ("C-t b K" . citar-denote-remove-reference)
        ("C-t b d" . citar-denote-dwim)))

;; Documents (E-books)
(use-package nov
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; LLMs: gptel
(use-package gptel
  :ensure t
  :bind
  (("C-t l" . gptel)
   ("C-t RET". gptel-send))
  :custom
  (gptel-default-mode #'org-mode)
  :config
  (gptel-make-gemini "Gemini"
    :key (gptel-api-key-from-auth-source "generativelanguage.googleapis.com" "apikey")
    :stream t))

;; Feeds: elfeed
(use-package elfeed
  :ensure t
  :custom
  (elfeed-search-title-max-width 110)
  :bind
  (("C-t w" . elfeed)))

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

;; Music: Simple MPD client (requires mpd service)
(use-package simple-mpc
  :ensure t
  :bind
  (("C-t m" . simple-mpc)))

;; Configure opening files with external programs: openwith
(use-package openwith
  :ensure t
  :config (openwith-mode t)
  :custom
  (openwith-associations '((".pdf" "evince" (file)))))
                           

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   '(avy citar citar-denote consult consult-notes denote denote-journal
         denote-org elfeed-org emms fountain-mode gptel json-mode
         lorem-ipsum lua-mode magit marginalia markdown-mode nov
         olivetti openwith orderless org-appear org-fragtog
         org-web-tools pyvenv simple-mpc slime spacious-padding
         titlecase vertico wgrep writegood-mode yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
