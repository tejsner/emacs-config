;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq citar-bibliography '("~/data/literature/books.bib" "~/data/literature/articles.bib")) ; paths to your bibtex files
(setq citar-library-paths '("~/data/literature/books/" "~/data/literature/articles")) ;; path to pdfs
(setq denote-directory (expand-file-name "~/wrk/notes/")) ;; path to denote dir

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Citation Management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package citar
  :ensure t
  :hook
  (org-mode . citar-capf-setup)
  :bind (("C-c b i" . citar-insert-citation)
         ("C-c b f" . citar-open)
         ("C-c b o" . citar-open-entry)
         ("C-c b r" . citar-insert-reference))
  :custom
  ;; Allows you to customize what citar-open does
  (citar-file-open-functions '(("html" . citar-file-open-external)
                               ("pdf" . citar-file-open-external)
                               (t . find-file))))

;; Optional: if you have the embark package installed, enable the ability to act
;; on citations with citar by invoking `embark-act'.
(use-package citar-embark
  :diminish ""
  :ensure t
  :after citar embark
  :no-require
  :config (citar-embark-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Note Taking
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package denote
  :ensure t
  :bind 
  (:map global-map
        ("C-c n n" . denote)
        ("C-c n f" . denote-open-or-create)
        ;; Not implemented yet
        ;; ("C-c n c" . denote-region)
        ("C-c n N" . denote-type)
        ("C-c n d" . denote-date)
        ("C-c n z" . denote-signature)
        ("C-c n s" . denote-subdirectory)
        ("C-c n t" . denote-template)
        ("C-c n l" . denote-link)))


(use-package citar-denote
  :ensure t
  :diminish ""
  :custom
  (citar-denote-subdir t)
  :init
  (citar-denote-mode))

