;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Agenda variables
(setq org-directory "~/wrk/org/") ; Non-absolute paths for agenda and
                                  ; capture templates will look here.

(setq org-agenda-files '("inbox.org" "projects.org"))

;; Org-refile: where should org-refile look?
(setq org-refile-targets '(("projects.org" . (:maxlevel . 1))))

;; org-download dir
(setq org-download-image-dir "~/wrk/org/img/org-download")

;; Advanced: Custom link types
;; This example is for linking a person's 7-character ID to their page on the
;; free genealogy website Family Search.
;; (setq org-link-abbrev-alist
;;       '(("family_search" . "https://www.familysearch.org/tree/person/details/%s")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Org-mode setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :bind (:map global-map
              ("C-t c" . org-capture)
              ("C-t a" . org-agenda))

  :config
  (require 'oc-csl)                     ; CSL citation support
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  ;; (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)

  ;; languages to use with org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (C . t)
     (sqlite . t)
     (shell . t)))

  ;; settings forsource code blocks
  (setq org-edit-src-content-indentation 0)
  (setq org-confirm-babel-evaluate nil)

  ;; Instead of just two states (TODO, DONE) we set up a few different states
  ;; that a task can be in.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "DONE(d!)")))
  (setq org-log-done t)

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  (setq org-capture-templates
        '(("i" "Inbox" entry (file "inbox.org")
           "* TODO %?\n/Entered on/ %U\n%i")))

  (setq org-agenda-custom-commands
        '(("n" "Agenda and All Todos"
           ((agenda)
            (todo))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Extensions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-download
  :ensure t
  :config
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Export engines
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-reveal
(use-package ox-reveal :ensure t)

;; export to rst
(use-package ox-rst :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Holidays
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

