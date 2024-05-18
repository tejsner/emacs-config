;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   E-books
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package visual-fill-column
  :ensure t
  :custom
  (visual-fill-column-center-text t))

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width t)
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Music
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (emms-all)
  ;; use mpd server on localhost
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (setq emms-player-mpd-music-directory "~/data/music")
  ;; volme buttons change on mpd server (+/- in playlist view)
  (require 'emms-volume)
  (setq emms-volume-change-function 'emms-volume-mpd-change)
  ;; remove emms info on modeline
  (emms-mode-line-mode -1)
  (emms-playing-time-enable-display)
  (emms-playing-time-disable-display)
  ;; automatically connect and update cache
  ;; (emms-player-mpd-connect)
  ;; (emms-cache-set-from-mpd-all)
  :bind
  (("C-t m" . emms-smart-browse)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Feeds (elfeed) and E-mail (notmuch)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elfeed
  :ensure t
  :custom
  (elfeed-feeds '("https://api.quantamagazine.org/feed/"))
  :bind
  (("C-t w" . elfeed)))

(use-package notmuch
  :demand t
  :custom
  (notmuch-search-oldest-first nil)
  :config
  (defun timsta--notmuch-inbox ()
    (interactive)
    (notmuch-search "folder:INBOX"))
  :bind
  (("C-t e" . timsta--notmuch-inbox)))
