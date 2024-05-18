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
  (elfeed-feeds '(("https://api.quantamagazine.org/feed/" science)
                  ("https://www.dr.dk/nyheder/service/feeds/indland" news)
                  ("https://www.dr.dk/nyheder/service/feeds/udland" news)
                  ("https://www.dr.dk/nyheder/service/feeds/penge" news)
                  ("https://www.dr.dk/nyheder/service/feeds/politik" news)
                  ("https://karthinks.com/tags/emacs/index.xml" emacs)
                  ("https://protesilaos.com/codelog.xml" emacs)
                  ("https://xkcd.com/atom.xml" comics)
                  ("https://godotengine.org/rss.xml" gamedev)
                  ("https://archlinux.org/feeds/news/" linux)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMb0O2CdPBNi-QqPk5T3gsQ" youtube) ; James Hoffmann
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCR2_8uHyWcTSV6iIw6BN4rg" youtube) ; Lance Hedrick
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA" youtube) ; Computerphile
                  ("https://www.youtube.com/feeds/videos.xml?playlist_id=PLeskMkEaHJYfTMaKwmBWGq4NogX9FLF2J" youtube) ; Daily Show: Jon Stewart
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCHnyfMqiRRG1u-2MsSQLbXA" youtube) ; Veritasium
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw" youtube) ; 3Blue1Brown
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCvBqzzvUBLCs8Y7Axb-jZew" youtube) ; Sixty Symbols
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC1_uAIS3r8Vu6JjXWvastJg" youtube) ; Mathologer
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCoxcjq-8xIDTYp3uz647V5A" youtube) ; Numberphile
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCyNtlmLB73-7gtlBz00XOQQ" youtube) ; Folding Ideas
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0JB7TSe49lg56u6qH8y_MQ" youtube) ; GDC
                  ))
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
