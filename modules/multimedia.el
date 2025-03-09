;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   E-books
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package visual-fill-column
  :ensure t
  :custom
  (visual-fill-column-center-text t)
  :bind
  ("C-t z" . visual-fill-column-mode))

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

;; Requires mpd (with a working configuration) and mpc
(use-package simple-mpc
  :ensure t
  :bind
  (("C-t m" . simple-mpc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Feeds (elfeed) and E-mail (notmuch)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elfeed
  :ensure t
  :custom
  (elfeed-feeds '(("https://api.quantamagazine.org/feed/" science)
                  ("https://acoup.blog/feed/" blog)
                  ("https://dthompson.us/feed.xml" blog gamedev lisp)
                  ("https://karthinks.com/tags/emacs/index.xml" emacs)
                  ("https://protesilaos.com/codelog.xml" blog emacs)
                  ("https://www.masteringemacs.org/feed/" blog emacs)
                  ("https://xkcd.com/atom.xml" comics)
                  ("https://www.smbc-comics.com/comic/rss" comics)
                  ("https://godotengine.org/rss.xml" gamedev)
                  ("https://archlinux.org/feeds/news/" linux)
                  ("https://karpathy.github.io/feed.xml" blog ai)
                  ("https://api.dr.dk/podcasts/v1/feeds/brinkmanns-briks" podcast)
                  ("https://technomancy.us/atom.xml" blog gamedev)
                  ("https://spritely.institute/feed.xml" gamedev)
                  ("https://terrytao.wordpress.com/feed/" science)
                  ("https://scottaaronson.blog/?feed=rss2" science)
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
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0JB7TSe49lg56u6qH8y_MQ" youtube gamedev) ; GDC
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCXUPKJO5MZQN11PqgIvyuvQ" youtube) ; Andrej Karpathy
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCRhV1rWIpm_pU19bBm_2RXw" youtube podcast) ; Sean Carroll
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7M-Wz4zK8oikt6ATcoTwBA" youtube) ; Freya Holmér
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCQlJ7XpBtiMLKNSd4RAJmRQ" youtube) ; The Weekly Show with Jon Stewart
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3XTzVzaHQEd30rQbuvCtTQ" youtube) ; LastWeekTonight
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCSju5G2aFaWMqn-_0YBtq5A" youtube) ; Stand-up Maths
                  ))
  (elfeed-search-title-max-width 110)
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
