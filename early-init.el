;;; early-init.el --- Tim's early-init.el -*- lexical-binding: t; -*-

;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Start maximized
(setq default-frame-alist '((fullscreen . maximized)))

;;; early-init.el ends here
