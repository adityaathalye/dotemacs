;;; package --- init.el  -*- lexical-binding: t -*- --- My Emacs configuration.

;;; Commentary:

;;; This file is not part of GNU Emacs.

;;; Author: Aditya Athalye
;;; Created on: 30 June 2023
;;; Copyright (c) 2023 Aditya Athalye

;;; License:
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the MIT license, which is included
;;; with this distribution. See the LICENCE.txt file.

;;; Code:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Always load newest byte code
(setq load-prefer-newer t) ; cf. bbatsov/prelude

;; Directory structure
;; Take clues from bbatsov/prelude, except keep structure relative to our
;; initial dotemacs-dir path. This way we can start the user's emacs via
;; ~/.emacs.d symlinked to the dotemacs repo, and develop/debug against
;; the repo without potentially overwriting transient state files of the
;; daily driver .emacs.d.
(defvar dotemacs-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The dotemacs' root.  Normally it should be ~/.emacs.d.")

(defvar dotemacs-savefile-dir (expand-file-name "savefile" dotemacs-dir)
  "This folder stores all the automatically generated save/history-files.")
(unless (file-exists-p dotemacs-savefile-dir)
  (make-directory dotemacs-savefile-dir))

;; Make emacs add customisations here, instead of the init file.
;; Usually customisations made from the UI go into custom-file.
(setq custom-file (expand-file-name "custom.el" dotemacs-dir))
(unless (file-exists-p custom-file)
  (make-empty-file custom-file))
(load-file custom-file) ; load *now*, instead of unpredictable load sequence

;;; Performance

;; Increase GC threshold for better overall performance. ~50-100MB
;; is commonly recommended, over the long-obsolete default of ~8MB.
(setq gc-cons-threshold 100000000)
;; Large files freeze Emacs. Warn if the file size is over ~100MB.
(setq large-file-warning-threshold 100000000)

;;; Sundries

;; Tabs v/s Spaces
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Just-Spaces.html
;; Spaces over tabs always, but keep up 8-wide conventional appearances.
;; Note: Use `C-q TAB` to insert literal tabs.
(setq-default indent-tabs-mode nil
	      tab-width 8)
(setq create-lockfiles nil) ; no lockfiles
(setq ring-bell-function 'ignore) ; no beeps
(setq require-final-newline t) ; always well-form files
(delete-selection-mode t) ; delete selection for any keypress
(global-display-line-numbers-mode 1) ; always show line numbers
(global-auto-revert-mode 1) ; auto-revert buffer if file-on-disk changes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual Aesthetics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)

;; More screen real estate
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(set-fringe-mode '(5 . 13)) ;; describe variable fringe-mode

;; Go easy on the eyes
;; This high-contrast darkmode theme is built into Emacs as of
;; Emacs version 28.1
(load-theme 'modus-vivendi)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
;; Explicitly set the exact package archives list
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
;; Set package download directory relative to the dotemacs-dir
(setq package-user-dir (file-name-as-directory
                        (expand-file-name "elpa" dotemacs-dir)))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ian Y.E. Pan's tutorial is a nice quick overview.
;; https://ianyepan.github.io/posts/setting-up-use-package/

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-expand-minimally t) ; set nil to debug use-package forms

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All the packages!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Packages useful to configure packages
(use-package diminish) ; to remove /minor/ mode name clutter from modeline
;; (use-package delight) ; to do finer-grained tweaks than diminish

;;; COMplete ANYthing, please!
;;; h/t suvratapte/dot-emacs-dot-d
(use-package company
  :bind (:map global-map
              ("TAB" . company-complete-common-or-cycle))
  :config
  (setq company-idle-delay 0.1)
  (global-company-mode t)
  :diminish)

;;; Lispy editing support

;; Tweak settings of built-in paren package
(use-package paren
  :ensure nil ; it already exists, don't try to search online
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode t)
  :diminish)

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  :bind
  (("M-[" . paredit-wrap-square)
   ("M-{" . paredit-wrap-curly))
  :diminish)

(use-package magit
  :diminish)

(provide 'init)
;;; init.el ends here
