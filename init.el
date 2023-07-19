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

;; Set the low bar Emacs compatibility high
(defvar dotemacs-min-version "28.1")

(when (version< emacs-version dotemacs-min-version)
  (error "We demand spiffy new Emacs, at least v%s, but you have v%s"
         dotemacs-min-version
         emacs-version))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIRECTORY STRUCTURE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Take clues from bbatsov/prelude, except keep structure relative to our
;; initial dotemacs-dir path. This way we can start the user's emacs via
;; ~/.emacs.d symlinked to the dotemacs repo, and develop/debug against
;; the repo without potentially overwriting transient state files of the
;; daily driver .emacs.d.

(defvar dotemacs-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The dotemacs' root.  Normally it should be ~/.emacs.d.")

(defvar dotemacs-custom-file (expand-file-name "custom.el" dotemacs-dir)
  "Make Emacs add customisations here, instead of the init file.
Usually customisations made from the UI go into `custom-file'.")
(setq custom-file dotemacs-custom-file)
(unless (file-exists-p dotemacs-custom-file)
  (make-empty-file dotemacs-custom-file))
(load-file custom-file) ; load *now*, instead of unpredictable load sequence

(defvar dotemacs-savefile-dir (file-name-as-directory
                               (expand-file-name "savefile" dotemacs-dir))
  "This folder stores all the automatically generated save/history-files.")
(unless (file-exists-p dotemacs-savefile-dir)
  (make-directory dotemacs-savefile-dir))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BETTER DEFAULTS
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; via
;; - technomancy/better-defaults
;; - bbatsov/prelude
;; - vedang/emacs-up
;; - suvratapte/dot-emacs-dot-d

;; SPACES OVER TABS,
;; but keep up conventional appearances of 8 character wide tabs.
;; NOTE: Use `C-q TAB` to insert literal tabs.
(setq-default indent-tabs-mode nil
              tab-width 8
              fill-column 80)

(setq
 ;; PERFORMANCE
 ;; gc ~100MB for better overall performance. ~50-100MB is recommended
 ;; these days over the long-obsolete default of ~8MB.
 gc-cons-threshold (* 100 1024 1024)
 ;; Large files freeze Emacs. Warn for files over ~100MB.
 large-file-warning-threshold (* 100 1024 1024)
 ;; Always load newest byte code. cf. bbatsov/prelude
 load-prefer-newer t

 ;; INTERACTIONS
 inhibit-startup-message t
 ring-bell-function 'ignore ; no beeps
 require-final-newline t ; always well-form files
 confirm-kill-emacs 'y-or-n-p ; instead of disabling 'C-x C-c'
 create-lockfiles nil
 tab-always-indent 'complete
 tab-first-completion 'word
 uniquify-buffer-name-style 'forward)

;; MORE INTERACTIONS
;; http://ergoemacs.org/emacs/emacs_save_restore_opened_files.html
(global-auto-revert-mode 1) ; auto-revert buffer if file-on-disk changes
(delete-selection-mode t) ; delete selection for any keypress
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(fset 'yes-or-no-p 'y-or-n-p) ; typing "yes/no" gets annoying fast
(global-unset-key (kbd "C-z")) ; avoid fat fingering `suspend-frame'

;; UTF-8 as default encoding
;; ref: http://ergoemacs.org/emacs/emacs_encoding_decoding_faq.html
(set-language-environment "UTF-8")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VISUAL AESTHETICS
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go easy on the eyes. This high-contrast darkmode theme is built
;; into Emacs, as of Emacs version 28.1
(load-theme 'modus-vivendi)

;; More screen real estate
(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0) ; disable popup, make help text appear in echo area
(menu-bar-mode 0)
(column-number-mode t)
(set-fringe-mode '(5 . 13)) ;; describe variable fringe-mode
(global-display-line-numbers-mode 1) ; always show line numbers
(global-hl-line-mode +1)

;; Tweak Font sizes globally, and also set line number mode
(defun adi/set-frame-font--default ()
  "Interactively set default frame font for day to day work."
  (interactive)
  (set-frame-font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
  (global-display-line-numbers-mode -1))

(defun adi/set-frame-font--pair-prog ()
  "Interactively set frame font for pair programming."
  (interactive)
  (set-frame-font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  (global-display-line-numbers-mode 1))

(defun adi/set-frame-font--code-demo ()
  "Interactively set frame font for presentations and demos."
  (interactive)
  (set-frame-font "-1ASC-Liberation Mono-normal-normal-normal-*-28-*-*-*-m-0-iso10646-1")
  (global-display-line-numbers-mode -1))

;; Ensure we always start Emacs with the default font.
(adi/set-frame-font--default)

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
