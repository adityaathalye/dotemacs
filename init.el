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
 enable-recursive-minibuffers t
 minibuffer-depth-indicate-mode t
 uniquify-buffer-name-style 'forward
 uniquify-after-kill-buffer-p t ; rename after killing uniquified
 uniquify-ignore-buffers-re "^\\*") ; spare special buffers

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
(global-visual-line-mode +1) ; prefer Visual Line Mode
;; (add-hook 'text-mode-hook #'visual-line-mode) ; selectively, instead of global visual line mode
;; (add-hook 'org-mode-hook  #'visual-line-mode) ; selectively, instead of global visual line mode

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

(eval-when-compile
  (add-to-list 'load-path package-user-dir)
  (require 'use-package))

(setq use-package-always-ensure t)
(setq use-package-expand-minimally t) ; set nil to debug use-package forms

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All the packages!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Packages useful to configure packages

;; radian-software/blackout to tune major/minor mode names
;; in modeline. It unifies functionality of the mutually
;; confusing modeline lighters delight, diminish, and dim.
;; It integrates seamlessly with use-package.
(use-package blackout)

(use-package helpful ; h/t systemcrafters.net
  ;; https://github.com/Wilfred/helpful
  :ensure t
  :config
  (setq helpful-max-buffers 1) ; but actually we want it to reuse buffer
  :bind (("C-h f" . #'helpful-callable)
         ("C-h F" . #'helpful-function) ; exclude macros
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-h x" . #'helpful-command)
         ;; Lookup the current symbol at point. C-c C-d is
         ;; a common keybinding for this in lisp modes.
         ("C-c C-d" . #'helpful-at-point))
  :blackout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remember states of files, buffer, desktop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package recentf
  :ensure t
  :config
  (setq recentf-save-file (expand-file-name "recentf"
                                            dotemacs-savefile-dir)
        recentf-max-saved-items 1000
        recentf-max-menu-items 10
        recentf-auto-cleanup 'never)
  (recentf-mode +1)
  :blackout)

(use-package savehist
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring)
        savehist-autosave-inerval 60
        savehist-file (expand-file-name "savehist"
                                        dotemacs-savefile-dir)))

(use-package saveplace
  :config
  (setq save-place-file
        (expand-file-name "saveplace" dotemacs-savefile-dir))
  (save-place-mode +1))

(use-package desktop
  :config
  (add-to-list 'desktop-path dotemacs-savefile-dir)
  (setq desktop-dirname dotemacs-savefile-dir
        desktop-auto-save-timeout 30)

  (defun adi/desktop-read-after-emacs-startup ()
    (when (file-exists-p
           (expand-file-name desktop-base-file-name desktop-dirname))
      (desktop-read)))
  ;; does not work with 'after-init-hook
  (add-hook 'emacs-startup-hook #'adi/desktop-read-after-emacs-startup)

  (desktop-save-mode 1))

(use-package super-save ; h/t bbatsov/prelude
  :ensure t
  :config
  (super-save-mode +1)
  :blackout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage windows, buffers, movement, navigation and create a more
;; "heads up display" kind of experience.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package counsel ; brings in ivy, and swiper too
  :ensure t
  :config
  (ivy-mode +1)
  :bind
  (:map counsel-find-file-map
        ("C-l" . counsel-up-directory)
        :map global-map
        ("M-x" . counsel-M-x)
        ("C-c c" . counsel-company)
        ("C-c i" . counsel-imenu)
        ("C-x C-f" . counsel-find-file)
        ("C-c C-f" . counsel-recentf)
        ("C-c g" . counsel-git)         ; find files respecting gitignore
        ("C-c k" . counsel-ag)
        ("C-c l" . counsel-locate))
  :blackout)

(use-package swiper
  :ensure t
  :bind
  (:map
   global-map
   ("C-s" . swiper)
   ("C-c t" . swiper-thing-at-point) ; swiper-map prefix key is "C-c"
   ("C-c a t" . swiper-all-thing-at-point)))

(use-package ivy
  :ensure t
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d") ; per the docs
  :blackout)

(use-package ivy-rich ; h/t suvratapte/dot-emacs-dot-d
  :ensure t
  :custom
  (ivy-rich-path-style 'abbreviate)
  :config
  (ivy-rich-mode +1)
  :blackout)

(use-package ibuffer
  :bind (:map global-map
              ("C-x C-b" . ibuffer-other-window))
  :blackout)

(use-package which-key
  :ensure t
  :config (which-key-mode t)
  (setq which-key-idle-delay 0.5)
  ;; Sort based on the key description ignoring case (default
  ;; is 'which-key-key-order).
  (setq which-key-sort-order 'which-key-description-order)
  :blackout)

(use-package ace-window
  :ensure t
  :bind
  (:map global-map
        ("s-w" . 'ace-window)
        ([remap other-window] . 'ace-window)))

(use-package avy
  :ensure t)

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode +1)

  (setq key-chord-one-key-delay 0.2)           ; e.g. "jj", default 0.2
  (setq key-chord-two-keys-delay 0.2)          ; e.g. "jk", default 0.1
  (setq key-chord-safety-interval-backward 0.5) ; default 0.1 is too close to key delays
  (setq key-chord-safety-interval-forward 0) ; default 0.35 causes laggy experience

  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-define-global "jk" 'avy-goto-char)
  (key-chord-define-global "XX" 'execute-extended-command)
  (key-chord-define-global "yy" 'counsel-yank-pop)
  (key-chord-define-global "g;" 'ace-window)
  (key-chord-define-global ";g" 'ace-window)
  :blackout)

(use-package golden-ratio
  ;; https://github.com/roman/golden-ratio.el
  :ensure t
  :config
  (golden-ratio-mode +1)
  ;; Set auto scale to `t' for wide screens, but not for 16:10, 1900px HD displays
  (setq golden-ratio-auto-scale nil
        ;; golden-ratio-wide-adjust-factor 1
        ;; golden-ratio-adjust-factor .8
        )
  ;; Make golden ratio play nice with other modes
  (dolist (cmd '(ace-window
                 magit-status
                 avy-goto-char
                 avy-goto-char-2
                 avy-goto-word-0
                 avy-goto-word-1
                 avy-goto-line))
    (add-to-list 'golden-ratio-extra-commands
                 cmd))
  :blackout)

(use-package rotate
  ;; https://github.com/daichirata/emacs-rotate
  :ensure t
  :blackout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General text viewing and editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable narrowings to enhance focus, and reduce accidental
;; edits of nonfocus areas (thanks to save-restrictions).
;; h/t bbatsov/prelude
;; Note: `C-x n w` makes all visible again.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; COMplete ANYthing, please!
;; TODO: instead try radian's completion system
;;; ref: https://github.com/radian-software/radian
(use-package company
  :bind (:map global-map
              ("TAB" . company-indent-or-complete-common)
              ;; Got to love the name hippie-expand. Use this for general
              ;; expansions, because company-complete is good for the
              ;; current/narrow-case expansions. By default, `M-/` binds
              ;; to the less powerful `dabbrev-expand`. To alter search
              ;; options, :config the hippie-expand-try-functions-list.
              ("M-/" . hippie-expand))
  :config (setq company-idle-delay 0.1
                company-minimum-prefix-length 2)
  (global-company-mode t)
  :blackout)

;; Selections
(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)
   ("C-M-=" . er/contract-region)))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :config
  ;; Idea taken from "Emacs: Define Key Sequence"
  ;; ref: http://ergoemacs.org/emacs/emacs_keybinding_power_of_keys_sequence.html
  ;; define prefix keymap for multiple cursors
  (define-prefix-command 'adi/multi-cursor-keymap)
  (define-key adi/multi-cursor-keymap (kbd "e") 'mc/edit-lines)
  (define-key adi/multi-cursor-keymap (kbd "a") 'mc/mark-all-like-this-dwim)
  (define-key adi/multi-cursor-keymap (kbd "r") 'mc/mark-all-in-region-regexp)
  (define-key adi/multi-cursor-keymap (kbd "s") 'mc/mark-all-symbols-like-this-in-defun)
  (define-key adi/multi-cursor-keymap (kbd "w") 'mc/mark-all-words-like-this-in-defun)
  (define-key adi/multi-cursor-keymap (kbd "C-n") 'mc/mark-next-like-this)
  (define-key adi/multi-cursor-keymap (kbd "C-p") 'mc/mark-previous-like-this)
  (define-key adi/multi-cursor-keymap (kbd "C-a") 'mc/mark-all-like-this)
  :bind-keymap ("C-c m" . adi/multi-cursor-keymap))

(use-package wgrep ; editable grep buffers FTW!
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lispy editing support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tweak settings of built-in paren package
(use-package paren
  :ensure nil                    ; it already exists, don't try to search online
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode t)
  :blackout)

(use-package paredit
  ;; Handy configs available at the wiki https://www.emacswiki.org/emacs/ParEdit
  ;; including combining with eldoc, "electric" enabled modes etc.
  :ensure t
  :bind
  (("M-[" . paredit-wrap-square)
   ("M-{" . paredit-wrap-curly))
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode)
         . paredit-mode)
  :blackout)

(use-package eldoc
  :ensure t
  :config
  (global-eldoc-mode t)
  :custom ; h/t jwiegley/dot-emacs
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-echo-area-display-truncation-message nil)
  ;; for hooks, ref: https://www.emacswiki.org/emacs/ElDoc
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode)
         . eldoc-mode)
  :blackout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming and Writing workflow support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :blackout)

(use-package projectile
  :ensure t
  :blackout)

(use-package yasnippet
  :ensure t
  :config
  (defvar dotemacs-yasnippets-dir
    (file-name-as-directory (expand-file-name "snippets" dotemacs-dir)))
  (unless (file-exists-p dotemacs-yasnippets-dir)
    (make-directory dotemacs-yasnippets-dir))

  (setq yas-snippet-dirs
        '(dotemacs-yasnippets-dir))

  (add-to-list 'hippie-expand-try-functions-list
               'yas-hippie-try-expand)

  (yas-global-mode +1)
  :blackout yas-minor-mode)

(use-package flyspell
  :config (flyspell-mode +1))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode +1)
  :blackout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clojure-mode
  :ensure t
  :blackout)

(use-package cider
  :ensure t
  :blackout)

(provide 'init)
;;; init.el ends here
