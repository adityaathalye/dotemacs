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
(defvar adi/dotemacs-min-version "28.1")

(when (version< emacs-version adi/dotemacs-min-version)
  (error "We demand spiffy new Emacs, at least v%s, but you have v%s"
         adi/dotemacs-min-version
         emacs-version))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIRECTORY STRUCTURE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Take clues from bbatsov/prelude, except keep structure relative to our
;; initial adi/dotemacs-dir path. This way we can start the user's emacs via
;; ~/.emacs.d symlinked to the dotemacs repo, and develop/debug against
;; the repo without potentially overwriting transient state files of the
;; daily driver .emacs.d.

(defvar adi/dotemacs-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The dotemacs' root.  Normally it should be ~/.emacs.d.")

(defvar adi/dotemacs-custom-file (expand-file-name "custom.el" adi/dotemacs-dir)
  "Make Emacs add customisations here, instead of the init file.
Usually customisations made from the UI go into `custom-file'.")
(setq custom-file adi/dotemacs-custom-file)
(unless (file-exists-p adi/dotemacs-custom-file)
  (make-empty-file adi/dotemacs-custom-file))
(load-file custom-file) ; load *now*, instead of unpredictable load sequence

(defvar adi/dotemacs-savefile-dir (file-name-as-directory
                               (expand-file-name "savefile" adi/dotemacs-dir))
  "This folder stores all the automatically generated save/history-files.")
(unless (file-exists-p adi/dotemacs-savefile-dir)
  (make-directory adi/dotemacs-savefile-dir))

(defvar adi/dotemacs-cache-dir (file-name-as-directory
                                (expand-file-name ".cache" adi/dotemacs-dir))
  "Store things like lsp servers that lsp-mode can auto-download.")
(unless (file-exists-p adi/dotemacs-cache-dir)
  (make-directory adi/dotemacs-cache-dir))

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
 ;; LSP-mode performance tweaks
 read-process-output-max (* 1024 1024) ; 1mb

 ;; INTERACTIONS
 inhibit-startup-message t
 ring-bell-function 'ignore ; no beeps
 require-final-newline t ; always well-form files
 confirm-kill-emacs 'y-or-n-p ; instead of disabling 'C-x C-c'
 use-dialog-box nil ; no popups please
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
;; Set package download directory relative to the adi/dotemacs-dir
(setq package-user-dir (file-name-as-directory
                        (expand-file-name "elpa" adi/dotemacs-dir)))

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

(setq use-package-always-ensure t
      use-package-expand-minimally t ; set nil to debug use-package forms
      use-package-verbose t)

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

(use-package command-log-mode
  :ensure t
  :blackout)

(use-package esup
  ;; emacs startup profiler
  :blackout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remember states of files, buffer, desktop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package recentf
  :ensure t
  :config
  (setq recentf-save-file (expand-file-name "recentf"
                                            adi/dotemacs-savefile-dir)
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
                                        adi/dotemacs-savefile-dir)))

(use-package saveplace
  :config
  (setq save-place-file
        (expand-file-name "saveplace" adi/dotemacs-savefile-dir))
  (save-place-mode +1))

(use-package desktop
  ;; Restore Emacs buffers, windows, placement to how it was at shutdown
  ;; ref: https://www.emacswiki.org/emacs/Desktop
  :init
  ;; Reduce startup slowdown due to side effects like Language Servers being
  ;; started synchronously due to eager restores of programming buffers.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
  (setq desktop-restore-eager 1)
  :config
  (add-to-list 'desktop-path adi/dotemacs-savefile-dir)
  (setq desktop-dirname adi/dotemacs-savefile-dir
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

(use-package amx ; h/t Protesilaos Stavrou
  ;; NOTES: When amx is active:
  ;; - "C-h f" calls describe-function
  ;; - "M-." jumps to definition
  ;; - "C-h w" shows keybindings for thing at point
  :ensure t
  :config
  (setq amx-save-file
        (expand-file-name "amx-items" adi/dotemacs-savefile-dir))
  (setq amx-backend 'ivy) ; integrates with counsel-M-x
  (setq amx-show-key-bindings t) ; t by default
  ;; (add-to-list 'amx-ignored-command-matchers "") ; to ignore commands
  (amx-mode +1)
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
        ;; because counsel find file does not let us edit results
        ("C-M-y" . ivy-insert-current-full)
        ("C-c C-f" . counsel-recentf)
        ("C-c g" . counsel-git)         ; find files respecting gitignore
        ("C-c k" . counsel-ag)
        ("C-c l" . counsel-locate)
        ("C-c b" . counsel-switch-buffer-other-window))
  :blackout)

(use-package swiper
  :ensure t
  :bind
  (:map
   global-map
   ("C-s" . swiper)
   ("C-c t" . swiper-thing-at-point) ; swiper-map prefix key is "C-c"
   ("C-c a t" . swiper-all-thing-at-point)))

;; Note: Use "M-o" after "C-x C-f" or "C-s" for additional options
;; for the context for the thing selected in the minibuffer.
(use-package ivy
  :ensure t
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d: " ; per the docs
        ;; don't start ivy prompts with '^' regex match,
        ;; because we want fuzzier matching by default
        ivy-initial-inputs-alist nil)
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
  ;; See also slightly modified version h/t github.com/nebkor
  ;; https://git.sr.ht/~wklew/golden
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
  :config (setq company-idle-delay 0.3
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

(use-package smartparens               ; h/t bbatsov/prelude use it like paredit
  :ensure t
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          ielm-mode
          clojure-mode
          cider-repl-mode) . smartparens-strict-mode)
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
  (defvar adi/dotemacs-yasnippets-dir
    (file-name-as-directory (expand-file-name "snippets" adi/dotemacs-dir)))
  (unless (file-exists-p adi/dotemacs-yasnippets-dir)
    (make-directory adi/dotemacs-yasnippets-dir))

  (setq yas-snippet-dirs
        (list adi/dotemacs-yasnippets-dir))

  (add-to-list 'hippie-expand-try-functions-list
               'yas-hippie-try-expand)

  (yas-global-mode +1) ; nb. global mode can slow down emacs startup
  :blackout yas-minor-mode)

(use-package yasnippet-snippets
  :ensure t
  :requires yasnippet
  :config
  (yasnippet-snippets-initialize)
  :blackout)

(use-package flyspell
  ;; "C-," to goto next error,
  ;; "C-M-i" to auto-fix current error (cycles options)
  :ensure nil ; built in
  :hook ((text-mode org-mode) . flyspell-mode)
  :hook ((prog-mode) . flyspell-prog-mode)
  :blackout)

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode +1)
  :blackout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  ;; ref: https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  :hook ((clojure-mode clojurescript-mode clojurec-mode) . lsp-deferred)
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :custom
  ;; LSP "workspace" dirs:
  ;; nb. "workspace" seems to be a confusing concept. It is a VSCode concept
  ;; that lsp-mode superseded as "session", because lsp-mode was already using
  ;; the word "workspace" in some other context. See: `lsp-describe-session'.
  ;; https://github.com/emacs-lsp/lsp-mode/discussions/3095
  ;; "workspace" directories still seem to server some purpose (no idea what),
  ;; and seem to be language specific.
  (lsp-clojure-workspace-dir
   (file-name-as-directory (expand-file-name "workspace"
                                             adi/dotemacs-dir)))
  :config
  (setq lsp-server-install-dir (file-name-as-directory ; install local to dotemacs
                                (expand-file-name "lsp" adi/dotemacs-cache-dir))
        ;; Perf. tweaks. Ref: https://emacs-lsp.github.io/lsp-mode/page/performance/
        lsp-idle-delay 0.500 ; bump higher if lsp-mode gets sluggish
        lsp-log-io nil
        ; lsp-enable-indentation nil ; set 'nil' to use cider indentation instead of lsp
        ; lsp-enable-completion-at-point nil; set 'nil' to use cider completion instead of lsp

        ;; No semgrep. https://emacs-lsp.github.io/lsp-mode/page/lsp-semgrep/
        ;; IDK why semgrep is on by default, docs are thin on configuring it
        ;; I don't want the error 'Command "semgrep lsp" is not present on the path.'
        ;; because I don't want to "pip install semgrep --user".
        lsp-semgrep-server-command nil)
  ;; LANGUAGE SPECIFIC SETTINGS
  ;; clojure-lsp: cf. https://clojure-lsp.io/clients/#emacs
  (add-to-list 'lsp-language-id-configuration
                `(clojurex-mode . "clojure"))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map ; h/t github.com/bbatsov/prelude
              ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . #'lsp-ui-peek-find-references)
              ("C-c C-l ." . 'lsp-ui-peek-find-definitions)
              ("C-c C-l ?" . 'lsp-ui-peek-find-references)
              ("C-c C-l r" . 'lsp-rename)
              ("C-c C-l x" . 'lsp-workspace-restart)
              ("C-c C-l w" . 'lsp-ui-peek-find-workspace-symbol)
              ("C-c C-l i" . 'lsp-ui-peek-find-implementation)
              ("C-c C-l d" . 'lsp-describe-thing-at-point)
              ("C-c C-l e" . 'lsp-execute-code-action))
  :config
  ; h/t github.com/bbatsov/prelude
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable t
        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t))

(use-package lsp-ivy
  :ensure t
  :after lsp-mode
  :commands lsp-ivy-workspace-symbol)

;; treemacs is cool, but I'm not sure I want it yet.
;; cf: https://github.com/emacs-lsp/lsp-treemacs
;; and https://github.com/Alexander-Miller/treemacs
;; (use-package lsp-treemacs
;;   :ensure t
;;   :after lsp-mode
;;   :commands lsp-treemacs-errors-list
;;   :config
;;   (setq treemacs-space-between-root-nodes nil))

;; dap-mode, optionally to use LANGUAGE-specific debuggers
;; cf. https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE :ensue t :after dap-mode) ; load dap adapter for LANGUAGE

(use-package clojure-mode
  ;; Brings in clojure-mode for Clj, clojurescript-mode for Cljs,
  ;; and clojurec-mode for Cljc
  :ensure t
  ;; Hook into subword-mode to work with CamelCase tokens like Java classes
  ;; h/t suvratapte/dot-emacs-dot-d
  :hook ((clojure-mode . subword-mode)
         (clojure-mode . yas-minor-mode))

  :config
  (setq clojure-indent-style 'align-arguments)
  :blackout "Clj")

(use-package cider
  ;; Note: Ensure CIDER and lsp-mode play well together, as we use both.
  ;; - LSP for more static-analysis-y services (completions, lookups, errors etc.),
  ;; - CIDER for "live" runtime services (enhanced REPL, interactive debugger etc.).
  :ensure t
  :after clojure-mode
  :init
  ;; Use clojure-lsp for eldoc and completions
  ;; h/t cider docs and ericdallo/dotfiles/.config/doom/config.el
  (remove-hook 'eldoc-documentation-functions #'cider-eldoc)
  (remove-hook 'completion-at-point-functions #'cider-complete-at-point)
  :config
  ;; settings h/t suvratapte/dot-emacs-dot-d
  (setq cider-repl-pop-to-buffer-on-connect t
        cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-repl-history-file (expand-file-name "cider-history"
                                                  adi/dotemacs-savefile-dir)
        cider-repl-wrap-history t
        cider-prompt-for-symbol nil
        cider-repl-use-pretty-printing t
        nrepl-log-messages nil
        ;; play nice with lsp-mode
        ;; h/t ericdallo/dotfiles/.config/doom/config.el
        cider-font-lock-dynamically nil ; use lsp semantic tokens
        cider-eldoc-display-for-symbol-at-point nil ; use lsp
        cider-prompt-for-symbol nil ; use lsp
        cider-use-xref nil ; use lsp
        )
  :blackout)

(provide 'init)
;;; init.el ends here
