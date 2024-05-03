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

;; TODO: Evaluate the `no-littering' package.
;; It aims to "Help keeping ~/.config/emacs clean.".
;; cf. https://github.com/emacscollective/no-littering

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

(defvar adi/dotemacs-custom-file-private (expand-file-name "private-custom.el" adi/dotemacs-dir)
  "A file for private customisations. TODO: create a private package to load out-of-band.")
(unless (file-exists-p adi/dotemacs-custom-file-private)
  (make-empty-file adi/dotemacs-custom-file-private))
(load-file adi/dotemacs-custom-file-private) ; load *now*, instead of unpredictable load sequence

(defvar adi/dotemacs-savefile-dir (file-name-as-directory
                               (expand-file-name "savefile" adi/dotemacs-dir))
  "This folder stores all the automatically generated save/history-files.")
(unless (file-exists-p adi/dotemacs-savefile-dir)
  (make-directory adi/dotemacs-savefile-dir))

(defvar adi/dotemacs-backup-files-dir (file-name-as-directory
                                     (expand-file-name "autosaves"
                                                       adi/dotemacs-savefile-dir))
  "Store backup file copies Emacs makes when editing files,
and for auto-saves we can restore from.")
(unless (file-exists-p adi/dotemacs-backup-files-dir)
  (make-directory adi/dotemacs-backup-files-dir))

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
              ;; to hard wrap, use fill-paragraph
              fill-column 70
              global-display-fill-column-indicator-mode t)

(setq
 ;; PERFORMANCE

 ;; gc ~100MB for better overall performance. ~50-100MB is recommended
 ;; these days over the long-obsolete default of ~8MB.
 ;;   TODO: evaluate the Garbage Collection Magic Hack (GCMH) to
 ;;   - Enforce a sneaky Garbage Collection strategy to minimize GC
 ;;     interference with user activity.
 ;;   - If this is done at the beginning of your .emacs start-up time
 ;;     should also benefit from it.
 ;;   cf. https://gitlab.com/koral/gcmh, https://akrl.sdf.org/#orgc9536b4,
 ;;   via HN: https://news.ycombinator.com/item?id=39190110
 gc-cons-threshold (* 100 1024 1024)

 ;; Large files freeze Emacs. Warn for files over ~100MB.
 large-file-warning-threshold (* 100 1024 1024)
 ;; Always load newest byte code. cf. bbatsov/prelude
 load-prefer-newer t
 ;; LSP-mode performance tweaks
 read-process-output-max (* 1024 1024) ; 1mb

 ;; FILE BACKUP RESTORE
 ;; cf. https://www.emacswiki.org/emacs/BackupDirectory
 ;; and https://www.emacswiki.org/emacs/BackupFiles
 ;; and https://www.emacswiki.org/emacs/AutoSave
 ;; To also configure local backups for tramp,
 ;; use `tramp-backup-directory-alist'
 backup-by-copying t ; don't clobber symlinks
 backup-directory-alist ; put backup files here, e.g. `file~' and `#file#' copies
 `(("." . ,adi/dotemacs-backup-files-dir))

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

;; CREATURE COMFORTS

;; Tracking world time with Emacs
;; https://emacsredux.com/blog/2024/03/11/tracking-world-time-with-emacs/
;; Zone format: "Country/Region" "Label"
(setq world-clock-list adi/world-clock-list
      world-clock-time-format "%a %d %b %R %Z")

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO: use `straight-use-package' for great good.
;; cf. https://github.com/radian-software/straight.el?tab=readme-ov-file#integration-with-use-package
;;
;; To use use-package, first install it with straight.el:
;;
;;   (straight-use-package 'use-package)
;;
;; Now use-package will use straight.el to automatically install missing packages if you provide :straight t:
;;
;;   (use-package el-patch
;;     :straight t)
;;
;; You can still provide a custom recipe for the package... etc. etc.
;;

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

;; PATH Setting
;;
;; Make the environment variables inside Emacs look the same as my shell
;; ref: https://github.com/purcell/exec-path-from-shell
;; e.g. LSP server installation requires nvm path that is not available
;; in PATH if we start Emacs from the GUI.
(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))
(require 'exec-path-from-shell)
;; Set list of vars to look up from shell env. Do so _before_ initialising.
(dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
  (add-to-list 'exec-path-from-shell-variables var))
;; Set $MANPATH, $PATH and exec-path from  shell, when executed in a GUI
;; frame on OS X and Linux.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))

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

(use-package gnu-elpa-keyring-update
  ;; To keep package verification signatures up to date.
  ;; ref: https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
  ;;
  ;; If our keys are already too old, causing signature verification
  ;; errors when installing packages, then in order to install this
  ;; package we have to temporarily disable signature verification
  ;; (see variable `package-check-signature') :-(
  ;;
  ;; If keys have expired, we get an error like this:
  ;;
  ;;   Failed to verify signature archive-contents.sig:
  ;;   No public key for 645357D2883A0966 created at 2024-05-03T02:35:03+0530 using EDDSA
  ;;   Command output:
  ;;   gpg: Signature made Friday 03 May 2024 02:35:03 AM IST
  ;;   gpg:                using EDDSA key 0327BE68D64D9A1A66859F15645357D2883A0966
  ;;   gpg: Can't check signature: No public key
  ;;
  ;; To fix it:
  ;; ref: https://emacs.stackexchange.com/a/53142
  ;;
  ;; - Unset package-check-signature
  ;; (setq package-check-signature--old package-check-signature
  ;;       package-check-signature nil)
  ;;
  ;; - Install this check signature package
  ;;
  ;; - Set back package-check-signature to former value.
  ;; (setq package-check-signature package-check-signature--old)
  )

;; radian-software/blackout to tune major/minor mode names
;; in modeline. It unifies functionality of the mutually
;; confusing modeline lighters delight, diminish, and dim.
;; It integrates seamlessly with use-package.
(use-package blackout)

(use-package helpful ; h/t systemcrafters.net
  ;; https://github.com/Wilfred/helpful
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
  :blackout)

(use-package esup
  ;; emacs startup profiler
  :blackout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remember states of files, buffer, desktop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package recentf
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
  :config
  (super-save-mode +1)
  :blackout)

(use-package amx ; h/t Protesilaos Stavrou
  ;; NOTES: When amx is active:
  ;; - "C-h f" calls describe-function
  ;; - "M-." jumps to definition
  ;; - "C-h w" shows keybindings for thing at point
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
  :bind
  (:map
   global-map
   ("C-s" . swiper)
   ("C-c t" . swiper-thing-at-point) ; swiper-map prefix key is "C-c"
   ("C-c a t" . swiper-all-thing-at-point)))

;; Note: Use "M-o" after "C-x C-f" or "C-s" for additional options
;; for the context for the thing selected in the minibuffer.
(use-package ivy
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d: " ; per the docs
        ;; don't start ivy prompts with '^' regex match,
        ;; because we want fuzzier matching by default
        ivy-initial-inputs-alist nil
        ;; fix annoying problem of ivy not letting us rename file
        ;; that has partial completion
        ;; https://www.reddit.com/r/emacs/comments/e02lup/ivy_swiper_doesnt_let_me_rename_or_save_a_file/
        ivy-use-selectable-prompt t)
  :blackout)

(use-package ivy-rich ; h/t suvratapte/dot-emacs-dot-d
  :custom
  (ivy-rich-path-style 'abbreviate)
  :config
  (ivy-rich-mode +1)
  ;; perf. Caching:
  ;; ref: https://github.com/Yevgnen/ivy-rich#project-performance
  ;; ref: https://github.com/syl20bnr/spacemacs/issues/10101
  ;; ivy-rich slows down a lot if many buffers are open
  ;; `M-x profiler-start' revealed lots of busy work by ivy-rich.
  ;; This is super annoying, but the workaround is to add a cache.
  (ivy-rich-project-root-cache-mode +1)
  :blackout)

(use-package ibuffer
  :bind (:map global-map
              ("C-x C-b" . ibuffer-other-window))
  :blackout)

(use-package which-key
  :config (which-key-mode t)
  (setq which-key-idle-delay 0.5)
  ;; Sort based on the key description ignoring case (default
  ;; is 'which-key-key-order).
  (setq which-key-sort-order 'which-key-description-order)
  :blackout)

(use-package ace-window
  :bind
  (:map global-map
        ("s-w" . 'ace-window)
        ([remap other-window] . 'ace-window))
  :blackout)

(use-package avy
  :blackout)

(use-package key-chord
  :config
  (key-chord-mode +1)

  (setq key-chord-one-key-delay 0.2)           ; e.g. "jj", default 0.2
  (setq key-chord-two-keys-delay 0.2)          ; e.g. "jk", default 0.1
  (setq key-chord-safety-interval-backward 0.5) ; default 0.1 is too close to key delays
  (setq key-chord-safety-interval-forward 0) ; default 0.35 causes laggy experience

  (key-chord-unset-global "lj") ; we often want to type clj
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
  :bind
  (("C-=" . er/expand-region)
   ("C-M-=" . er/contract-region)))

;; Multiple cursors
(use-package multiple-cursors
  :config
  (setq mc/list-file (expand-file-name ".mc-lists.el"
                                       adi/dotemacs-savefile-dir))
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
  :blackout)

(use-package separedit
  ;; Edit comment or string/docstring or code block inside them in
  ;; separate buffer with your favorite mode. h/t Ag Ibragimov via
  ;; Clojurians Slack. Ag notes separedit uses indirect buffers to
  ;; work its magic. cf: https://github.com/twlz0ne/separedit.el
  ;; --
  ;; Ag: I use separadit to edit multiline text like:
  ;;   (str "foo"
  ;;        "bar"
  ;;        "zap"
  ;;        "zop")
  ;; it will let me edit the whole thing in a separate buffer, showing
  ;; only the string content and when I'm done, it will put the
  ;; modified text back into (str) block. It feels like pure magic.
  ;; --
  ;; I used it as an example use of indirect buffers.
  ;; Try this.
  ;; Open a namespace file (or any buffer, really)
  ;; - Clone indirect buffer M-x clone-indirect-buffer
  ;; - You'd see another cloned buffer
  ;; - Now, you can use narrow-to-defun, narrow-to-region commands
  ;;   in one buffer without affecting the other.
  ;; - Meanwhile editing text would equally be synchronized in both of them.
  :bind
  (:map global-map
        ;; "C-u C-c '" to edit with manually selected major mode
        ("C-c '" . #'separedit))
  :config
  ;; Feature options
  (setq separedit-preserve-string-indentation t
        separedit-continue-fill-column t
        separedit-write-file-when-execute-save t
        separedit-remove-trailing-spaces-in-comment t)

  :ensure t)

(use-package crux
  ;; cf. https://github.com/bbatsov/crux
  :bind
  (:map global-map
        ("C-M-z" . crux-indent-defun) ; indent defun at point
        ("C-c r" . crux-rename-file-and-buffer)
        ("C-c D" . crux-delete-file-and-buffer))
  :blackout)

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
  :blackout)

(use-package projectile
  :custom
  (projectile-cache-file (expand-file-name "projectile.cache"
                                           adi/dotemacs-cache-dir))
  :init
  (projectile-mode +1)
  :config
  (setq projectile-indexing-method 'alien ; default is 'alien
        ;; Enable caching unconditionally for all indexing modes
        projectile-enable-caching t
        ;; cf. https://docs.projectile.mx/projectile/configuration.html#caching
        ;; To manually invalidate cache:
        ;; - 'C-u C-c p f', prior to prompting for a file to jump to
        ;; - M-x projectile-purge-file-from-cache
        ;; - M-x projectile-purge-dir-from-cache

        ;; Set search paths string explicitly to re-index all known projects.
        ;; Optionally use cons cell to declare recursive search depth
        ;; '(("~/src" . 3) ("~/work/" . 4))
        projectile-project-search-path nil
        ;; make C-c p t also create a test file when missing
        projectile-create-missing-test-files t)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  :blackout)

(use-package yasnippet
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
  :config (global-flycheck-mode +1)
  :blackout)

(use-package zeal-at-point
  ;; ref: https://github.com/jinzhu/zeal-at-point
  :bind (:map global-map
              ("\C-c z" . 'zeal-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  ;; ref: https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  :hook ((clojure-mode clojurescript-mode clojurec-mode sh-mode) . lsp-deferred)
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
  :after lsp-mode
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map ; h/t github.com/bbatsov/prelude
              ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . #'lsp-ui-peek-find-references)
              ("C-c C-l ." . 'lsp-ui-peek-find-definitions)
              ("C-c C-l ," . 'lsp-ui-peek-find-references)
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
        lsp-ui-peek-always-show t
        lsp-ui-imenu-auto-refresh 'after-save
        ;; fix width to prevent golden ratio mode from
        ;; re-balancing the imenu window
        lsp-ui-imenu-window-fix-width t
        lsp-ui-imenu-window-width 40))

(use-package lsp-ivy
  :after lsp-mode
  :commands lsp-ivy-workspace-symbol)

;; treemacs is pretty useful for polylith-like multi-projects
;; cf: https://github.com/emacs-lsp/lsp-treemacs
;; and https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window)
        ("C-c C-t 1"   . treemacs-delete-other-windows)
        ("C-c C-t t"   . treemacs)
        ("C-c C-t d"   . treemacs-select-directory)
        ("C-c C-t B"   . treemacs-bookmark)
        ("C-c C-t C-t" . treemacs-find-file)
        ("C-c C-t M-t" . treemacs-find-tag))
  :config
  (setq treemacs-file-event-delay 500 ; default is 2000
        treemacs-file-follow-delay 0.1 ; default is 0.2
        ;; Make treemacs automatically switch to the project for the
        ;; current buffer. Without this, one has to remember which project
        ;; belongs to which workspace.
        treemacs-follow-mode t
        treemacs-find-workspace-method 'find-for-file-or-manually-select
        treemacs-project-follow-mode t
        treemacs-project-follow-cleanup t ; expand only current project
        treemacs-display-in-side-window t
        ;; treemacs--process-file-events produces errors in case of
        ;; transient files, even gitignored ones. Hiding gitignored files
        ;; appears to work around the timer error.
        ;; https://github.com/Alexander-Miller/treemacs/issues/749#issuecomment-1962363871
        treemacs-hide-gitignored-files-mode t)
  :blackout)

(use-package lsp-treemacs
  :after lsp-mode
  :commands lsp-treemacs-errors-list
  :config
  (setq treemacs-space-between-root-nodes nil
        lsp-treemacs-sync-mode +1)
  :blackout)

(use-package treemacs-projectile
  :after (treemacs projectile))

;; dap-mode, optionally to use LANGUAGE-specific debuggers
;; cf. https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE :ensue t :after dap-mode) ; load dap adapter for LANGUAGE

(use-package clojure-mode
  ;; Brings in clojure-mode for Clj, clojurescript-mode for Cljs,
  ;; and clojurec-mode for Cljc
  ;; Hook into subword-mode to work with CamelCase tokens like Java classes
  ;; h/t suvratapte/dot-emacs-dot-d
  :hook ((clojure-mode . subword-mode)
         (clojure-mode . yas-minor-mode))

  :config
  (setq clojure-indent-style 'always-align)
  :blackout "Clj")

(use-package cider
  ;; Note: Ensure CIDER and lsp-mode play well together, as we use both.
  ;; - LSP for more static-analysis-y services (completions, lookups, errors etc.),
  ;; - CIDER for "live" runtime services (enhanced REPL, interactive debugger etc.).
  :after clojure-mode
  :init
  ;; Use clojure-lsp for eldoc and completions
  ;; h/t cider docs and ericdallo/dotfiles/.config/doom/config.el
  (remove-hook 'eldoc-documentation-functions #'cider-eldoc)
  (remove-hook 'completion-at-point-functions #'cider-complete-at-point)
  :custom
  (cider-preferred-build-tool 'clj)
  :bind
  (:map cider-mode-map
        ("C-c C-l" . nil))
  :config
  ;; settings h/t suvratapte/dot-emacs-dot-d
  (setq cider-repl-pop-to-buffer-on-connect nil
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
        ;; Maybe customize variables for cider-jack-in
        ;; https://docs.cider.mx/cider/basics/up_and_running.html
        )
  :blackout)

;; clj-refactor can go where clojure-lsp refactor can't go
(use-package clj-refactor
  ;; config h/t ericdallo/dotfiles doom emacs config
  :after clojure-mode
  :config
  (setq cljr-warn-on-eval nil
        cljr-eagerly-build-asts-on-startup nil
        cljr-add-ns-to-blank-clj-files nil ; use lsp
        cljr-magic-require-namespaces
        '(("s"   . "schema.core")
          ("pp" . "clojure.pprint"))))

(use-package clj-deps-new
  ;; Emacs interface to deps-new (like lein new for Leiningen, or clj-new for Boot)
  ;; ref: https://github.com/jpe90/emacs-clj-deps-new
  :blackout)

;; Javascript / Typescript development
;; Use the `ts-ls' server recommended in the official lsp-mode documentation
;; cf. https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/
(use-package typescript-mode
  ;; NOTE: development has halted for this mode, because of how
  ;; complex TS and TSX parsing has become. They recommend using treesitter
  ;; powered `typescript-ts-mode' that is built into Emacs 29.
  ;; h/t systemcrafters
  ;; https://systemcrafters.net/emacs-from-scratch/build-your-own-ide-with-lsp-mode/#typescript
  ;; ALTERNATIVE:
  ;; If this doesn't work well, install tree-sitter and ts/tsx support
  ;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; DATABASES

(use-package sqlformat
  ;; https://github.com/purcell/sqlformat
  ;; with sqlfluff (pip installed for user) https://github.com/sqlfluff/sqlfluff
  :hook (sql-mode . sqlformat-on-save-mode)
  :config
  (setq sqlformat-command 'sqlfluff)
  (setq sqlformat-args '("--dialect" "postgres"))
  :blackout)

;; WEB DEVELOPMENT

(use-package web-mode
  :config
  ;; for config ref: bbatsov/prelude/modules/prelude-web.el
  ;; e.g. to set web-mode for templating languages,
  ;;      to define new smartparen pairs etc.
  (setq web-mode-enable-auto-pairing nil) ; play nice with smartparens
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :blackout)

(use-package json-mode
  :blackout)

(use-package js2-mode
  :blackout)

;; DOCUMENTATION FORMATS

(use-package adoc-mode
  :blackout)

;; INFORMATION OVERDOSING

(use-package elfeed
  ;; https://github.com/skeeto/elfeed
  ;; To search feeds, open elfeed search view, hit `s' and type a query
  ;; in the minibuffer. For example:
  ;;
  ;;   @6-months-ago +unread
  ;; Only show unread entries of the last six months. This is the default filter.
  ;;
  ;;   linu[xs] @1-year-old
  ;; Only show entries about Linux or Linus from the last year.
  ;;
  ;;   -unread +youtube #10
  ;; Only show the most recent 10 previously-read entries tagged as youtube.
  ;;
  ;;   +unread !x?emacs
  ;; Only show unread entries not having emacs or xemacs in the title or link.
  ;;
  ;;  +emacs =http://example.org/feed/
  ;; Only show entries tagged as emacs from a specific feed.
  ;;
  :bind
  (:map global-map
        ("C-x w" . elfeed))
  :config
  (setq elfeed-feeds adi/elfeed-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Mode specials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable org babel languages
;; ref: https://orgmode.org/manual/Languages.html#Languages
;; and: https://orgmode.org/worg/org-contrib/babel/languages.html
;;
;; NOTE: If org-babel appears to be broken, it may be an org 9+ compile issue.
;; If so, purging org mode .elc files helps.
;; ref: "Org-Mode Evaluation of code disabled #7641"
;;      https://github.com/syl20bnr/spacemacs/issues/7641#issuecomment-259701129
;; and: "Org-mode 9: unable to eval code-blocks"
;;      https://emacs.stackexchange.com/a/28448
;; Purge this way:
;; - shut down emacs
;; - find ~/.emacs.d/elpa/org-9.* -name "*elc" # ensure only orgmode files found
;; - find ~/.emacs.d/elpa/org-9.* -name "*elc" -delete # re-execute, but with delete
;; - start emacs
;; - profit!

(use-package ob-http
  :blackout)

(use-package ob-sql-mode
  :blackout)

(use-package ob-clojurescript
  :blackout)

(use-package ob-kotlin
  :blackout)

(use-package org
  :ensure nil
  :config
  (setq org-export-coding-system 'utf-8
        org-babel-clojure-backend 'cider)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (awk . t)
     (sed . t)
     (http . t)
     (clojure . t)
     (clojurescript . t)
     (python . t)
     (kotlin . t)
     (groovy . t)
     (sql .t )
     (sqlite . t)
     (plantuml . t))))

(use-package ox-asciidoc
  ;; Org export backend for org to asciidoc
  :blackout)

(use-package org-tree-slide
  ;; Simple org outline based presentation mode
  ;; ref: https://github.com/takaxp/org-tree-slide
  :bind (("<f8>" . 'org-tree-slide-mode)
         ("S-<f8>" . 'org-tree-slide-skip-done-toggle)
         :map org-tree-slide-mode-map
         ("<f9>" . 'org-tree-slide-move-previous-tree)
         ("<f10>" . 'org-tree-slide-move-next-tree)
         ("<f11>" . 'org-tree-slide-content))
  :config
  (setq org-tree-slide-skip-outline-level 4))

(provide 'init)
;;; init.el ends here
