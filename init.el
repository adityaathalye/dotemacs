;;; init.el --- My Emacs configuration.

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
(defvar dotemacs-dir (file-name-directory load-file-name)
  "The dotemacs' root.")
(defvar dotemacs-savefile-dir (expand-file-name "savefile" dotemacs-dir)
  "This folder stores all the automatically generated save/history-files.")
(unless (file-exists-p dotemacs-savefile-dir)
  (make-directory dotemacs-savefile-dir))

;; Make emacs add customisations here, instead of the init file.
;; Usually customisations made from the UI go into custom-file.
(setq custom-file (expand-file-name "custom.el" dotemacs-dir))
(unless (file-exists-p custom-file)
  (make-empty-file custom-file))

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
(setq package-user-dir (expand-file-name "elpa" dotemacs-dir))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


;; Use use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


(provide 'init)
;;; init.el ends here
