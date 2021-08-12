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


(provide 'init)
;;; init.el ends here
