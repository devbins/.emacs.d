;;; init.el --- -*- lexical-binding: t no-byte-compile: t; -*-
;;
;; Filename: init.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 18:54:11 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 109
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Speed up startup
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(advice-add #'package-initialize :after #'update-load-path)

(update-load-path)

(setq debug-on-error t)

;; Constants
(require 'init-const)

;; Functions
(require 'init-func)

;; Customization
(require 'init-custom)

;; Packages
(require 'init-package)

(require 'init-basic)
(require 'init-env)
(require 'init-hydra)
(require 'init-evil)

(require 'init-ui)
(require 'init-edit)
(require 'init-vertico)
(unless (eq my-lsp 'lsp-bridge)
  (require 'init-corfu))

(require 'init-calendar)
(require 'init-dashboard)
(require 'init-dired)
(require 'init-highlight)
(require 'init-ibuffer)
(require 'init-kill-ring)
(require 'init-workspace)
(require 'init-window)
(require 'init-treemacs)

(require 'init-shell)

(require 'init-space)
(require 'init-markdown)
(require 'init-org)
(require 'init-agenda)

(require 'init-rss)
(require 'init-utils)
(require 'init-reader)
(require 'init-misc)
(require 'init-mail)
(require 'init-w3m)
(require 'init-dict)

;; Programming
(require 'init-vcs)
(require 'init-flymake)
(require 'init-projectile)
(require 'init-lsp)
(require 'init-dap)
(require 'init-ctags)

(require 'init-leetcode)
(require 'init-prog)
(require 'init-elisp)
(require 'init-c)
(require 'init-go)
(require 'init-rust)
(require 'init-kotlin)
(require 'init-python)
(require 'init-dart)
(require 'init-web)
(require 'init-ai)
;; (require 'init-ros)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
