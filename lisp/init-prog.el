;;; init-prog.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-prog.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:36:12 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 110
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

(use-package prog-mode
  :ensure nil
  :init
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  :hook(prog-mode . (lambda ()
                      (setq prettify-symbols-alist prettify-prog-symbols-alist)
                      (prettify-symbols-mode))))

;; Cross-referencing commands
(use-package xref
  :ensure nil
  :init
  (when (and (boundp 'xref-search-program) (executable-find "rg"))
    (setq xref-search-program 'rg))

  (setq xref-show-xrefs-function #'xref-show-definitions-completing-read
        xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

;; Run commands quickly
(use-package quickrun
  :bind (("C-<f5>" . quickrun)
         ("C-c x" . quickrun)))

(use-package cask-mode)
(use-package csv-mode)
(use-package dockerfile-mode)
(use-package julia-mode)
(use-package lua-mode)
(use-package plantuml-mode
  :init (setq org-plantuml-jar-path (concat user-emacs-directory "plantuml.jar")))
(use-package rmsbolt)                   ; A compiler output viewer
(use-package groovy-mode)
(use-package scala-mode)
(use-package swift-mode)
(use-package vimrc-mode)

(use-package subword
  :ensure nil
  :hook (after-init . global-subword-mode))

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

;; New `conf-toml-mode' in Emacs 26
(unless (fboundp 'conf-toml-mode)
  (use-package toml-mode))

(use-package format-all)

(use-package helm-dash
  :config
  (setq helm-dash-browser-func 'w3m))

;; Browse devdocs.io documents using EWW
(use-package devdocs
  :if emacs/>=27p
  :commands devdocs--installed-p
  :bind (:map prog-mode-map
         ("<f1>" . devdocs-dwim))
  :init
  (defvar devdocs-major-mode-docs-alist
    '((c-mode . ("C"))
      (c++-mode . ("C++"))
      (python-mode . ("Python 3.9" "Python 3.8"))
      (ruby-mode . ("Ruby 3"))
      (go-mode . ("Go"))
      (rustic-mode . ("Rust"))
      (css-mode . ("CSS"))
      (html-mode . ("HTML"))
      (js-mode . ("JavaScript" "JQuery"))
      (js2-mode . ("JavaScript" "JQuery"))
      (emacs-lisp-mode . ("Elisp")))
    "Alist of MAJOR-MODE and list of docset names.")

  (mapc
   (lambda (mode)
     (add-hook (intern (format "%s-hook" (car mode)))
               (lambda ()
                 (setq-local devdocs-current-docs (cdr mode)))))
   devdocs-major-mode-docs-alist)

  (setq devdocs-data-dir (expand-file-name "devdocs" user-emacs-directory))

(defun devdocs-dwim()
    "Look up a DevDocs documentation entry.

Install the doc if it's not installed."
    (interactive)
    ;; Install the doc if it's not installed
    (mapc
     (lambda (slug)
       (unless (member slug (let ((default-directory devdocs-data-dir))
                              (seq-filter #'file-directory-p
                                          (when (file-directory-p devdocs-data-dir)
                                            (directory-files "." nil "^[^.]")))))
         (mapc
          (lambda (doc)
            (when (string= (alist-get 'slug doc) slug)
              (devdocs-install doc)))
          (devdocs--available-docs))))
     (alist-get major-mode devdocs-major-mode-docs-alist))

    ;; Lookup the symbol at point
    (devdocs-lookup nil (thing-at-point 'symbol t))))

(use-package gdb-mi
  :init
  (setq
   gdb-mi-decode-strings "utf-8"
   ;; use gdb-many-windows by default when `M-x gdb'
   gdb-many-windows t
   ;; Non-nil means display source file containing the main routine at startup
   gdb-show-main t
   gdb-use-colon-colon-notation t
   gdb-show-changed-values t
   gdb-restore-window-configuration-after-quit t))

(use-package realgud)
(use-package realgud-lldb)

(use-package demangle-mode)

(use-package separedit
  :bind (:map prog-mode-map ("C-c '" . separedit))
  :init
  (setq separedit-default-mode 'org-mode
        separedit-remove-trailing-spaces-in-comment t))

(use-package yaml-mode)
(use-package protobuf-mode)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :config (use-package yasnippet-snippets))

(use-package treesit-auto
  :commands (global-treesit-auto-mode)
  :demand
  :init (setq treesit-auto-install 'prompt
              treesit-font-lock-level 4)
  :config (global-treesit-auto-mode))

(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
