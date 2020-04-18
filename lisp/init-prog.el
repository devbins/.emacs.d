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
;;     Update #: 23
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

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :init
  (setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                         ("<-" . ?←)
                                         ("->" . ?→)
                                         ("->>" . ?↠)
                                         ("=>" . ?⇒)
                                         ("map" . ?↦)
                                         ("/=" . ?≠)
                                         ("!=" . ?≠)
                                         ("==" . ?≡)
                                         ("<=" . ?≤)
                                         (">=" . ?≥)
                                         ("=<<" . (?= (Br . Bl) ?≪))
                                         (">>=" . (?≫ (Br . Bl) ?=))
                                         ("<=<" . ?↢)
                                         (">=>" . ?↣)
                                         ("&&" . ?∧)
                                         ("||" . ?∨)
                                         ("not" . ?¬)
                                         ("alpha" . ?α)
                                         ("beta" . ?β)
                                         ("gamma" . ?γ)
                                         ("delta" . Δ)
                                         ("epsilon" . ?ε)
                                         ("zeta" . ?ζ)
                                         ("eta" . ?η)
                                         ("theta" . ?θ)
                                         ("micro" . ?μ)
                                         ("pi" . ?π)
                                         ("rho" . ?ρ)
                                         ("sigma" . ?σ)
                                         ("phi" . ?φ)
                                         ("omega" . ?Ω)
                                         ("sqrt" . ?√)
                                         ("sum" . ∑)
                                         ("infinity" . ∞)
                                         ("Infinity" . ∞)
                                         ("->" . ?→)))
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; Jump to definition
(use-package dumb-jump
  :pretty-hydra
  ((:title (pretty-hydra-title "Dump Jump" 'faicon "anchor")
    :color blue :quit-key "q")
   ("Jump"
    (("j" dumb-jump-go "Go")
     ("o" dumb-jump-go-other-window "Go other window")
     ("e" dumb-jump-go-prefer-external "Go external")
     ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
    "Other"
    (("i" dumb-jump-go-prompt "Prompt")
     ("l" dumb-jump-quick-look "Quick look")
     ("b" dumb-jump-back "Back"))))
  :bind (:map dumb-jump-mode-map
         ("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)
         ("C-M-j" . dumb-jump-hydra/body))
  :hook (after-init . dumb-jump-mode)
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'ivy))

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
  :init (setq org-plantuml-jar-path (concat user-emacs-directory"plantuml.jar")))
(use-package powershell)
(use-package rmsbolt)                   ; A compiler output viewer
(use-package scala-mode)
(use-package swift-mode)
(use-package vimrc-mode)

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

;; New `conf-toml-mode' in Emacs 26
(unless (fboundp 'conf-toml-mode)
  (use-package toml-mode))

;; Batch Mode eXtras
(use-package bmx-mode
  :after company
  :diminish
  :hook (after-init . bmx-mode-setup-defaults))

;; Fish shell
(use-package fish-mode
  :hook (fish-mode . (lambda ()
                       (add-hook 'before-save-hook
                                 #'fish_indent-before-save))))

(use-package unicad
  :commands (unicad-enable unicad-disable)
  :load-path (lambda ()(expand-file-name "site-lisp/unicad/" user-emacs-directory)))

(use-package format-all)

(use-package helm-dash
  :config
  (setq helm-dash-browser-func 'eww))


(use-package gdb-mi
  :init
  (setq
   ;; use gdb-many-windows by default when `M-x gdb'
   gdb-many-windows t
   ;; Non-nil means display source file containing the main routine at startup
   gdb-show-main t))

(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
