;;; init-utils.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-utils.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:33:35 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 259
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
;; Display available keybindings in popup
(use-package which-key
  :diminish
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :hook (after-init . which-key-mode)
  :init (setq which-key-show-remaining-keys t
              which-key-max-description-length 30)
  :config
  (which-key-add-key-based-replacements
    "SPC a" "application"
    "SPC a o" "agenda"
    "SPC a t" "command log"
    "SPC a y" "ein"
    "SPC b" "buffer"
    "SPC f" "file"
    "SPC g" "magit"
    "SPC h" "describe"
    "SPC n" "narrow"
    "SPC p" "project"
    "SPC q" "quit"
    "SPC s" "search"
    "SPC t" "toggle"
    "SPC w" "window"
    "SPC x" "up/downcase"
    "SPC y" "youdao"
    "SPC F" "frame"
    "SPC P" "password"))

;; Youdao Dictionary
(use-package youdao-dictionary
  :commands youdao-dictionary-play-voice-of-current-word
  :bind (("C-c y" . my-youdao-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point)
         :map evil-normal-state-map
         ("q" . quit-window))
  :init
  (setq url-automatic-caching t
        youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词

  (with-no-warnings
    (defun my-youdao-search-at-point ()
      "Search word at point and display result with `posframe', `pos-tip', or buffer."
      (interactive)
      (if (display-graphic-p)
          (if emacs/>=26p
              (youdao-dictionary-search-at-point-posframe)
            (youdao-dictionary-search-at-point-tooltip))
        (youdao-dictionary-search-at-point)))))

  ;; Default, comment out the providers you don't need.
(use-package fanyi
  :init
  (setq fanyi-auto-select t)
  :config
  (with-eval-after-load 'org
    (require 'ol-fanyi))
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider)))


(use-package english-teacher
  :load-path "site-lisp/english-teacher"
  :custom
  (english-teacher-backend 'baidu)
  (english-teacher-show-result-function 'english-teacher-eldoc-show-result-function)
  :hook ((Info-mode
          elfeed-show-mode
          eww-mode
          Man-mode
          Woman-Mode) . english-teacher-follow-mode))

(use-package go-translate
  :config
  (setq gts-translate-list '(("en" "zh"))
        gts-default-translator (gts-translator
                                :picker (gts-prompt-picker)
                                :engines (list (gts-bing-engine))
                                :render (gts-posframe-pop-render))))

;; Writable `grep' buffer
(use-package wgrep
  :bind (:map grep-mode-map
         ("C-c C-q" . wgrep-change-to-wgrep-mode))
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; Fast search tool: `ripgrep'
(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
         ("c" . rg-dwim-current-dir)
         ("f" . rg-dwim-current-file)
         ("m" . rg-menu))
  :init (setq rg-group-result t
              rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases))


;; Docker
(use-package docker
  :defines docker-image-run-arguments
  :bind ("C-c d" . docker)
  :init (setq docker-image-run-arguments '("-i" "-t" "--rm")
              docker-container-shell-file-name "/bin/bash"))

;; `tramp-container' is builtin since 29
(unless emacs/>=29p
  (use-package docker-tramp))


;; Persistent the scratch buffer
(use-package persistent-scratch
  :if (not (eq my-lsp 'lsp-bridge))
  :diminish
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode)))

;; Music player
(use-package bongo
  :functions (bongo-add-dired-files
              dired-get-filename
              dired-marker-regexp
              dired-move-to-filename)
  :commands (bongo-buffer
             bongo-library-buffer-p
             bongo-library-buffer)
  :bind ("C-<f9>" . bongo)
  :init
  (with-eval-after-load 'dired
    (defun bongo-add-dired-files ()
      "Add marked files to Bongo library"
      (interactive)
      (bongo-buffer)
      (let (file (files nil))
        (dired-map-over-marks
         (setq file (dired-get-filename)
               files (append files (list file)))
         nil t)
        (with-bongo-library-buffer
         (mapc 'bongo-insert-file files)))
      (bongo-switch-buffers))
    (bind-key "b" #'bongo-add-dired-files dired-mode-map)))

(use-package netease-cloud-music
  :commands (netease-cloud-music))

;; IRC
(use-package erc
  :ensure nil
  :defines erc-autojoin-channels-alist
  :init (setq erc-rename-buffers t
              erc-interpret-mirc-color t
              erc-lurker-hide-list '("JOIN" "PART" "QUIT")
              erc-autojoin-channels-alist '(("freenode.net" "#emacs"))))

;; text mode directory tree
(use-package ztree
  :custom-face
  (ztreep-header-face ((t (:inherit diff-header))))
  (ztreep-arrow-face ((t (:inherit font-lock-comment-face))))
  (ztreep-leaf-face ((t (:inherit diff-index))))
  (ztreep-node-face ((t (:inherit font-lock-variable-name-face))))
  (ztreep-expand-sign-face ((t (:inherit font-lock-function-name-face))))
  (ztreep-diff-header-face ((t (:inherit (diff-header bold)))))
  (ztreep-diff-header-small-face ((t (:inherit diff-file-header))))
  (ztreep-diff-model-normal-face ((t (:inherit font-lock-doc-face))))
  (ztreep-diff-model-ignored-face ((t (:inherit font-lock-doc-face :strike-through t))))
  (ztreep-diff-model-diff-face ((t (:inherit diff-removed))))
  (ztreep-diff-model-add-face ((t (:inherit diff-nonexistent))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Ztree" 'octicon "nf-oct-diff" :face 'nerd-icons-green)
    :color pink :quit-key "q")
   ("Diff"
    (("C" ztree-diff-copy "copy" :exit t)
     ("h" ztree-diff-toggle-show-equal-files "show/hide equals" :exit t)
     ("H" ztree-diff-toggle-show-filtered-files "show/hide ignores" :exit t)
     ("D" ztree-diff-delete-file "delete" :exit t)
     ("v" ztree-diff-view-file "view" :exit t)
     ("d" ztree-diff-simple-diff-files "simple diff" :exit t)
     ("r" ztree-diff-partial-rescan "partial rescan" :exit t)
     ("R" ztree-diff-full-rescan "full rescan" :exit t))
    "View"
    (("RET" ztree-perform-action "expand/collapse or view" :exit t)
     ("SPC" ztree-perform-soft-action "expand/collapse or view in other" :exit t)
     ("TAB" ztree-jump-side "jump side" :exit t)
     ("g" ztree-refresh-buffer "refresh" :exit t)
     ("x" ztree-toggle-expand-subtree "expand/collapse" :exit t)
     ("<backspace>" ztree-move-up-in-tree "go to parent" :exit t))))
  :bind (:map ztreediff-mode-map
         ("C-<f5>" . ztree-hydra/body))
  :init (setq ztree-draw-unicode-lines t
              ztree-show-number-of-children t))

;; Emacs-appcliation-framework
(use-package eaf
  :load-path "site-lisp/emacs-application-framework"
  :commands (eaf-open eaf-open-browser eaf-open-browser-with-history)
  :hook (eaf-mode . (lambda () (vertico-posframe-mode -1)))
  :preface
  (defun eaf-proxy-enable ()
    "Enable proxy in eaf"
    (interactive)
    (setq eaf-proxy-type "socks5"
          eaf-proxy-host socks-proxy
          eaf-proxy-port (format "%s" socks-port)))
  (defun eaf-proxy-disable ()
    "Disable proxy in eaf"
    (interactive)
    (setq eaf-proxy-type ""
          eaf-proxy-host ""
          eaf-proxy-port ""))
  :custom
  (eaf-find-alternate-file-in-dired t)
  (eaf-start-python-process-when-require t)
  (eaf-browser-continue-where-left-off t)
  (eaf-python-command (executable-find "python3"))
  :config
  (require 'eaf-browser nil t)
  (require 'eaf-evil nil t)
  (require 'eaf-js-video-player nil t)
  (require 'eaf-markdown-previewer nil t)
  (require 'eaf-mindmap nil t)
  (require 'eaf-jupyter nil t)
  (require 'eaf-org-previewer nil t)
  (require 'eaf-org nil t)
  (when (display-graphic-p)
    (require 'eaf-all-the-icons))
  (eaf-bind-key open_link "C-M-s" eaf-browser-keybinding)
  (eaf-bind-key open_devtools "M-i" eaf-browser-keybinding)
  (eaf-bind-key insert_or_recover_prev_close_page "X" eaf-browser-keybinding)
  (eaf-bind-key clear_cookies "C-M-q" eaf-browser-keybinding)
  (eaf-bind-key clear_history "C-M-p" eaf-browser-keybinding)
  (eaf-bind-key open_link "C-M-s" eaf-browser-keybinding)
  (setq eaf-browser-enable-adblocker "true"
        eaf-browser-enable-autofill "true"
        eaf-browser-dark-mode nil))

(use-package eww
  :ensure nil
  :commands (eww)
  :hook (eww-after-render . shrface-mode)
  :config
  (require 'shrface))

;; Misc
(use-package copyit)                    ; copy path, url, etc.
(use-package daemons)                   ; system services/daemons
(use-package diffview)                  ; side-by-side diff view
(use-package esup)                      ; Emacs startup profiler
(use-package focus)                     ; Focus on the current region
(use-package memory-usage)
(use-package tldr)
(use-package reveal-in-osx-finder
  :if sys/macp)
(use-package grab-mac-link
  :if sys/macp)
(use-package command-log-mode
  :commands global-command-log-mode)

;; https://qiqijin.com/cn/dictionary-overlay.html
(use-package dictionary-overlay
  :commands (dictionary-overlay-install dictionary-overlay-start dictionary-overlay-stop dictionary-overlay-render-buffer dictionary-overlay-toggle dictionary-overlay-lookup)
  :load-path "site-lisp/dictionary-overlay")

(use-package websocket-bridge
  :load-path "site-lisp/websocket-bridge")

(use-package mind-wave
  :mode ("\\.chat\\'" . mind-wave-chat-mode)
  :load-path "site-lisp/mind-wave")

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "emacs-slack"
   :default t
   :cookie (password-store-get "slack-cookie")
   :token (password-store-get "slack-token")
   :full-and-display-names t))

(use-package color-rg
  :commands (color-rg-search-input color-rg-search-symbol color-rg-search-input-in-project color-rg-search-symbol-in-project color-rg-search-symbol-in-current-file color-rg-search-input-in-current-file color-rg-search-project-rails color-rg-search-symbol-with-type color-rg-search-project-with-type color-rg-search-project-rails-with-type)
  :load-path "site-lisp/color-rg")

(use-package holo-layer
  :commands (holo-layer-enable)
  :custom (holo-layer-enable-cursor-animation t)
  :load-path "site-lisp/holo-layer")

(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
