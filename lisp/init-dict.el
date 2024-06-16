;;; init-dict.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-dict.el
;; Description:
;; Author: devbin
;; Maintainer:
;; Copyright (C) 2019 devbin
;; Created: Sun Jun 16 14:11:30 2024 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 17
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

;; Youdao Dictionary
(use-package youdao-dictionary
  :commands youdao-dictionary-play-voice-of-current-word
  :bind (("C-c y" . my-youdao-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point)
         :map evil-normal-state-map
         ("q" . quit-window))
  :init
  (setq url-automatic-caching t)

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
  :init
  (setq english-teacher-backend 'tencent)
  :hook ((Info-mode
          eww-mode
          Man-mode
          Woman-Mode) . english-teacher-follow-mode))

(use-package go-translate
  :config
  (setq gt-langs '("en" "zh")
        gt-default-translator (gt-translator
                               :take (gt-taker :text 'buffer :pick 'paragraph)
                               :engines (list (gt-stardict-engine :exact t)
                                              (gt-bing-engine))
                               :render (gt-posframe-pop-render))))

;; https://qiqijin.com/cn/dictionary-overlay.html
(use-package dictionary-overlay
  :load-path "site-lisp/dictionary-overlay"
  :commands (dictionary-overlay-install dictionary-overlay-start dictionary-overlay-stop dictionary-overlay-render-buffer dictionary-overlay-toggle dictionary-overlay-lookup))

;; Automatically translate Chinese into English
(use-package insert-translated-name
  :load-path "site-lisp/insert-translated-name"
  :if (executable-find "ollama")
  :commands (insert-translated-name-insert)
  :bind ("C-c t t" . 'insert-translated-name-insert)
  :config
  (setq insert-translated-name-camel-style-mode-list
        '(go-mode))
  (setq insert-translated-name-program "ollama"
        insert-translated-name-ollama-model-name "qwen2"))

(use-package paw
  :load-path "site-lisp/paw")

(provide 'init-dict)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dict.el ends here
