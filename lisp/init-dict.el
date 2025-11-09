;;; init-dict.el --- -*- lexical-binding: t no-byte-compile: t; -*-
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
;;     Update #: 52
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

(use-package gt
  :bind ("C-c t e" . gt-translate)
  :init
  (setq gt-langs '(en zh)
        gt-buffer-render-follow-p t
        gt-buffer-render-window-config
        '((display-buffer-reuse-window display-buffer-in-direction)
          (direction . bottom)
          (window-height . 0.4)))

  (setq gt-pop-posframe-forecolor (face-foreground 'tooltip nil t)
        gt-pop-posframe-backcolor (face-background 'tooltip nil t)
        gt-pin-posframe-bdcolor (face-background 'posframe-border nil t))
  :config
  (with-no-warnings
    (setq gt-preset-translators
          `((default . ,(gt-translator
                         :taker   (list (gt-taker :pick nil :if 'selection)
                                        (gt-taker :text 'paragraph :if '(Info-mode help-mode helpful-mode devdocs-mode))
                                        (gt-taker :text 'buffer :pick 'fresh-word
                                                  :if (lambda (translatror)
                                                        (and (not (derived-mode-p 'fanyi-mode)) buffer-read-only)))
                                        (gt-taker :text 'word))
                         :engines (if (display-graphic-p)
                                      (list (gt-bing-engine :if 'not-word)
                                            (gt-youdao-dict-engine :if 'word))
                                    (list (gt-bing-engine :if 'not-word)
                                          (gt-youdao-dict-engine :if 'word)
                                          (gt-youdao-suggest-engine :if 'word)
                                          (gt-google-engine :if 'word)))
                         :render  (list (gt-posframe-pop-render
                                         :if (lambda (translator)
                                               (and (display-graphic-p)
                                                    (not (derived-mode-p 'Info-mode 'help-mode 'helpful-mode 'devdocs-mode))
                                                    (not (member (buffer-name) '("COMMIT_EDITMSG")))))
                                         :frame-params (list :accept-focus nil
                                                             :width 70
                                                             :height 15
                                                             :left-fringe 16
                                                             :right-fringe 16
                                                             :border-width 1
                                                             :border-color gt-pin-posframe-bdcolor))
                                        (gt-overlay-render :if 'read-only)
                                        (gt-insert-render :if (lambda (translator) (member (buffer-name) '("COMMIT_EDITMSG"))))
                                        (gt-buffer-render))))
            (multi-dict . ,(gt-translator :taker (gt-taker :prompt t)
                                          :engines (list (gt-bing-engine)
                                                         (gt-youdao-dict-engine)
                                                         (gt-youdao-suggest-engine :if 'word)
                                                         (gt-google-engine))
                                          :render (gt-buffer-render)))
            (Text-Utility . ,(gt-text-utility :taker (gt-taker :pick nil)
                                              :render (gt-buffer-render)))))

    (defun gt--translate (dict)
      "Translate using DICT from the preset tranlators."
      (gt-start (alist-get dict gt-preset-translators)))

    (defun gt-translate-prompt ()
      "Translate with prompt using the multiple dictionaries."
      (interactive)
      (gt--translate 'multi-dict))

    (defun gt-use-text-utility ()
      "Handle the texts with the utilities."
      (interactive)
      (gt--translate 'Text-Utility))))

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
        insert-translated-name-ollama-model-name "qwen3:14b"))

(use-package anki-editor
  :load-path "site-lisp/anki-editor")

(use-package paw
  :load-path "site-lisp/paw"
  :commands (paw paw-annotation-mode paw-annotation-live-mode)
  :config
  (setq paw-authorization-keys (auth-source-pass-get 'secret "eudic")
              paw-online-word-servers '(eudic anki)
              paw-nerd-icons-icon-enable t
              paw-click-overlay-enable t
              paw-sdcv-dictionary-data-dir (expand-file-name "dict" user-emacs-directory)
              paw-sdcv-dictionary-list '(""))
  (add-to-list 'paw-annotation-mode-supported-modes 'xwidget-webkit-mode))

(provide 'init-dict)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dict.el ends here
