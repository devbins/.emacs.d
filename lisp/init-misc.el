;;; init-misc.el --- -*- lexical-binding: t no-byte-compile: t; -*-
;;
;; Filename: init-misc.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Thu Feb 13 11:38:36 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 107
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

(use-package aria2
  :defer t
  :commands aria2-downloads-list
  :config
  (setq aria2-download-directory (expand-file-name "~/Downloads"))
  (evil-define-key 'normal aria2-mode-map
    "f" 'aria2-add-file
    "u" 'aria2-add-uris
    "D" 'aria2-remove-download
    "C" 'aria2-clean-removed-download
    "q" 'quit-window
    "Q" 'aria2-terminate))

;; Header2Pac
(use-package header2
  :load-path "site-lisp/header2"
  :hook (emacs-lisp-mode . auto-make-header)
  :custom
  (header-copyright-notice (concat "Copyright (C) 2019 " (user-full-name) "\n"))
  :config
  (autoload 'auto-make-header "header2")
  (autoload 'auto-update-file-header "header2")
  (add-to-list 'write-file-functions 'auto-update-file-header))


;; broadway isometric1 starwars doom banner
;; sudo pacman -S figlet
(use-package figlet
  :if (executable-find "figlet")
  :config (setq figlet-default-font "banner"))

(use-package carbon-now-sh)

(use-package speed-type)

(use-package telega
  :commands (telega)
  :defer t
  :init (setq telega-use-images (if (display-graphic-p) t nil))
  :config
  (setq telega-autoplay-mode t
        telega-notifications-mode t
        telega-emoji-use-images nil
        telega-chat-fill-column 90
        telega-sticker-size '(6 . 24)
        ;; 替代两行头像，防止头像因为字符高度不统一裂开。
        telega-avatar-workaround-gaps-for (when (display-graphic-p) '(return t))
        ;; 以下都是 telega-symbols-emojify 中的 telega-symbol
        ;; telega-symbol
        ;; remove iterm from `telega-symbols-emojify`
        telega-symbols-emojify (cl-reduce (lambda (emojify key)
                                            (assq-delete-all key emojify))
                                          '(verified vertical-bar checkmark forum heavy-checkmark reply reply-quote horizontal-bar forward button-close summarize-in summarize-out)
                                          :initial-value telega-symbols-emojify)
        telega-symbol-button-close (nerd-icons-mdicon "nf-md-close_box_outline")
        telega-symbol-verified (nerd-icons-codicon "nf-cod-verified_filled" :face 'telega-blue)
        telega-symbol-vertical-bar "│" ;; U+2502 Box Drawings Light Vertical
        telega-symbol-saved-messages-tag-end (nerd-icons-faicon "nf-fa-tag")
        telega-symbol-forum (nerd-icons-mdicon "nf-md-format_list_text")
        telega-symbol-flames (nerd-icons-mdicon "nf-md-delete_clock")
        telega-symbol-mark (propertize " " 'face 'telega-button-highlight)
        telega-symbol-reply (nerd-icons-faicon "nf-fa-reply")
        telega-symbol-reply-quote (nerd-icons-faicon "nf-fa-reply_all")
        telega-symbol-forward (nerd-icons-faicon "nf-fa-mail_forward")
        telega-symbol-checkmark (nerd-icons-mdicon "nf-md-check")
        telega-symbol-heavy-checkmark (nerd-icons-codicon "nf-cod-check_all")
        telega-symbol-summarize-in (nerd-icons-octicon "nf-oct-fold")
        telega-symbol-summarize-out (nerd-icons-octicon "nf-oct-unfold")
        telega-translate-to-language-by-default "zh"
        telega-chat-input-markups '("markdown2" "org")
        ;; telega-root
        ;; telega-root-default-view-function 'telega-view-folders
        telega-root-keep-cursor 'track
        ;; telega-root-show-avatars nil
        telega-root-buffer-name "*Telega Root*"
        ;; remove chat folder icons
        telega-chat-folders-insexp (lambda () nil)
        telega-filters-custom nil
        telega-root-fill-column 70 ; fill-column
        telega-filter-custom-show-folders nil)
  (add-hook 'telega-before-auth-hook
            (lambda ()
              (telega--addProxy `(:server ,socks-proxy :port ,socks-port
                                  :type (:@type "proxyTypeSocks5")) :enable-p 'enable))))


(use-package pass
  :if (executable-find "pass")
  :init
  (use-package password-store)
  (use-package auth-source-pass
    :ensure nil
    :init
    (setq auth-source-debug t)
    (auth-source-pass-enable)))

(use-package epa-file
  :ensure nil
  :init
  (setq epg-pinentry-mode 'loopback
        epa-file-cache-passphrase-for-symmetric-encryption t)
  :config
  (epa-file-enable))

(use-package autoinsert
  :ensure nil
  :hook (after-init . auto-insert-mode)
  :init (setq-default auto-insert-directory (expand-file-name "auto-insert-template" user-emacs-directory))
  (setq auto-insert-query nil)
  :config
  (defun autoinsert-yas-expand ()
    "Replace text in yasnippet template."
    (evil-insert 0)
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  (add-to-list 'auto-insert-alist '(("\\.py\\'" . "Python souce code header") . ["template.py" autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '(("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header") . ["template.h" autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '(("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ program") . ["template.c" autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '(("\\.go\\'" . "Go program") . ["template.go" autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '(("\\.sh\\'" . "shell program") . ["template.sh" autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '(("CMakeLists.txt\\'" . "CMake") . ["template.cmake" autoinsert-yas-expand])))

(use-package restart-emacs
  :commands restart-emacs)

(use-package achive
  :load-path "site-lisp/achive"
  :custom
  (achive-auto-refresh t)
  (achive-refresh-seconds 5)
  (achive-stock-list '("sh600036" "sh601012" "sz000625" "sz002050" "sz002013" "sh600176")))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'init-misc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-misc.el ends here
