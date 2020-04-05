;;; init-mail.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-mail.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Sat Mar 21 23:05:06 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 13
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

;; https://medium.com/@enzuru/emacs-26-wanderlust-and-modern-gmail-authentication-36e1ae61471f
;; http://juanjose.garciaripoll.com/blog/emacs-wanderlust-email/index.html
;; https://box.matto.nl/emacsgmail.html
(use-package wanderlust
  :defer t
  ;; Neither wl-folder-mode or wl-summary-mode are correctly defined as major
  ;; modes, so `evil-set-initial-state' won't work here.
  ;; (add-hook! '(wl-folder-mode-hook wl-summary-mode-hook)
  ;;            #'evil-emacs-state)
  :hook ((wl-folder-mode . evil-emacs-state)
         (wl-summary-mode . evil-emacs-state))
  :config
  (setq mail-user-agent 'wl-user-agent
        org-mime-library 'semi
        pgg-scheme 'gpg
        mime-edit-split-message nil)
  (setq mime-view-text/html-previewer shr
        shr-use-fonts nil
        shr-use-colors nil
        mime-w3m-safe-url-regexp nil
        mime-setup-enable-inline-html 'shr
        mime-shr-blocked-images nil
        mime-setup-enable-inline-image t)

  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'wl-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook))

  (setq wl-demo nil
        wl-stay-folder-window t
        wl-folder-window-width 25)

  (setq wl-message-truncate-lines t
        wl-summary-width 120
        wl-message-visible-field-list
        '("^Message-Id:"
          "^User-Agent:"
          "^X-Mailer:"
          "^X-Face:"
          "^To:"
          "^Cc:"
          "^From:"
          "^Subject:"
          "^Date:"))

  ;; ;; IMAP
  ;; (setq elmo-imap4-default-server "imap.gmail.com"
  ;;       elmo-imap4-default-user "<accountname>@gmail.com"
  ;;       elmo-imap4-default-port 993
  ;;       elmo-imap4-default-authenticate-type 'clear ; CRAM-MD5
  ;;       elmo-imap4-default-user user-mail-address
  ;;       elmo-imap4-default-stream-type 'ssl
  ;;       elmo-imap4-use-modified-utf7 t
  ;;       elmo-imap4-set-seen-flag-explicitly t)

  ;; SMTP
  (setq wl-smtp-connection-type 'starttls
        wl-smtp-posting-port 587
        wl-smtp-authenticate-type "plain"
        wl-smtp-posting-user user-mail-address
        wl-smtp-posting-server "smtp.gmail.com"
        wl-local-domain "gmail.com")

  (setq wl-default-folder "%inbox"
        wl-draft-folder "%[Gmail]/Drafts"
        wl-trash-folder "%[Gmail]/Trash"
        wl-fcc-force-as-read t
        wl-folder-check-async t
        wl-default-spec "%")

  (setq wl-message-id-domain wl-local-domain)

  (add-hook 'mime-edit-mode-hook #'auto-fill-mode))


(provide 'init-mail)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-mail.el ends here
