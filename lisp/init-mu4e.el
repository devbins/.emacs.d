;;; init-mu4e.el ---
;;
;; Filename: init-mu4e.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Sat Mar  6 14:40:56 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 38
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

(use-package mu4e
  :commands (mu4e)
  :load-path "/usr/local/Cellar/mu/1.4.15/share/emacs/site-lisp/mu/mu4e"
  :if (executable-find "mu")
  :bind (:map mu4e-view-mode-map
         ("l" . mu4e-view-go-to-url)
         ("0" . scroll-down-command)
         ("9" . scroll-up-command)
         :map mu4e-compose-mode-map
         ("C-c '" . org-mime-edit-mail-in-org-mode))
  :config
  (require 'mu4e-contrib)
  (setq mu4e-contexts
		(list
         (make-mu4e-context
		  :name "126"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/126" (mu4e-message-field msg :maildir))))
          :vars '((mu4e-sent-folder . "/126/Sent Messages")
                  (mu4e-trash-folder . "/126/Deleted Messages")
                  (mu4e-refile-folder . "/126/Archive")
                  (mu4e-drafts-folder . "/126/Drafts")
                  ( user-mail-address       . "bin6160@126.com")
                  ( smtpmail-smtp-user      . "bin6160@126.com")
                  ( smtpmail-smtp-server    . "smtp.126.com")
                  ( smtpmail-smtp-service   . 465)
                  (smtpmail-stream-type . 'ssl)
                  (mu4e-maildir-shortcuts . ((:maildir "/126/INBOX" :key ?i)
                                             (:maildir "/126/Sent Messages" :key ?s)
                                             (:maildir "/126/Drafts" :key ?D)
                                             (:maildir "/126/Junk" :key ?j)
                                             )))))
        smtpmail-smtp-service 465
        smtpmail-smtp-server "smtp.126.com"
        smtpmail-stream-type 'ssl
        message-send-mail-function 'smtpmail-send-it
        ;; https://emacs.stackexchange.com/a/45216/16450
        message-citation-line-format "\nOn %a, %b %d, %Y at %r %z, %N wrote:\n"
        message-citation-line-function 'message-insert-formatted-citation-line
        ;; message-cite-style message-cite-style-gmail
        ;; mml-secure-openpgp-signers '("D3026E5C08A0BAB4")
        ;; mml-secure-openpgp-encrypt-to-self t
        mu4e-view-show-addresses t
        mu4e-view-show-images t
        mu4e-view-prefer-html t
        mu4e-view-date-format "%a, %Y-%m-%d %T"
        mu4e-view-html-plaintext-ratio-heuristic  most-positive-fixnum
        mu4e-attachment-dir "~/.mails/Downloads"
        mu4e-sent-messages-behavior 'delete
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask-if-none
        mu4e-compose-dont-reply-to-self t
        mu4e-compose-format-flowed t
        mu4e-confirm-quit nil
        mu4e-headers-date-format "%+4Y-%m-%d"
        mu4e-headers-auto-update t
        mu4e-update-interval (* 15 60)
        mu4e-get-mail-command "mbsync -a"
        mu4e-use-fancy-chars t
        auth-source-debug t
        auth-source-do-cache nil
        auth-sources '(password-store))
  (auth-source-pass-enable)
  (add-to-list 'mu4e-view-actions '("browser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-fields :bcc)
  (add-to-list 'mu4e-bookmarks '(:name "All Inbox"
                                 :query "maildir:/126/INBOX"
                                 :key ?i)))

(use-package org-mime
  :config

  (defun my/org-mime-htmlize ()
    (let ((answer (read-from-minibuffer "Org htmlize? [y/n]:")))
      (when (string-equal "y" answer)
        (org-mime-htmlize))))

  (defun my/sign-or-encrypt-message ()
    (let ((answer (read-from-minibuffer "Sign or encrypt?[s/e]: ")))
      (cond
       ((string-equal answer "s") (progn
                                    (message "Signing message.")
                                    (mml-secure-message-sign-pgpmime)))
       ((string-equal answer "e") (progn
                                    (message "Encrypt and signing message.")
                                    (mml-secure-message-encrypt-pgpmime)))
       (t (progn
            (message "Dont signing or encrypting message.")
            nil)))))

  (defun my/send-message-hook ()
    (my/org-mime-htmlize)
    (my/sign-or-encrypt-message))

  (add-hook 'message-send-hook 'my/send-message-hook)
  (setq org-mime-export-ascii 'utf-8
        org-mime-export-options '(:section-numbers nil
                                                   :with-author nil
                                                   :with-toc nil)))

(provide 'init-mu4e)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-mu4e.el ends here
