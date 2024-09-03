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
;;     Update #: 30
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
  :ensure nil
  :commands (mu4e make-mu4e-context)
  :init
  (use-package mu4e-alert
    :config
    (when (executable-find "notify-send")
      (mu4e-alert-set-default-style 'libnotify))
    :hook
    ((after-init . mu4e-alert-enable-notifications)
     (after-init . mu4e-alert-enable-mode-line-display)))
  (use-package mu4e-overview :defer t)
  :bind
  ((:map mu4e-view-mode-map
    ("e" . mu4e-view-save-attachment)))
  :custom
  (mu4e-maildir (expand-file-name "~/.mail"))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-view-prefer-html t)
  (mu4e-update-interval 180)
  (mu4e-headers-auto-update t)
  (mu4e-compose-format-flowed t)
  (mu4e-view-show-images t)
  (mu4e-change-filenames-when-moving t) ; work better for mbsync
  (mu4e-attachment-dir "~/.mail/Downloads")
  (message-kill-buffer-on-exit t)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-view-show-addresses t)
  (mu4e-confirm-quit nil)
  (mu4e-use-fancy-chars t)
  (mu4e-headers-unread-mark    '("u" . "📩 "))
  (mu4e-headers-draft-mark     '("D" . "🚧 "))
  (mu4e-headers-flagged-mark   '("F" . "🚩 "))
  (mu4e-headers-new-mark       '("N" . "✨ "))
  (mu4e-headers-passed-mark    '("P" . "↪ "))
  (mu4e-headers-replied-mark   '("R" . "↩ "))
  (mu4e-headers-seen-mark      '("S" . " "))
  (mu4e-headers-trashed-mark   '("T" . "🗑️ "))
  (mu4e-headers-attach-mark    '("a" . "📎 "))
  (mu4e-headers-encrypted-mark '("x" . "🔑 "))
  (mu4e-headers-signed-mark    '("s" . "🖊 "))
  (mu4e-headers-list-mark      '("l" . "🔈 "))
  (mu4e-headers-personal-mark  '("p" . "👨 "))
  (mu4e-headers-calendar-mark  '("c" . "📅 "))
  (mu4e-headers-results-limit 1000)
  (mu4e-view-use-gnus t)
  (mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶"))
  (mu4e-headers-thread-orphan-prefix '("┬>" . "┬▶ "))
  (mu4e-headers-thread-connection-prefix '("│ " . "│ "))
  (mu4e-headers-thread-first-child-prefix '("├>" . "├▶"))
  (mu4e-headers-thread-child-prefix '("├>" . "├▶"))
  (mu4e-headers-thread-last-child-prefix '("└>" . "╰▶"))
  (gnus-icalendar-org-capture-file "~/.org/.agenda/meetings.org") ; Prerequisite: set it to meetings org fie
  (gnus-icalendar-org-capture-headline '("Meetings")) ; Make sure to create Calendar heading first
  :hook
  ((mu4e-view-mode . visual-line-mode)
   (mu4e-compose-mode . (lambda ()
                          (visual-line-mode)
                          (use-hard-newlines -1)
                          (flyspell-mode)))
   (mu4e-view-mode . (lambda() ;; try to emulate some of the eww key-bindings
                       (local-set-key (kbd "<tab>") 'shr-next-link)
                       (local-set-key (kbd "<backtab>") 'shr-previous-link)))
   (mu4e-headers-mode . (lambda ()
                          (interactive)
                          (setq mu4e-headers-fields
                                `((:human-date . 25) ;; alternatively, use :date
                                  (:flags . 6)
                                  (:from . 22)
                                  (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
                                  (:size . 7))))))
  :init
  (use-package mu4e-thread-folding
    :load-path "site-lisp/mu4e-thread-folding"
    :after mu4e
    :bind
    ((:map mu4e-headers-mode-map
           ("TAB" . mu4e-headers-toggle-at-point)
           ("C-<tab>" . mu4e-headers-toggle-fold-all))
     (:map mu4e-search-minor-mode-map
           ("S" . mu4e-kill-update-mail)))
    :custom
    (mu4e-thread-folding-default-view `folded)
    (mu4e-headers-fields '((:empty         .    2)
                           (:human-date    .   12)
                           (:flags         .    6)
                           (:mailing-list  .   10)
                           (:from          .   22)
                           (:subject       .   nil)))
    :config
    (add-to-list 'mu4e-header-info-custom
                 '(:empty . (:name "Empty"
                                   :shortname ""
                                   :function (lambda (msg) "  ")))))
  :config
  (require 'mu4e-icalendar)
  (setq mail-user-agent (mu4e-user-agent))
  (mu4e-icalendar-setup)
  (gnus-icalendar-org-setup)
  (defalias 'mu4e-add-attachment 'mail-add-attachment
    "I prefer the add-attachment function to begin wih mu4e so I can find it easily.")

  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "gmail"
          :enter-func (lambda () (mu4e-message "Entering context gmail"))
          :leave-func (lambda () (mu4e-message "Leaving context gmail"))
          :match-func
          (lambda (msg)
            (when msg
              (string-match "gmail" (mu4e-message-field msg :maildir))))
          :vars '((mu4e-sent-folder . "/gmail/Sent Mail")
                  (mu4e-drafts-folder . "/gmail/Drafts")
                  (mu4e-trash-folder . "/gmail/Trash")
                  (mu4e-sent-messages-behavior . sent)
                  (mu4e-compose-signature . user-full-name)
                  (user-mail-address . user-mail-address) ; Prerequisite: Set this to your email
                  (mu4e-compose-format-flowed . t)
                  (smtpmail-queue-dir . "~/.mail/gmail/queue/cur")
                  (message-send-mail-function . smtpmail-send-it)
                  (smtpmail-smtp-user . "matthewzmd") ; Set to your username
                  (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
                  (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
                  (smtpmail-default-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-debug-info . t)
                  (smtpmail-debug-verbose . t)
                  (mu4e-maildir-shortcuts . ( ("/gmail/INBOX" . ?i)
                                              ("/gmail/Sent Mail" . ?s)
                                              ("/gmail/Trash"       . ?t)
                                              ("/gmail/All Mail"  . ?a)
                                              ("/gmail/Starred"   . ?r)
                                              ("/gmail/Drafts"    . ?d))))))))

(provide 'init-mail)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-mail.el ends here
