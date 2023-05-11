;;; init-rss.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-rss.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:32:56 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 21
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

(use-package elfeed
  :defer t
  :pretty-hydra
  ((:title (pretty-hydra-title "Elfeed" 'faicon "nf-fa-rss_square" :face 'nerd-icons-orange)
    :color amaranth :quit-key "q")
   ("Search"
    (("c" elfeed-db-compact "compact db")
     ("g" elfeed-search-update--force "refresh")
     ("G" elfeed-search-fetch "update")
     ("y" elfeed-search-yank "copy URL")
     ("+" elfeed-search-tag-all "tag all")
     ("-" elfeed-search-untag-all "untag all"))
    "Filter"
    (("s" elfeed-search-live-filter "live filter")
     ("S" elfeed-search-set-filter "set filter")
     ("*" (elfeed-search-set-filter "@6-months-ago +star") "starred")
     ("A" (elfeed-search-set-filter "@6-months-ago" "all"))
     ("T" (elfeed-search-set-filter "@1-day-ago" "today")))
    "Article"
    (("b" elfeed-search-browse-url "browse")
     ("n" next-line "next")
     ("p" previous-line "previous")
     ("u" elfeed-search-tag-all-unread "mark unread")
     ("r" elfeed-search-untag-all-unread "mark read")
     ("RET" elfeed-search-show-entry "show"))))
  :bind (("C-x w" . elfeed)
         :map elfeed-search-mode-map
         ("?" . elfeed-hydra/body)
         :map elfeed-show-mode-map
         ("o" . ace-link)
         ("q" . delete-window))
  :hook (elfeed-show-mode . read-mode)
  :init (setq url-queue-timeout 30
              elfeed-db-directory (locate-user-emacs-file ".elfeed")
              elfeed-show-entry-switch #'pop-to-buffer
              elfeed-show-entry-delete #'delete-window)
  :config (push elfeed-db-directory recentf-exclude))

(use-package elfeed-dashboard
  :commands (elfeed-dashboard)
  :config
  (setq elfeed-dashboard-file (expand-file-name "elfeed-dashboard.org" user-emacs-directory))
  ;; update feed counts on elfeed-quit
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

(use-package elfeed-org
  :init (setq rmh-elfeed-org-files (list (expand-file-name "elfeed-feeds.org" user-emacs-directory)))
  :hook (elfeed-dashboard-mode . my/elfeed-hook)
  :config
  (defun my/elfeed-hook ()
    (defun my/reload-org-feeds ()
      (interactive)
      (rmh-elfeed-org-process rmh-elfeed-org-files rmh-elfeed-org-tree-id))
    (advice-add 'elfeed-dashboard-update :before #'my/reload-org-feeds)
    (elfeed-org)))

;; Another Atom/RSS reader
(use-package newsticker
  :ensure nil
  :defer t
  :bind ("C-x W" . newsticker-show-news)
  :hook (newsticker-treeview-item-mode . read-mode)
  :init (setq newsticker-url-list
              '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
                ("Mastering Emacs" "http://www.masteringemacs.org/feed/")
                ("Oremacs" "https://oremacs.com/atom.xml")
                ("EmacsCast" "https://pinecast.com/feed/emacscast")
                ("Emacs Reddit" "https://www.reddit.com/r/emacs.rss")
                ("Blog News" "http://feed.williamlong.info/") ; 月光博客
                ("Blog Ruanyifeng" "http://www.ruanyifeng.com/blog/atom.xml") ; 阮一峰
                ("manateelazycat" "https://manateelazycat.github.io/feed.xml")))) ; manateelazycat 懒猫 王勇

(provide 'init-rss)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rss.el ends here
