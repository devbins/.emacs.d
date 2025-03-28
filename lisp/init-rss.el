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
         ("j" . next-line)
         ("k" . previous-line)
         ("?" . elfeed-hydra/body)
         :map elfeed-show-mode-map
         ("j" . next-line)
         ("k" . previous-line)
         ("o" . ace-link)
         ("q" . delete-window))
  :hook (elfeed-show-mode . read-mode)
  :init (setq url-queue-timeout 30
              elfeed-db-directory (locate-user-emacs-file ".elfeed")
              elfeed-show-entry-switch #'pop-to-buffer
              elfeed-show-entry-delete #'delete-window)
  :config
  (defun nerd-icon-for-tags (tags)
    "Generate Nerd Font icon based on tags.
  Returns default if no match."
    (cond ((member "youtube" tags)  (nerd-icons-faicon "nf-fa-youtube_play" :face '(:foreground "#FF0200")))
          ((member "emacs" tags) (nerd-icons-sucicon "nf-custom-emacs" :face '(:foreground "#9A5BBE")))
          ((member "economics" tags) (nerd-icons-mdicon "nf-md-alpha_e_box_outline" :face '(:foreground "#E3120C")))
          ((member "db" tags) (nerd-icons-devicon "nf-dev-database" :face '(:foreground "#0574E8")))
          ((member "novel" tags) (nerd-icons-faicon "nf-fa-book" :face '(:foreground "#02C298")))
          ((member "forum" tags) (nerd-icons-faicon "nf-fa-forumbee" :face '(:foreground "#EF9120")))
          ((member "blog" tags) (nerd-icons-octicon "nf-oct-note"))
          ((member "Android" tags) (nerd-icons-faicon "nf-fa-android" :face '(:foreground "#2AB24C")))
          ((member "github" tags) (nerd-icons-faicon "nf-fa-github"))
          ((member "sourcehut" tags) (nerd-icons-faicon "nf-fa-circle_o"))
          (t (nerd-icons-faicon "nf-fae-feedly" :face '(:foreground "#2AB24C")))))
  (defun +elfeed-search-print-entry--better-default (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (date-width (car (cdr elfeed-search-date-format)))
           (title (concat (or (elfeed-meta entry :title)
                             (elfeed-entry-title entry) "")
                          ;; NOTE: insert " " for overlay to swallow
                          " "))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (mapconcat (lambda (s) (propertize s 'face 'elfeed-search-tag-face)) tags ","))
           (title-width (- (frame-width)
                           ;; (window-width (get-buffer-window (elfeed-search-buffer) t))
                           date-width elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                                 elfeed-search-title-max-width) :left))


           ;; Title/Feed ALIGNMENT
           (align-to-feed-pixel (+ date-width
                                   (max elfeed-search-title-min-width
                                        (min title-width elfeed-search-title-max-width)))))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize title-column 'face title-faces 'kbd-help title))
      (put-text-property (1- (point)) (point) 'display `(space :align-to ,align-to-feed-pixel))
      ;; (when feed-title (insert " " (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when feed-title
        (insert " " (concat (nerd-icon-for-tags tags) " ")
                (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when tags (insert "(" tags-str ")"))))
  (setq elfeed-search-print-entry-function #'+elfeed-search-print-entry--better-default)
  (push elfeed-db-directory recentf-exclude))

(use-package elfeed-dashboard
  :commands (elfeed-dashboard)
  :bind (:map elfeed-dashboard-mode-map
         ("j" . next-line)
         ("k" . previous-line))
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
