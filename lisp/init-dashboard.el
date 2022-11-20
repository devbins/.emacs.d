;;; init-dashboard.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-dashboard.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:25:50 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 22
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


;; Dashboard
(use-package dashboard
  :after all-the-icons
  :diminish dashboard-mode
  :functions (all-the-icons-material
              winner-undo
              widget-forward)
  :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Dashboard" 'material "dashboard" :height 1.1 :v-adjust -0.225)
    :color pink :quit-key "q")
   ("Navigator"
    (("H" browse-homepage "homepage" :exit t)
     ("R" restore-previous-session "recover session" :exit t)
     ("L" restore-session "list sessions" :exit t))
    "Section"
    (("}" dashboard-next-section "next")
     ("{" dashboard-previous-section "previous")
     ("r" dashboard-goto-recent-files "recent files")
     ("m" dashboard-goto-bookmarks "bookmarks")
     ("p" dashboard-goto-projects "projects"))
    "Item"
    (("RET" widget-button-press "open" :exit t)
     ("<tab>" widget-forward "next")
     ("C-i" widget-forward "next")
     ("<backtab>" widget-backward "previous")
     ("C-n" next-line "next line")
     ("C-p" previous-line "previous  line"))
    "Misc"
    (("<f2>" open-dashboard "open" :exit t)
     ("g" dashboard-refresh-buffer "refresh" :exit t)
     ("Q" quit-dashboard "quit" :exit t))))
  :bind (("<f2>" . open-dashboard)
         :map dashboard-mode-map
         ("<mouse-1>" . widget-button-click)
         ("<mouse-2>" . widget-button-click)
         ("H" . browse-homepage)
         ("R" . restore-previous-session)
         ("L" . restore-session)
         ("q" . quit-dashboard)
         ("h" . dashboard-hydra/body)
         ("?" . dashboard-hydra/body))
  :hook (dashboard-mode . (lambda () (setq-local frame-title-format "")))
  :init
  (setq welcomes '("立志要如山，行道要如水，不如山，不能坚定，不如水，不能曲达。"
                   "人除了死亡，其他都是擦伤。"
                   "你可以打破常识，但不能没有常识。"
                   "你是在拜佛，还是在拜私欲？"
                   "你所有的迷茫，不过是清醒的看着自己沉沦。"
                   "你现在的生活不是你想要的，但绝对是你自找的。"
                   "世道如弈棋，变化不容覆。"
                   "别人就是最敏感的你，附托在另一个躯壳上。"
                   "好在，别人如何分析我，跟我本身是一点关系也没有的。"
                   "好朋友不是通过努力争取来的，而是在各自的道路上奔跑遇见的。"
                   "岁月悠悠，衰微只及肌肤；热忱抛却，颓废必致灵魂。"
                   "远离那些企图让你丧失雄心的人吧，小人经常如此，而真正的伟人，会让你觉得你也可以变得伟大。"
                   "不要嘲笑那些比你们拼命努力的人，也不要理会那些嘲笑你拼命努力的人。——松下幸之助"
                   "岁月骛过，山陵浸远。——《后汉书》"
                   "只要你见性志诚，念念回首处，即是灵山。"
                   "大风可以吹起一张白纸，却无法吹走一只蝴蝶，因为生命的力量在于不顺从。——冯骥才"
                   "繁花落尽，我心中仍留有花落的声音，一朵、一朵，在无人的山间轻轻飘落。——慕容熙"
                   "纸上得来终觉浅，绝知此事要躬行。——陆游"
                   "矮人看戏何曾见，都是随人说长短。——赵翼"
                   "如同明月将死那样生活，如同永远不死那样求知。——甘地"))
  (setq dashboard-banner-logo-title (nth (random (length welcomes)) welcomes)
        dashboard-startup-banner 'official
        dashboard-center-content t
        dashboard-show-shortcuts nil
        dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5))

        dashboard-set-init-info t
        dashboard-set-file-icons (display-graphic-p)
        dashboard-set-heading-icons (display-graphic-p)
        dashboard-heading-icons '((recents   . "file-text")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")
                                  (projects  . "file-directory")
                                  (registers . "database"))

        dashboard-set-footer t
        dashboard-footer-icon (cond ((display-graphic-p)
                                     (all-the-icons-faicon "heart"
                                                           :height 1.1
                                                           :v-adjust -0.05
                                                           :face 'error))
                                    ((char-displayable-p ?🧡) "🧡 ")
                                    (t (propertize ">" 'face 'dashboard-footer)))

        dashboard-set-navigator t
        dashboard-navigator-buttons
        `(((,(when (display-graphic-p)
               (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
            "Homepage" "Browse homepage"
            (lambda (&rest _) (browse-url "https://github.com/devbins/.emacs.d")))
           (,(when (display-graphic-p)
               (all-the-icons-material "restore" :height 1.35 :v-adjust -0.24))
            "Restore" "Restore previous session"
            (lambda (&rest _) (restore-previous-session)))
           (,(when (display-graphic-p)
               (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
            "Settings" "Open init file"
            (lambda (&rest _) (find-file (expand-file-name "init.el" user-emacs-directory))))
           (,(if (display-graphic-p)
                 (all-the-icons-faicon "question" :height 1.2 :v-adjust -0.1)
               "?")
            "" "Help (?/h)"
            (lambda (&rest _) (dashboard-hydra/body))
            font-lock-string-face))))

  (dashboard-setup-startup-hook)
  :config
  (defun my-banner-path (&rest _)
    "Return the full path to banner."
    (expand-file-name "banner.txt" user-emacs-directory))
  (advice-add #'dashboard-get-banner-path :override #'my-banner-path)

  ;; WORKAROUND: fix differnct background color of the banner image.
  ;; @see https://github.com/emacs-dashboard/emacs-dashboard/issues/203
  (defun my-dashboard-insert-image-banner (banner)
    "Display an image BANNER."
    (when (file-exists-p banner)
      (let* ((title dashboard-banner-logo-title)
             (spec (create-image banner))
             (size (image-size spec))
             (width (car size))
             (left-margin (max 0 (floor (- dashboard-banner-length width) 2))))
        (goto-char (point-min))
        (insert "\n")
        (insert (make-string left-margin ?\ ))
        (insert-image spec)
        (insert "\n\n")
        (when title
          (dashboard-insert-center (format "%s\n\n" (propertize title 'face 'dashboard-banner-logo-title)))))))
  (advice-add #'dashboard-insert-image-banner :override #'my-dashboard-insert-image-banner)

  ;; FIXME: Insert copyright
  ;; @see https://github.com/emacs-dashboard/emacs-dashboard/issues/219
  (defun my-dashboard-insert-copyright ()
    "Insert copyright in the footer."
    (when dashboard-set-footer
      (insert "\n  ")
      (dashboard-insert-center (propertize (format "\nPowered by devbins(1.01^365=37.8), %s\n" (format-time-string "%Y"))
                                           'face 'font-lock-comment-face))))
  (advice-add #'dashboard-insert-footer :after #'my-dashboard-insert-copyright)

  (defvar dashboard-recover-layout-p nil
    "Wether recovers the layout.")

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    ;; Check if need to recover layout
    (if (> (length (window-list-1))
           ;; exclude `treemacs' window
           (if (and (fboundp 'treemacs-current-visibility)
                  (eq (treemacs-current-visibility) 'visible))
               2
             1))
        (setq dashboard-recover-layout-p t))

    (delete-other-windows)

    ;; Refresh dashboard buffer
    (if (get-buffer dashboard-buffer-name)
        (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)

    ;; Jump to the first section
    (goto-char (point-min))
    (dashboard-goto-recent-files))

  (defun quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (when (and dashboard-recover-layout-p
             (bound-and-true-p winner-mode))
      (winner-undo)
      (setq dashboard-recover-layout-p nil)))

  (defun restore-previous-session ()
    "Restore the previous session."
    (interactive)
    (when (bound-and-true-p persp-mode)
      (restore-session persp-auto-save-fname)))

  (defun restore-session (fname)
    "Restore the specified session."
    (interactive (list (read-file-name "Load perspectives from a file: "
                                       persp-save-dir)))
    (when (bound-and-true-p persp-mode)
      (message "Restoring session...")
      (quit-window t)
      (condition-case-unless-debug err
          (persp-load-state-from-file fname)
        (error "Error: Unable to restore session -- %s" err))
      (message "Done")))

  (defun dashboard-goto-recent-files ()
    "Go to recent files."
    (interactive)
    (funcall (local-key-binding "r")))

  (defun dashboard-goto-projects ()
    "Go to projects."
    (interactive)
    (funcall (local-key-binding "p")))

  (defun dashboard-goto-bookmarks ()
    "Go to bookmarks."
    (interactive)
    (funcall (local-key-binding "m"))))

(provide 'init-dashboard)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
