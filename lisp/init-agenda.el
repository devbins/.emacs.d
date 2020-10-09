;;; init-agenda.el ---
;;
;; Filename: init-agenda.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Thu Oct  8 08:25:59 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 6
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

(use-package org-agenda
  :ensure nil
  :defer t
  :preface
  ;;Sunrise and Sunset
  ;;日出而作, 日落而息
  (defun diary-sunrise ()
    (let ((dss (diary-sunrise-sunset)))
      (with-temp-buffer
        (insert dss)
        (goto-char (point-min))
        (while (re-search-forward " ([^)]*)" nil t)
          (replace-match "" nil nil))
        (goto-char (point-min))
        (search-forward ",")
        (buffer-substring (point-min) (match-beginning 0)))))

  (defun diary-sunset ()
    (let ((dss (diary-sunrise-sunset))
          start end)
      (with-temp-buffer
        (insert dss)
        (goto-char (point-min))
        (while (re-search-forward " ([^)]*)" nil t)
          (replace-match "" nil nil))
        (goto-char (point-min))
        (search-forward ", ")
        (setq start (match-end 0))
        (search-forward " at")
        (setq end (match-beginning 0))
        (goto-char start)
        (capitalize-word 1)
        (buffer-substring start end))))

  (defun org-agenda-time-grid-spacing()
    "Set different line spacing based on clock time duration."
    (save-excursion
      (let* ((colors (cl-case (alist-get 'background-mode (frame-parameters))
		               ('light
		                (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7"))
		               ('dark
		                (list "#1ABC9C" "#2ECC71" "#3498D8" "#9966ff"))))
             pos
             duration)
        (nconc colors colors)
        (goto-char (point-min))
        (while (setq pos (next-single-property-change (point) 'duration))
          (goto-char pos)
          (when (and (not (equal pos (point-at-eol)))
                     (setq duration (org-get-at-bol 'duration)))
            ;; larger duration bar height
            (let ((line-height (if (< duration 15) 1.0 (+ 0.5 (/ duration 30))))
                  (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
              (overlay-put ov 'face `(:background ,(car colors) :foreground "black"))
              (setq colors (cdr colors))
              (overlay-put ov 'line-height line-height)
              (overlay-put ov 'line-spacing (1- line-height))))))))

  :hook (org-agenda-finalize . org-agenda-time-grid-spacing)
  :init
  (setq org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'current-window
        org-agenda-inhibit-startup t   ;; ~50x speedup
        org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        ;;https://sachachua.com/blog/2015/06/adding-calculations-based-on-time-to-the-org-agenda-clock-report/
        org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))
        org-agenda-use-time-grid t
        org-agenda-time-grid (quote ((daily today require-timed)
                                     (300 600 900 1200 1500 1800 2100 2400)
                                     "......" "----------------"))

        org-agenda-align-tags-to-column (- (- (/ (/ (display-pixel-width) 2) 10) 3))
        org-agenda-tags-column (- (- (/ (/ (display-pixel-width) 2) 10) 3))
        org-agenda-columns-add-appointments-to-effort-sum t
        org-agenda-prefix-format '((agenda . " %i %-12:c %? e %?-12t % s")
                                   (timeline . " % s")
                                   (effort . " %e %(or (org-entry-get (point) \"Effort\") \"0:00\")")
                                   (todo . " %i %-12:c")
                                   (search . " %i %-12:c")
                                   (tags . " %i %-12:c"))


        org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir)
        org-agenda-files (list org-agenda-dir)

        org-agenda-include-diary t
        org-agenda-show-log t
        org-agenda-log-mode-items '(clock)
        org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-span 'day
        org-agenda-sorting-strategy '((agenda habit-down time-up user-defined-up effort-up priority-down)
                                      (todo priority-down category-keep)
                                      (tags priority-down category-keep)))
  :config
  (require 'org-habit)
  (setq org-habit-show-done-always-green t
        ;; 减少显示天数，使其可以放在任务条的左边
        org-habit-graph-column 1
        org-habit-preceding-days 10
        org-habit-following-days 2
        ;; 恢复默认日历行为
        org-habit-show-habits-only-for-today nil)
  (add-to-list 'org-agenda-custom-commands
               '("r" "Daily Agenda Review"
                 ((agenda "" ((org-agenda-overriding-header "今日记录")
                              (org-agenda-span 'day)
                              (org-agenda-show-log 'clockcheck)
                              (org-agenda-start-with-log-mode nil)
                              (org-agenda-log-mode-items '(closed clock))
                              (org-agenda-clockreport-mode t)
                              )))))

  (evil-define-key 'normal org-agenda-mode-map
    (kbd "RET")    'org-agenda-switch-to
    "c"            'org-agenda-goto-calendar
    "d"            'org-agenda-day-view
    "e"            'org-agenda-set-effort
    "f"            'org-agenda-follow-mode
    "gd"           'org-agenda-toggle-time-grid
    "gh"           'org-agenda-holidays
    "gj"           'org-agenda-goto-date
    "gJ"           'org-agenda-clock-goto
    "gk"           'org-agenda-action
    "gm"           'org-agenda-bulk-mark
    "go"           'org-agenda-open-link
    "gO"           'delete-other-windows
    "gr"           'org-agenda-redo
    "gv"           'org-agenda-view-mode-dispatch
    "gw"           'org-agenda-week-view
    "g/"           'org-agenda-filter-by-tag
    "t"            'org-agenda-todo
    "h"            'org-agenda-earlier
    "i"            'org-agenda-diary-entry
    "j"            'org-agenda-next-line
    "k"            'org-agenda-previous-line
    "l"            'org-agenda-later
    "L"            'org-agenda-log-mode
    "R"            'org-agenda-clockreport-mode
    "M-j"          'org-agenda-next-item
    "M-k"          'org-agenda-previous-item
    "M-h"          'org-agenda-earlier
    "M-l"          'org-agenda-later
    "C-h" nil
    "q"            'org-agenda-quit)
  (evil-leader/set-key-for-mode 'org-agenda-mode
    "ma"                        'org-agenda
    "mCc"                       'org-agenda-clock-cancel
    "mCi"                       'org-agenda-clock-in
    "mCo"                       'org-agenda-clock-out
    "mCp"                       'org-pomodoro
    "mdd"                       'org-agenda-deadline
    "mds"                       'org-agenda-schedule
    "mie"                       'org-agenda-set-effort
    "mit"                       'org-agenda-set-tags
    "msr"                       'org-agenda-refile)

  (use-package appt
    :ensure nil
    :config
    (setq appt-time-msg-list nil
          appt-message-warning-time '10
          appt-display-mode-line t
          appt-display-format 'window
          appt-display-interval '5
          appt-audible t)
    (run-at-time "24:01" 3600 'org-agenda-to-appt)
    (defun appt-disp-window-and-notification (min-to-appt current-time appt-msg)
      (notify current-time appt-msg)) ;同时也调用原有的提醒函数
    (setq appt-disp-window-function 'appt-disp-window-and-notification)
    :hook ((org-agenda-finalize . org-agenda-to-appt)
           (after-init . appt-activate))))


(provide 'init-agenda)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-agenda.el ends here
