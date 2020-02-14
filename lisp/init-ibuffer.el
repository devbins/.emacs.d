;;; init-ibuffer.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-ibuffer.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:28:00 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 2
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

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  (with-no-warnings
    ;; FIXME: delete when `all-the-icons-ibuffer' is available
    ;; Display buffer icons on GUI
    (when (display-graphic-p)
      ;; For alignment, the size of the name field should be the width of an icon
      (define-ibuffer-column icon (:name "  ")
        (let ((icon (if (and (buffer-file-name)
                             (all-the-icons-auto-mode-match?))
                        (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                      (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
          (if (symbolp icon)
              (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
            icon)))

      (setq ibuffer-formats `((mark modified read-only ,(if emacs/>=26p 'locked "")
                                    ;; Here you may adjust by replacing :right with :center or :left
                                    ;; According to taste, if you want the icon further from the name
                                    " " (icon 2 2 :left :elide)
                                    ,(propertize " " 'display `(space :align-to 8))
                                    (name 18 18 :left :elide)
                                    " " (size 9 -1 :right)
                                    " " (mode 16 16 :left :elide) " " filename-and-process)
                              (mark " " (name 16 -1) " " filename))))

    (with-eval-after-load 'counsel
      (defun my-ibuffer-find-file ()
        (interactive)
        (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                   (if (buffer-live-p buf)
                                       (with-current-buffer buf
                                         default-directory)
                                     default-directory))))
          (counsel-find-file default-directory)))
      (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file)))

  ;; Group ibuffer's list by project root
  (use-package ibuffer-projectile
    :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
    :hook ((ibuffer . (lambda ()
                        (ibuffer-projectile-set-filter-groups)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic)))))
    :config
    (setq ibuffer-projectile-prefix
          (if (display-graphic-p)
              (concat
               (all-the-icons-octicon "file-directory"
                                      :face ibuffer-filter-group-name-face
                                      :v-adjust -0.05
                                      :height 1.25)
               " ")
            "Project: "))))


(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))


(defun switch-to-scratch-buffer (&optional arg)
  "Switch to the `*scratch*' buffer, creating it first if needed.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (let ((exists (get-buffer "*scratch*")))
    (if arg
        (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
      (switch-to-buffer (get-buffer-create "*scratch*")))
    (when (and (not exists)
               (not (eq major-mode 'text-mode))
               (fboundp 'text-mode))
      (funcall 'text-mode))))

(provide 'init-ibuffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here
