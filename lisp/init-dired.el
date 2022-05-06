;;; init-dired.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-dired.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:26:34 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 24
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

;; Directory operations
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ("C-c C-p" . wdired-change-to-wdired-mode)
         ("e" . wdired-change-to-wdired-mode))
  :config
  (evil-define-key 'normal dired-mode-map
    "gg" 'evil-goto-first-line
    "G" 'evil-goto-line)
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  (put 'dired-find-alternate-file 'disabled nil)  ;; 只有一个buffer
  (when sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when (executable-find "gls")
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (when (or (and sys/macp (executable-find "gls"))
           (and (not sys/macp) (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)

    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first")

    ;; Quick sort dired buffers via hydra
    (use-package dired-quick-sort
      :bind (:map dired-mode-map
             ("S" . hydra-dired-quick-sort/body))
      :config
      (evil-define-key 'normal dired-mode-map
        "S" 'hydra-dired-quick-sort/body)))

  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
           (")" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
  (use-package dired-rsync
    :bind (:map dired-mode-map
           ("C-c C-r" . dired-rsync)))

  (use-package dired-subtree
    :bind (:map dired-mode-map
           ("TAB" . dired-subtree-cycle)))

  ;; Colourful dired
  (use-package diredfl
    :init (diredfl-global-mode 1))

  ;; Extra Dired functionality
  (use-package dired-aux :ensure nil)
  (use-package dired-x
    :ensure nil
    :demand
    :config
    (let ((cmd (cond
                (sys/mac-x-p "open")
                (sys/linux-x-p "xdg-open")
                (sys/win32p "start")
                (t ""))))
      (setq dired-guess-shell-alist-user
            `(("\\.pdf\\'" ,cmd)
              ("\\.docx\\'" ,cmd)
              ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))

    (setq dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

(use-package dired-async
  :ensure async
  :diminish dired-async-mode
  :hook (dired-mode-hook . dired-async-mode))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

(use-package image-dired
  :ensure nil
  :commands (image-dired)
  :config
  (setq image-dired-dir (expand-file-name "image-dired" user-emacs-directory)
        image-dired-thumbnail-storage 'standard)
  (evil-define-key 'normal image-dired-thumbnail-mode-map
    "j" 'image-dired-next-line
    "k" 'image-dired-previous-line
    "l" 'image-dired-forward-image
    "h" 'image-dired-backward-image
    "q" 'image-dired-kill-buffer-and-window
    "RET" 'image-dired-display-thumbnail-original-image))

(use-package image-mode
  :ensure nil
  :config (evil-define-key 'motion image-mode-map
            "h" 'image-backward-hscroll
            "j" 'image-next-file
            "k" 'image-previous-file
            "l" 'image-forward-hscroll
            "=" 'image-mode-fit-frame
            "x" 'image-transform-reset
            "h" 'image-transform-fit-to-height
            "w" 'image-transform-fit-to-width
            "s" 'image-transform-set-scale))

(use-package dirvish
  :custom
  ;; Feel free to replace `all-the-icons' with `vscode-icon'.
  (dirvish-attributes '(expanded-state all-the-icons file-size))
  ;; Maybe the icons are too big to your eyes
  ;; (dirvish-all-the-icons-height 0.8)

  ;; List directories that has over 10000 files asynchronously
  ;; This feature is disabled by default
  ;; (dirvish-async-listing-threshold 10000)
  ;; Place this line under :init to ensure the overriding at startup, see #22
  (dirvish-side-follow-buffer-file nil)
  (dirvish-override-dired-mode)
  :config
  (dirvish-peek-mode)
  ;; In case you want the details at startup like `dired'
  ;; :hook
  ;; (dirvish-mode . (lambda () (dired-hide-details-mode -1)))
  :bind
  (nil ; Bind `dirvish', `dirvish-dired' and `dirvish-side' as you see fit
   :map dired-mode-map
   ("SPC" . dirvish-show-history)
   ("r"   . dirvish-roam)
   ("b"   . dirvish-goto-bookmark)
   ("f"   . dirvish-file-info-menu)
   ("M-a" . dirvish-mark-actions-menu)
   ("M-s" . dirvish-setup-menu)
   ("M-f" . dirvish-toggle-fullscreen)
   ([remap dired-summary] . dirvish-dispatch)
   ([remap dired-do-copy] . dirvish-yank)
   ([remap mode-line-other-buffer] . dirvish-other-buffer)))

(provide 'init-dired)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
