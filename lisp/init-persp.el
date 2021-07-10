;;; init-persp.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-persp.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:29:14 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 9
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

;; Windows/buffers sets shared among frames + save/load.
(use-package persp-mode
  :diminish
  :defines (recentf-exclude ivy-ignore-buffers)
  :functions persp--frame-parameter
  :commands (get-current-persp persp-contain-buffer-p)
  :hook ((after-init . persp-mode)
         (persp-mode . persp-load-frame)
         (kill-emacs . persp-save-frame))
  :init (setq persp-keymap-prefix (kbd "C-x p")
              persp-nil-name "default"
              persp-set-last-persp-for-new-frames nil
              persp-kill-foreign-buffer-behaviour 'kill
              persp-auto-resume-time 0)
  :config
  ;; Save and load frame parameters (size & position)
  (defvar persp-frame-file (expand-file-name "persp-frame" persp-save-dir)
    "File of saving frame parameters.")

  (defun persp-save-frame ()
    "Save the current frame parameters to file."
    (interactive)
    (when (and (display-graphic-p) persp-mode)
      (condition-case error
          (with-temp-buffer
            (erase-buffer)
            (insert
             ";;; -*- mode: emacs-lisp; coding: utf-8-unix -*-\n"
             ";;; This is the previous frame parameters.\n"
             ";;; Last generated " (current-time-string) ".\n"
             "(setq initial-frame-alist\n"
             (format "      '((top . %d)\n" (frame-parameter nil 'top))
             (format "        (left . %d)\n" (frame-parameter nil 'left))
             (format "        (width . %d)\n" (frame-parameter nil 'width))
             (format "        (height . %d)\n" (frame-parameter nil 'height))
             (format "        (fullscreen . %s)))\n" (frame-parameter nil 'fullscreen)))
            (when (file-writable-p persp-frame-file)
              (write-file persp-frame-file)))
        (error
         (warn "persp frame: %s" (error-message-string error))))))

  (defun persp-load-frame ()
    "Load frame with the previous frame's geometry."
    (interactive)
    (when (and (display-graphic-p) persp-mode)
      (fix-fullscreen-cocoa)
      (when (file-readable-p persp-frame-file)
        (condition-case error
            (progn
              (load persp-frame-file)

              ;; Handle multiple monitors gracefully
              (when (>= (eval (frame-parameter nil 'left)) (display-pixel-width))
                (set-frame-parameter nil 'left 0))
              (when (>= (eval (frame-parameter nil 'top)) (display-pixel-height))
                (set-frame-parameter nil 'top 0)))
          (error
           (warn "persp frame: %s" (error-message-string error)))))))

  ;; Don't save if the sate is not loaded
  (with-no-warnings
    (defvar persp-state-loaded nil
      "Whether the state is loaded.")

    (defun my-persp-after-load-state (&rest _)
      (setq persp-state-loaded t))
    (advice-add #'persp-load-state-from-file :after #'my-persp-after-load-state)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (add-hook 'find-file-hook #'my-persp-after-load-state)))

    (defun my-persp-asave-on-exit (fn &optional interactive-query)
      (if persp-state-loaded
          (funcall fn interactive-query)
        t))
    (advice-add #'persp-asave-on-exit :around #'my-persp-asave-on-exit))

  ;; Don't save dead or temporary buffers
  (add-hook 'persp-filter-save-buffers-functions
               (lambda (b)
                 "Ignore dead buffers."
                 (not (buffer-live-p b))))
  (add-hook 'persp-filter-save-buffers-functions
               (lambda (b)
                 "Ignore temporary buffers."
                 (let ((bname (file-name-nondirectory (buffer-name b))))
                   (or (string-prefix-p ".newsrc" bname)
                       (string-prefix-p "magit" bname)
                       (string-prefix-p "COMMIT_EDITMSG" bname)
                       (string-prefix-p "Pfuture-Callback" bname)
                       (string-match-p "\\.elc\\|\\.tar\\|\\.gz\\|\\.zip\\'" bname)
                       (string-match-p "\\.bin\\|\\.so\\|\\.dll\\|\\.exe\\'" bname)
                       (eq (buffer-local-value 'major-mode b) 'erc-mode)
                       (eq (buffer-local-value 'major-mode b) 'rcirc-mode)
                       (eq (buffer-local-value 'major-mode b) 'nov-mode)
                       (eq (buffer-local-value 'major-mode b) 'vterm-mode)))))

  ;; Don't save persp configs in `recentf'
  (with-eval-after-load 'recentf
    (push persp-save-dir recentf-exclude))

  ;; Ivy Integraticon
  (with-eval-after-load 'ivy
    (add-to-list 'ivy-ignore-buffers
                 #'(lambda (b)
                     (when persp-mode
                       (let ((persp (get-current-persp)))
                         (if persp
                             (not (persp-contain-buffer-p b persp))
                           nil))))))

  ;; Eshell integration
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))

  ;; Shell integration
  (persp-def-buffer-save/load
   :mode 'shell-mode :tag-symbol 'def-shell-buffer
   :mode-restore-function (lambda (_) (shell))
   :save-vars '(major-mode default-directory)))

;; Projectile integration
(use-package persp-mode-projectile-bridge
  :commands (persp-mode-projectile-bridge-find-perspectives-for-all-buffers
             persp-mode-projectile-bridge-kill-perspectives)
  :hook ((persp-mode . persp-mode-projectile-bridge-mode)
         (persp-mode-projectile-bridge-mode
          .
          (lambda ()
            (if persp-mode-projectile-bridge-mode
                (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
              (persp-mode-projectile-bridge-kill-perspectives)))))
  :init (setq persp-mode-projectile-bridge-persp-name-prefix "[p]")
  :config
  ;; HACK: Allow saving to files
  (with-no-warnings
    (defun my-persp-mode-projectile-bridge-add-new-persp (name)
      (let ((persp (persp-get-by-name name *persp-hash* :nil)))
        (if (eq :nil persp)
            (prog1
                (setq persp (persp-add-new name))
              (when persp
                (set-persp-parameter 'persp-mode-projectile-bridge t persp)
                (persp-add-buffer (projectile-project-buffers)
                                  persp nil nil)))
          persp)))
    (advice-add #'persp-mode-projectile-bridge-add-new-persp
                :override #'my-persp-mode-projectile-bridge-add-new-persp)))

(provide 'init-persp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-persp.el ends here
