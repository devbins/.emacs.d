;;; init-shell.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-shell.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:31:26 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 11
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

(use-package shell
  :ensure nil
  :hook ((shell-mode . my-shell-mode-hook)
         (comint-output-filter-functions . comint-strip-ctrl-m))
  :init
  (setq system-uses-terminfo nil)

  (with-no-warnings
    (defun my-shell-simple-send (proc command)
      "Various PROC COMMANDs pre-processing before sending to shell."
      (cond
       ;; Checking for clear command and execute it.
       ((string-match "^[ \t]*clear[ \t]*$" command)
        (comint-send-string proc "\n")
        (erase-buffer))
       ;; Checking for man command and execute it.
       ((string-match "^[ \t]*man[ \t]*" command)
        (comint-send-string proc "\n")
        (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
        (setq command (replace-regexp-in-string "[ \t]+$" "" command))
        ;;(message (format "command %s command" command))
        (funcall 'man command))
       ;; Send other commands to the default handler.
       (t (comint-simple-send proc command))))

    (defun my-shell-mode-hook ()
      "Shell mode customizations."
      (local-set-key '[up] 'comint-previous-input)
      (local-set-key '[down] 'comint-next-input)
      (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)

      (ansi-color-for-comint-mode-on)
      (setq comint-input-sender 'my-shell-simple-send))))

;; ANSI & XTERM 256 color support
(use-package xterm-color
  :defines (compilation-environment
            eshell-preoutput-filter-functions
            eshell-output-filter-functions)
  :functions (compilation-filter my-advice-compilation-filter)
  :init
  ;; For shell and interpreters
  (setenv "TERM" "xterm-256color")
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled
              (make-local-variable 'font-lock-function)
              (setq font-lock-function #'ignore)))

  ;; For compilation buffers
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my-advice-compilation-filter (f proc string)
    (funcall f proc
             (if (eq major-mode 'rg-mode) ; compatible with `rg'
                 string
               (xterm-color-filter string))))
  (advice-add 'compilation-filter :around #'my-advice-compilation-filter)
  (advice-add 'gud-filter :around #'my-advice-compilation-filter))

;; Better term
;; @see https://github.com/akermu/emacs-libvterm#installation
(when (and module-file-suffix           ; dynamic module
           (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm
    :bind (:map vterm-mode-map
           ([f9] . (lambda ()
                     (interactive)
                     (and (fboundp 'shell-pop-toggle)
                        (shell-pop-toggle)))))
    :init (setq vterm-always-compile-module t)))

;; Shell Pop: leverage `popper'
(with-no-warnings
  (defvar shell-pop--frame nil)
  (when (childframe-workable-p)
    (defun shell-pop-posframe-hidehandler (_)
      "Hidehandler used by `shell-pop-posframe-toggle'."
      (not (eq (selected-frame) posframe--frame)))

    (defun shell-pop--shell (&optional arg)
      "Get shell buffer."
      (cond ((fboundp 'vterm) (vterm arg))
            (sys/win32p (eshell arg))
            (t (shell))))

    (defun shell-pop-posframe-toggle ()
      "Toggle shell in child frame."
      (interactive)
      (let* ((buffer (shell-pop--shell 100))
             (window (get-buffer-window buffer)))
        ;; Hide window
        (when (window-live-p window)
          (delete-window window))

        (if (and (frame-live-p shell-pop--frame)
                 (frame-visible-p shell-pop--frame))
            (progn
              ;; Hide child frame and refocus in parent frame
              (make-frame-invisible shell-pop--frame)
              (select-frame-set-input-focus (frame-parent shell-pop--frame))
              (setq shell-pop--frame nil))
          (let ((width  (max 80 (floor (* (frame-width) 0.5))))
                (height (floor (* (frame-height) 0.5))))
            ;; Shell pop in child frame
            (setq shell-pop--frame
                  (posframe-show
                   buffer
                   :poshandler #'posframe-poshandler-frame-center
                   :hidehandler #'shell-pop-posframe-hidehandler
                   :left-fringe 8
                   :right-fringe 8
                   :width width
                   :height height
                   :min-width width
                   :min-height height
                   :internal-border-width 3
                   :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                   :background-color (face-background 'tooltip nil t)
                   :override-parameters '((cursor-type . t))
                   :accept-focus t))

            ;; Focus in child frame
            (select-frame-set-input-focus shell-pop--frame)

            (with-current-buffer buffer
              (setq-local cursor-type 'box) ; blink cursor
              (goto-char (point-max))
              (when (fboundp 'vterm-reset-cursor-point)
                (vterm-reset-cursor-point)))))))
    (bind-key "C-`" #'shell-pop-posframe-toggle))

  (defvar shell-pop--window nil)
  (defun shell-pop-toggle ()
    "Toggle shell."
    (interactive)
    ;; Hide child frame
    (unless (and (frame-live-p shell-pop--frame)
                 (frame-visible-p shell-pop--frame))
      (if (window-live-p shell-pop--window)
          (progn
            (delete-window shell-pop--window)
            (setq shell-pop--window nil))
        (setq shell-pop--window
              (get-buffer-window (shell-pop--shell))))))
  (bind-key [f9] #'shell-pop-toggle))

(use-package aweshell
  :quelpa (aweshell :fetcher github :repo "manateelazycat/aweshell")
  :commands (aweshell-new aweshell-dedicated-open)
  :bind
  (("M-#" . aweshell-dedicated-open)
   (:map eshell-mode-map ("M-#" . aweshell-dedicated-close))))

(provide 'init-shell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
