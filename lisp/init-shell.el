;;; init-shell.el --- -*- lexical-binding: t no-byte-compile: t; -*-
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
;;     Update #: 26
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
    :init (setq vterm-always-compile-module t))

  (use-package multi-vterm
    :bind ("C-<f9>" . multi-vterm)
    :custom (multi-vterm-buffer-name "vterm")
    :config
    (with-no-warnings
      ;; Use `pop-to-buffer' instead of `switch-to-buffer'
      (defun my-multi-vterm ()
        "Create new vterm buffer."
        (interactive)
        (let ((vterm-buffer (multi-vterm-get-buffer)))
          (setq multi-vterm-buffer-list
                (nconc multi-vterm-buffer-list (list vterm-buffer)))
          (set-buffer vterm-buffer)
          (multi-vterm-internal)
          (pop-to-buffer vterm-buffer)))
      (advice-add #'multi-vterm :override #'my-multi-vterm))))

;; Better terminal emulator
(unless sys/win32p
  (use-package ghostel
    :hook (eshell-load . ghostel-eshell-visual-command-mode)
    :config (use-package evil-ghostel
              :after (ghostel evil)
              :hook (ghostel-mode . evil-ghostel-mode))))

;; Shell Pop: leverage `popper'
(with-no-warnings
  (defvar shell-pop--frame nil)
  (defvar shell-pop--window nil)
  (defvar shell-pop--buffer nil)

  (defun shell-pop--reset ()
    "Reset shell-pop."
    (when shell-pop--frame
      (delete-frame shell-pop--frame))
    (setq shell-pop--buffer nil
          shell-pop--window nil
          shell-pop--frame nil))

  (defun shell-pop--reset-cursor-point ()
    "Reset cursor point."
    (with-current-buffer shell-pop--buffer
      (goto-char (point-max))

      (when (derived-mode-p 'ghostel-mode)
        (ghostel-send-key "down"))))

  (defun shell-pop--shell (&optional arg)
    "Run shell and return the buffer."
    (setq shell-pop--buffer
          (cond ((fboundp 'ghostel) (ghostel arg))
                (sys/win32p (eshell arg))
                (t (shell))))
    (when (and shell-pop--buffer
               (buffer-live-p shell-pop--buffer))
      (sleep-for 0.2)                   ; wait for shell-ready
      (setq shell-pop--window (get-buffer-window shell-pop--buffer))
      (add-hook 'kill-buffer-hook #'shell-pop--reset t)))

  (defun shell-pop--hide-window ()
    "Hide shell window."
    (when (and shell-pop--window
               (window-live-p shell-pop--window)
               shell-pop--window
               (get-buffer-window (buffer-name shell-pop--buffer) 'visible))
      (delete-window shell-pop--window)))

  (defun shell-pop--hide-frame ()
    "Hide child frame and refocus in parent frame."
    (when (and shell-pop--frame
               (frame-live-p shell-pop--frame)
               (frame-visible-p shell-pop--frame))
      (make-frame-invisible shell-pop--frame)
      (select-frame-set-input-focus (frame-parent shell-pop--frame))))

  (defun shell-pop-window-toggle ()
    "Toggle shell in a split window."
    (interactive)
    (shell-pop--hide-frame)
    (if (and shell-pop--buffer
             (get-buffer-window (buffer-name shell-pop--buffer) 'visible))
        (shell-pop--hide-window)
      (shell-pop--shell)))

  ;; Shell Pop in a child frame
  (defun shell-pop-posframe-hidehandler (_)
    "Hidehandler used by `shell-pop-posframe-toggle'."
    (let ((parent (and shell-pop--frame
                       (frame-live-p shell-pop--frame)
                       (frame-parent shell-pop--frame))))
      (and (frame-live-p shell-pop--frame)
           (frame-visible-p shell-pop--frame)
           (not (active-minibuffer-window))
           (not (memq (selected-frame) (list shell-pop--frame parent))))))

  (defun shell-pop-posframe-toggle ()
    "Toggle shell in child frame."
    (interactive)
    (if (and shell-pop--frame
             (frame-live-p shell-pop--frame)
             (frame-visible-p shell-pop--frame))
        (shell-pop--hide-frame)
      (let ((width  (max 100 (round (* (frame-width) 0.62))))
            (height (round (* (frame-height) 0.62))))
        ;; Create shell
        (shell-pop--shell)

        (when (and shell-pop--buffer (buffer-live-p shell-pop--buffer))
          ;; Bury `shell-pop--buffer'
          (when (and shell-pop--window
                     (get-buffer-window (buffer-name shell-pop--buffer) 'visible))
            (switch-to-prev-buffer shell-pop--window))
          (shell-pop--hide-window)

          ;; Pop shell in child frame
          (setq shell-pop--frame
                (posframe-show
                 shell-pop--buffer
                 :cursor 'box
                 :poshandler #'posframe-poshandler-frame-center
                 :hidehandler #'shell-pop-posframe-hidehandler
                 :left-fringe 8
                 :right-fringe 8
                 :width width
                 :height height
                 :min-width width
                 :min-height height
                 :internal-border-width 3
                 :internal-border-color (face-background 'region nil t)
                 :background-color (face-background 'default nil t)
                 :foreground-color (face-foreground 'default nil t)
                 :override-parameters '((minibuffer . nil))
                 :tty-non-selected-cursor t
                 :accept-focus t))

          ;; Delete the child frames of `shell-pop--frame'
          (when (and shell-pop--frame (frame-live-p shell-pop--frame))
            (dolist (frame (frame-list))
              (when (eq (frame-parent frame) shell-pop--frame)
                (delete-frame frame))))

          ;; Focus in child frame
          (select-frame-set-input-focus shell-pop--frame)))))

  (defun shell-pop-toggle ()
    "Toggle shell in a split window or child frame."
    (interactive)
    ;; Don't use `childframe-workable-p' here!!!
    (if (or (display-graphic-p)
            (featurep 'tty-child-frames))
        (shell-pop-posframe-toggle)
      (shell-pop-window-toggle)))

  (bind-keys ("C-`"    . shell-pop-toggle)
             ("<f9>"   . shell-pop-toggle)
             ("C-<f9>" . shell-pop-window-toggle)))

(use-package aweshell
  :load-path "site-lisp/aweshell"
  :commands (aweshell-new aweshell-dedicated-open)
  :config
  ;; 在Emacs里输入vi，直接在buffer里打开文件
  (defalias 'eshell/vi 'find-file)
  (defalias 'eshell/vim 'find-file)
  :bind
  (("M-#" . aweshell-dedicated-open)
   (:map eshell-mode-map ("M-#" . aweshell-dedicated-close))))

(provide 'init-shell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
