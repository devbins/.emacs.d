;;; init-ui.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-ui.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:19:57 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 170
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


;; Logo
(setq fancy-splash-image nil)

;; Title
(setq frame-title-format '("Emacs - %b")
      icon-title-format frame-title-format)

(when sys/mac-x-p
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Menu/Tool/Scroll bars
(unless emacs/>=27p
  (push '(vertical-scroll-bars) default-frame-alist))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Theme
(use-package doom-themes
  :custom-face
  (doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
  :custom
  (doom-themes-treemacs-theme "doom-colors")
  :init (load-theme 'doom-one)
  :config
  (doom-themes-treemacs-config))

;; Mode-line
(use-package doom-modeline
  :custom
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-minor-modes t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-mu4e nil)
  :hook (after-init . doom-modeline-mode)
  :init
  ;; prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq doom-modeline--old-format mode-line-format)
    (setq-default mode-line-format nil)))

;; A minor-mode menu for mode-line
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; Icons
;; NOTE: Must run `M-x all-the-icons-install-fonts', and install fonts manually on Windows
(use-package all-the-icons
  :if (display-graphic-p)
  :init (unless (or sys/win32p (font-installed-p "all-the-icons"))
          (all-the-icons-install-fonts t)))

;; Show native line numbers if possible, otherwise use `linum'
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :config (setq display-line-numbers-type 'relative)
      :hook (prog-mode . display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :init (setq linum-format "%4d ")
    :config
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
      :hook (global-linum-mode . hlinum-activate)
      :init (setq linum-highlight-in-all-buffersp t))))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

(when sys/macp
  ;; Render thinner fonts
  (setq ns-use-thin-smoothing t)
  ;; Don't open a file in a new frame
  (setq ns-pop-up-frames nil))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;;beacon
(use-package beacon
  :defer t
  :if (display-graphic-p)
  :diminish
  :hook (after-init . beacon-mode)
  :config
  ;; only flash on window/buffer changes...
  (setq beacon-blink-when-window-changes t
        ;; ... don't be excessive:
        beacon-blink-when-point-moves-vertically nil
        beacon-blink-duration .5       ; default .3
        beacon-blink-delay .2          ; default .3
        beacon-size 20))

(defvar active-transparency 80
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's active or selected. Transparency
can be toggled through `toggle-transparency'.")

(defvar inactive-transparency 75
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's inactive or deselected. Transparency
can be toggled through `toggle-transparency'.")


(defun toggle-transparency (&optional frame)
  "Toggle between transparent and opaque state for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let ((alpha (frame-parameter frame 'alpha))
        (dotfile-setting (cons active-transparency
                               inactive-transparency)))
    (if (equal alpha dotfile-setting)
        (disable-transparency frame)
      (enable-transparency frame dotfile-setting))))

(defun enable-transparency (&optional frame alpha)
  "Enable transparency for FRAME.
If FRAME is nil, it defaults to the selected frame.
ALPHA is a pair of active and inactive transparency values. The
default value for ALPHA is based on
`active-transparency' and
`inactive-transparency'."
  (interactive)
  (let ((alpha-setting (or alpha
                          (cons active-transparency
                                inactive-transparency))))
    (set-frame-parameter frame 'alpha alpha-setting)))

(defun disable-transparency (&optional frame)
  "Disable transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (set-frame-parameter frame 'alpha '(100 . 100)))

(defun increase-transparency (&optional frame)
  "Increase transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let* ((current-alpha (or (car (frame-parameter frame 'alpha)) 100))
         (increased-alpha (- current-alpha 5)))
    (when (>= increased-alpha frame-alpha-lower-limit)
      (set-frame-parameter frame 'alpha
                           (cons increased-alpha increased-alpha)))))

(defun decrease-transparency (&optional frame)
  "Decrease transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let* ((current-alpha (or (car (frame-parameter frame 'alpha)) 100))
         (decreased-alpha (+ current-alpha 5)))
    (when (<= decreased-alpha 100)
      (set-frame-parameter frame 'alpha
                           (cons decreased-alpha decreased-alpha)))))

;; Fonts
(when (display-graphic-p)
  ;; Set default font
  (cl-loop for font in '("SF Mono" "JetBrains Mono" "Source Code Pro" "Fira Code"
                      "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil :font
                                  (format "%s:pixelsize=%d" font 16)))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Symbols" "Symbola" "Symbol" "icons-in-terminal")
           when (font-installed-p font)
           return (set-fontset-font t 'unicode font nil 'prepend))

  (cl-loop for font in '("Apple Color Emoji")
           when (font-installed-p font)
           return (set-fontset-font t 'symbol font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("STKaiti" "WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (dolist (charset '(kana han symbol cjk-misc bopomofo))
                (set-fontset-font (frame-parameter nil 'font) charset
                                  (font-spec :family font)))))

(add-to-list 'default-frame-alist '(alpha . (80 . 75)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Display dividers between windows
(setq window-divider-default-places 'right-only
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; fix chinese & english can't sync scale
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode))

(use-package mixed-pitch
  :diminish)

(use-package zone
  :ensure nil
  :defer 5
  :config
  (zone-when-idle 600) ; in seconds
  (defun zone-choose (pgm)
    "Choose a PGM to run for `zone'."
    (interactive
     (list
      (completing-read
       "Program: "
       (mapcar 'symbol-name zone-programs))))
    (let ((zone-programs (list (intern pgm))))
      (zone))))

(use-package shrface
  :config
  (shrface-basic)
  (shrface-trial)
  (setq shrface-href-versatile t))

(provide 'init-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
