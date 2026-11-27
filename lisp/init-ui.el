;;; init-ui.el --- -*- lexical-binding: t no-byte-compile: t; -*-
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
;;     Update #: 302
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

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Title
(setq frame-title-format '("Emacs - %b")
      icon-title-format frame-title-format)

(when sys/mac-x-p
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (if (display-graphic-p)
                  (menu-bar-mode 1)
                (menu-bar-mode -1))))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Theme
(use-package doom-themes
  :custom-face
  (doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
  :custom
  (doom-themes-treemacs-theme "doom-colors")
  :init (load-theme 'doom-one)
  :config
  (when (featurep 'nerd-icons)
    (with-eval-after-load 'lsp-treemacs
      (doom-themes-treemacs-config))))

(use-package flucui-themes)

;; Mode-line
;; (use-package doom-modeline
;;   :hook (after-init . doom-modeline-mode)
;;   :init
;;   (setq doom-modeline-icon (display-graphic-p)
;;         doom-modeline-minor-modes t)
;;   ;; prevent flash of unstyled modeline at startup
;;   (unless after-init-time
;;     (setq-default mode-line-format nil)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon (display-graphic-p)
        doom-modeline-minor-modes t
        doom-modeline-modal nil
        doom-modeline-time-icon nil
        doom-modeline-lsp nil
        doom-modeline-vcs-max-length 200
        doom-modeline-buffer-file-name-style 'buffer-name
        doom-modeline-enable-word-count nil)
  (unless after-init-time
    (setq-default mode-line-format nil))
  :config
  (require 'vc-git)

  ;; 字号：改这里即可整体缩放 modeline
  (defconst my/ml-size 18)
  ;; ---- 配色 ----
  (defvar my-ml-palette
    '((normal   "#7aa2f7" "black")
      (insert   "#9ece6a" "black")
      (visual   "#e0af68" "black")
      (replace  "#f7768e" "black")
      (motion   "#bb9af7" "black")
      (filename "#3b4261" "#7aa2f7")
      (modified "#3b4261" "#e0af68")
      (git      "#3b4261" "#7aa2f7")
      (time     "#7aa2f7" "black"))
    "modeline 各段配色.")

  (defun my-ml-bg (r) (nth 1 (assq r my-ml-palette)))

  (defun my-ml-fg (r) (nth 2 (assq r my-ml-palette)))

  (defun my-ml-icons-p ()
    (and (display-graphic-p)
         (bound-and-true-p doom-modeline-icon)
         (fboundp 'nerd-icons-mdicon)))

  ;; ---- face / 字体（全部显式 slant normal 压掉斜体）----
  (defun my-ml-set-faces ()
    (dolist (r '(normal insert visual replace motion))
      (set-face-attribute (intern (format "doom-modeline-evil-%s-state" r)) nil
                          :inherit 'doom-modeline :weight 'bold :slant 'normal
                          :background (my-ml-bg r) :foreground (my-ml-fg r)))
    (dolist (p '((doom-modeline-buffer-file . filename)
                 (doom-modeline-buffer-modified . modified)
                 (doom-modeline-git-state . git)
                 (doom-modeline-time-state . time)))
      (unless (facep (car p)) (make-face (car p)))
      (set-face-attribute (car p) nil :inherit 'doom-modeline :weight 'bold :slant 'normal
                          :background (my-ml-bg (cdr p))
                          :foreground (my-ml-fg (cdr p))))
    (dolist (f '(mode-line mode-line-inactive))
      (if (display-graphic-p)
          (set-face-attribute f nil
                              :family "JetBrains Mono"
                              :height (* my/ml-size 10)
                              :weight 'normal :slant 'normal
                              :box nil :underline nil)
        (set-face-attribute f nil
                            :family "JetBrains Mono"
                            :height (* my/ml-size 10)
                            :weight 'normal :slant 'normal
                            :box nil :underline nil)))

    (set-face-attribute 'doom-modeline nil :slant 'normal :weight 'normal)

    ;; 箭头取色用的 face
    (dolist (r '(normal insert visual replace motion filename modified git time base inactive))
      (let* ((face (intern (format "my-ml-arrow-%s" r)))
             (bg (pcase r
                   ('base (or (face-background 'mode-line nil t) "#000000"))
                   ('inactive (or (face-background 'mode-line-inactive nil t) "#000000"))
                   (_ (my-ml-bg r)))))
        (unless (facep face) (make-face face))
        (set-face-attribute face nil :inherit 'doom-modeline
                            :inverse-video nil :foreground "black"
                            :background bg :weight 'bold :slant 'normal))))
  (my-ml-set-faces)
  (advice-add 'enable-theme :after (lambda (&rest _) (my-ml-set-faces)))
  ;; ---- 纯文本三角箭头（GUI/TTY 一致）----
  (defun my-ml-face-bg (face)
    (let ((c (face-background face nil t)))
      (if (and (stringp c) (not (string-prefix-p "unspecified" c))) c "#000000")))
  (defun my-ml-tri-left (f1 f2)
    "左→右三角 U+E0B0：前景取 F1 背景色，底色取 F2 背景色。"
    (propertize "" 'face
                (list :inherit 'doom-modeline :slant 'normal :weight 'bold
                      :foreground (my-ml-face-bg f1)
                      :background (my-ml-face-bg f2))))
  (defun my-ml-tri-right (f1 f2)
    "右→左三角 U+E0B2：底色取 F1 背景色，前景取 F2 背景色。"
    (propertize "" 'face
                (list :inherit 'doom-modeline :slant 'normal :weight 'bold
                      :foreground (my-ml-face-bg f2)
                      :background (my-ml-face-bg f1))))
  (defun my-ml-sep (dir f1 f2)
    (if (eq dir 'left) (my-ml-tri-left f1 f2) (my-ml-tri-right f1 f2)))
  (defun my-ml-sep-gap (left-face right-face dir)
    "带 mode-line 底色间隔的两段箭头。"
    (if (eq dir 'left)
        (concat (my-ml-tri-left left-face (my-ml-base-arrow-face))
                (my-ml-tri-left (my-ml-base-arrow-face) right-face))
      (concat (my-ml-tri-right left-face (my-ml-base-arrow-face))
              (my-ml-tri-right (my-ml-base-arrow-face) right-face))))
  (defun my-ml-base-face ()
    (if (doom-modeline--active) 'mode-line 'mode-line-inactive))
  (defun my-ml-base-arrow-face ()
    (if (doom-modeline--active) 'my-ml-arrow-base 'my-ml-arrow-inactive))
  (defun my-ml-state-arrow-face ()
    (if (doom-modeline--active)
        (pcase (and (boundp 'evil-state) evil-state)
          ('insert  'my-ml-arrow-insert)
          ('visual  'my-ml-arrow-visual)
          ('replace 'my-ml-arrow-replace)
          ('motion  'my-ml-arrow-motion)
          (_        'my-ml-arrow-normal))
      'my-ml-arrow-normal))
  (defun my-ml-git-branch ()
    (when-let* ((file (buffer-file-name))
                ((eq (vc-backend file) 'Git))
                (branch (vc-git--symbolic-ref file)))
      (concat
       (propertize " " 'face 'doom-modeline-git-state)
       (propertize (concat branch
                           (if (eq (vc-state file) 'up-to-date) "" "꙳") " ")
                   'face 'doom-modeline-git-state))))
  (doom-modeline-def-segment my-evil-state
    (propertize
     (pcase (and (boundp 'evil-state) evil-state)
       ('normal  " NORMAL ")  ('insert  " INSERT ")
       ('visual  " VISUAL ")  ('replace " REPLACE ")
       ('motion  " MOTION ")  ('emacs   " EMACS ")
       (_        " EMACS "))
     'face (if (doom-modeline--active)
               (pcase (and (boundp 'evil-state) evil-state)
                 ('insert  'doom-modeline-evil-insert-state)
                 ('visual  'doom-modeline-evil-visual-state)
                 ('replace 'doom-modeline-evil-replace-state)
                 ('motion  'doom-modeline-evil-motion-state)
                 (_        'doom-modeline-evil-normal-state))
             'doom-modeline-evil-normal-state)))

  (defun my-ml-file-arrow-face ()
    (if (buffer-modified-p) 'my-ml-arrow-modified 'my-ml-arrow-filename))

  (doom-modeline-def-segment my-arrow-state-file
    (my-ml-sep-gap (my-ml-state-arrow-face) (my-ml-file-arrow-face) 'left))

  (doom-modeline-def-segment my-arrow-file-base
    (my-ml-sep 'left (my-ml-file-arrow-face) (my-ml-base-arrow-face)))

  (doom-modeline-def-segment my-filename
    (let* ((modified (buffer-modified-p))
           (role (if modified 'modified 'filename))
           (face (if modified
                     'doom-modeline-buffer-modified
                   'doom-modeline-buffer-file)))
      (concat
       (when (my-ml-icons-p)
         (let ((icon (doom-modeline-icon-for-buffer)))
           (when (stringp icon)
             (let ((iface (get-text-property 0 'face icon)))
               (concat
                (propertize " " 'face face)
                (propertize icon 'face
                            (append (if (listp iface) iface (list :inherit iface))
                                    (list :foreground (my-ml-fg role)
                                          :background (my-ml-bg role)))))))))
       (propertize (concat " " (buffer-name)
                           (if modified "꙳" "") " ")
                   'face face))))

  (doom-modeline-def-segment my-git-branch
    (my-ml-git-branch))

  (doom-modeline-def-segment my-arrow-git-start
    (my-ml-sep 'right (my-ml-base-arrow-face)
               (if (my-ml-git-branch) 'my-ml-arrow-git 'my-ml-arrow-time)))

  (doom-modeline-def-segment my-arrow-git-time
    (when (my-ml-git-branch)
      (my-ml-sep-gap 'my-ml-arrow-git 'my-ml-arrow-time 'right)))

  (doom-modeline-def-segment my-time
    (concat
     (when (my-ml-icons-p)
       (let* ((icon (nerd-icons-mdicon "nf-md-clock"))
              (face (get-text-property 0 'face icon)))
         (when (and (stringp icon) (not (string-empty-p icon)))
           (concat
            (propertize " " 'face 'doom-modeline-time-state)
            (propertize icon
                        'face (append (if (listp face) face (list :inherit face))
                                      (list :foreground (my-ml-fg 'time)
                                            :background (my-ml-bg 'time)))
                        'display '(raise 0.1))))))
     (propertize (format-time-string " %H:%M ")
                 'face 'doom-modeline-time-state)))
  ;; ---- 布局 ----
  (doom-modeline-def-modeline 'main
    '(window-number my-evil-state my-arrow-state-file my-filename my-arrow-file-base
                    buffer-position parrot selection-info matches)
    '(compilation misc-info debug lsp spell minor-modes input-method indent-info
                  buffer-encoding major-mode process check
                  my-arrow-git-start my-git-branch my-arrow-git-time my-time)))

;; A minor-mode menu for mode-line
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package nerd-icons
  :config
  (when (and (display-graphic-p) (not (font-installed-p nerd-icons-font-family)))
    (nerd-icons-install-fonts t)))

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
  (cl-loop for font in '("Maple Mono NF CN" "Iosevka Mono" "LXGW WenKai Mono" "Sarasa Mono SC" "SF Mono" "JetBrains Mono" "Source Code Pro" "Fira Code"
                         "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil :font (font-spec :family font :slant 'italic :size 18)))

  (cl-loop for font in '("Apple Symbols" "Segoe UI Symbol" "Symbola" "Symbol" "icons-in-terminal")
           when (font-installed-p font)
           return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))

  (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
           when (font-installed-p font)
           return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("LXGW WenKai Mono" "Maple Mono NF CN" "STKaiti" "Xingkai SC" "LingWai SC" "HanziPen SC" "WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (dolist (charset '(han symbol bopomofo kana)) (set-fontset-font t charset (font-spec :family font :slant 'normal)))))

(setq initial-frame-alist '((fullscreen . maximized)))

;; Display dividers between windows
(setq window-divider-default-places 'right-only
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; fix chinese & english can't sync scale
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode))

;; Scrolling
;; Scroll one line at a time (less "jumpy" than defaults)
(setq hscroll-step 1
      hscroll-margin 2
      scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount-horizontal 1
      mouse-wheel-progressive-speed nil)

;; Smooth scrolling
(when (fboundp 'pixel-scroll-precision-mode) ;; 29+
  (use-package ultra-scroll
    :hook (after-init . ultra-scroll-mode)))

;; Child frame
(when (childframe-workable-p)
  (use-package posframe
    :hook (after-load-theme . posframe-delete-all)
    :init
    (defface posframe-border
      `((t (:inherit region)))
      "Face used by the `posframe' border."
      :group 'posframe)

    :config
    (with-no-warnings
      (defun my-posframe--prettify-frame (&rest _)
        (set-face-background 'fringe nil posframe--frame))
      (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

      (defun posframe-poshandler-frame-center-near-bottom (info)
        (cons (/ (- (plist-get info :parent-frame-width)
                    (plist-get info :posframe-width))
                 2)
              (/ (+ (plist-get info :parent-frame-height)
                    (* 2 (plist-get info :font-height)))
                 2))))))

(use-package shrface
  :config
  (shrface-basic)
  (shrface-trial)
  (setq shrface-href-versatile t))

(provide 'init-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
