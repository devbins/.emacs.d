;;; init-evil.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-evil.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Sun Feb  9 09:36:55 2020 (+0800)
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

(use-package evil
  :defer t
  :preface
  (setq evil-want-visual-char-semi-exclusive t
        evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t
        evil-mode-line-format 'nil
        evil-respect-visual-line-mode t
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; cursor appearance
        ;; evil-default-cursor '((face-background 'mode-line) . (face-background 'mode-line))
        ;; evil-normal-state-cursor '(box '((face-foreground 'mode-line) . (face-foreground 'mode-line)))
        ;; evil-emacs-state-cursor  '(box '(face-foreground 'mode-line))
        ;; evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow)
  :hook ((after-init . evil-mode)
         (org-capture-mode . evil-insert-state))
  :config
  (setq evil-magic 'very-magic)
  (setq evil-want-fine-undo t)
  (setq evil-want-change-word-to-end t)
  (evil-set-initial-state 'flycheck-error-list-mode 'normal)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'git-rebase-mode 'normal)
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-set-initial-state 'vc-annotate-mode 'normal)
  (evil-set-initial-state 'Custom-mode 'normal)
  (evil-set-initial-state 'erc-mode 'normal)
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (evil-set-initial-state 'vc-dir-mode 'normal)
  (evil-set-initial-state 'vc-git-log-view-mode 'normal)
  (evil-set-initial-state 'vc-svn-log-view-mode 'normal)
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (evil-set-initial-state 'minibuffer-inactive-mode 'emacs)
  (evil-set-initial-state 'ivy-occur-mode 'normal)
  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
  (evil-set-initial-state 'pdf-view-mode 'normal)
  (evil-set-initial-state 'pdf-outline-buffer-mode 'normal)
  (evil-set-initial-state 'pdf-annot-list-mode 'normal)
  (evil-set-initial-state 'pdf-occur-buffer-mode 'normal)
  (evil-set-initial-state 'grep-mode 'normal)
  (evil-set-initial-state 'Info-mode 'motion)
  (evil-set-initial-state 'calc-mode 'normal)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'calendar-mode 'emacs)
  (evil-set-initial-state 'esup-mode 'emacs)
  (evil-set-initial-state 'diff-mode 'normal)
  (evil-set-initial-state 'deft-mode 'emacs)
  (evil-set-initial-state 'image-mode 'motion)
  (evil-set-initial-state 'image-dired-minor-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'snails-mode 'emacs)
  (evil-set-initial-state 'multi-term-mode 'emacs)

  (evil-define-key 'normal help-mode-map
    "q" 'quit-window)

  (evil-define-key 'normal helpful-mode-map
    "q" 'quit-window))

(use-package evil-leader
  :defer t
  :init(global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "SPC" 'counsel-M-x

    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9
    "0" 'treemacs-select-window


    "//" 'org-sparse-tree

    "ayl" 'ein:notebooklist-login
    "ayo" 'ein:notebooklist-open
    "ayr" 'ein:run
    "ays" 'ein:stop
    "aoo" 'org-agenda
    "aoa" 'org-agenda-list
    "aoc" 'org-capture
    "aot" 'org-todo-list

    "bb" 'ivy-switch-buffer
    "bd" 'kill-this-buffer
    "bm" 'bookmark-set
    "bj" 'bookmark-jump
    "bI" 'ibuffer
    "bp" 'previous-buffer
    "bn" 'next-buffer
    "bt" 'imenu-list-smart-toggle
    "bs" 'switch-to-scratch-buffer

    "ff" 'counsel-find-file
    "fj" 'dired-jump
    "fCd" 'unix2dos
    "fCu" 'dos2unix
    "fE" 'sudo-edit
    "fd" 'delete-this-file
    "ft" 'treemacs
    "fo" 'open-file-or-directory-in-external-app
    "fr" 'counsel-recentf
    "fR" 'rename-this-file
    "fs" 'save-buffer
    "fa" 'save-some-buffers
    "fyy" 'copy-file-name

    "Fb" 'switch-to-buffer-other-frame
    "FB" 'display-buffer-other-frame
    "Fd" 'delete-frame
    "FD" 'delete-other-frames
    "Fn" 'make-frame
    "Fo" 'other-frame
    "FO" 'dired-other-frame
    "Ff" 'find-file-other-frame

    "gs" 'magit-status
    "gt" 'git-timemachine
    "gb" 'magit-blame
    "gp" 'git-messenger:popup-message

    "hf" 'describe-function
    "hk" 'describe-key
    "hv" 'describe-variable
    "hp" 'describe-package

    "Ti" 'org-toggle-inline-images
    "Tl" 'org-toggle-link-display

    "nf" 'narrow-to-defun
    "np" 'narrow-to-page
    "nr" 'narrow-to-region
    "nw" 'widen

    "Pc" 'password-store-copy
    "Pg" 'password-store-generate
    "Pr" 'password-store-remove
    "Pe" 'password-store-edit
    "PR" 'password-store-rename
    "Pi" 'password-store-insert
    "Pu" 'password-store-url

    "sa" 'mark-whole-buffer
    "sp" 'projectile-ripgrep
    "ss" 'swiper
    "sn" 'snails

    "tl" 'toggle-truncate-lines
    "tw" 'toggle-frame-maximized
    "tf" 'toggle-frame-fullscreen

    "ww" 'other-window
    "wm" 'toggle-maximize-buffer
    "wo" 'other-frame
    "w/" 'split-window-right
    "w-" 'split-window-below
    "wd" 'delete-window
    "w=" 'balance-windows
    "wh" 'evil-window-left
    "wH" 'evil-window-move-far-left
    "wl" 'evil-window-right
    "wL" 'evil-window-move-far-right
    "wj" 'evil-window-down
    "wJ" 'evil-window-move-very-bottom
    "wk" 'evil-window-up
    "wK" 'evil-window-move-very-top

    "v" 'er/expand-region

    "xu" 'downcase-region
    "xU" 'upcase-region
    "u" 'downcase-word
    "U" 'upcase-word

    "yp" 'youdao-dictionary-search-at-point-posframe
    "yi" 'youdao-dictionary-search-from-input

    "qq" 'save-buffers-kill-terminal
    "qr" 'devbins/restart-emacs

    "'" 'shell-pop
    "TAB" 'switch-to-prev-buffer))


(use-package evil-lisp-state
  :defer t
  :init
  (setq evil-lisp-state-global t)
  :hook (prog-mode . (lambda ()(require 'evil-lisp-state)))
  :config
  (evil-lisp-state-leader "SPC k"))


(use-package evil-nerd-commenter
  :init (evil-leader/set-key
          "ci" 'evilnc-comment-or-uncomment-lines
          "cl" 'evilnc-comment-or-uncomment-lines
          "cc" 'evilnc-copy-and-comment-lines
          "cp" 'evilnc-comment-or-uncomment-paragraphs
          "cr" 'comment-or-uncomment-region
          "cv" 'evilnc-toggle-invert-comment-line-by-line
          "."  'evilnc-copy-and-comment-operator
          "/" 'evilnc-comment-operator))

(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-surround
  :defer t
  :ensure t
  :commands
  (evil-surround-edit
   evil-Surround-edit
   evil-surround-region
   evil-Surround-region)
  :init
  (evil-define-key 'operator global-map "s" 'evil-surround-edit)
  (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
  (evil-define-key 'visual global-map "S" 'evil-surround-region)
  (evil-define-key 'visual global-map "gS" 'evil-Surround-region)
  :config
  (global-evil-surround-mode))

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; remove all keybindings from insert-state keymap,it is VERY VERY important
(setcdr evil-insert-state-map nil)

  ;;;把emacs模式下的按键绑定到Insert模式下
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)


;; but [escape] should switch back to normal state
(define-key evil-insert-state-map [escape] 'evil-normal-state)

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)


(provide 'init-evil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-evil.el ends here
