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
;;     Update #: 229
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
  :hook (after-init . evil-mode)
  :init
  (setq evil-magic 'very-magic
        evil-want-visual-char-semi-exclusive t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t
        evil-mode-line-format nil
        evil-respect-visual-line-mode t
        evil-symbol-word-search t
        evil-visual-state-cursor 'hollow
        evil-auto-indent t
        evil-ex-complete-emacs-commands t
        evil-want-keybinding nil
        evil-want-fine-undo t
        evil-want-change-word-to-end t)
  :config
  (evil-set-undo-system 'undo-redo)
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
  (evil-set-initial-state 'pdf-view-mode 'normal)
  (evil-set-initial-state 'pdf-outline-buffer-mode 'emacs)
  (evil-set-initial-state 'pdf-outline-minor-mode 'emacs)
  (evil-set-initial-state 'pdf-annot-list-mode 'normal)
  (evil-set-initial-state 'pdf-occur-buffer-mode 'normal)
  (evil-set-initial-state 'grep-mode 'normal)
  (evil-set-initial-state 'Info-mode 'motion)
  (evil-set-initial-state 'calc-mode 'normal)
  (evil-set-initial-state 'easy-hugo-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'calendar-mode 'emacs)
  (evil-set-initial-state 'esup-mode 'emacs)
  (evil-set-initial-state 'diff-mode 'normal)
  (evil-set-initial-state 'deft-mode 'emacs)
  (evil-set-initial-state 'bongo-mode 'emacs)
  (evil-set-initial-state 'netease-cloud-music-mode 'emacs)
  (evil-set-initial-state 'leetcode--problems-mode 'emacs)
  (evil-set-initial-state 'eaf-mode 'emacs)
  (evil-set-initial-state 'image-mode 'motion)
  (evil-set-initial-state 'image-dired-minor-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'multi-term-mode 'emacs)
  (evil-set-initial-state 'aweshell-mode 'emacs)
  (evil-set-initial-state 'org-capture-mode 'insert)
  (evil-set-initial-state 'calculator-mode 'emacs)
  (evil-set-initial-state 'ros-process-mode 'emacs)
  (evil-set-initial-state 'telega-chat-mode 'emacs)
  (evil-set-initial-state 'ccls-tree-mode 'emacs)
  (evil-set-initial-state 'nov-mode 'emacs)
  (evil-set-initial-state 'elfeed 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-dashboard-mode 'emacs)
  (evil-set-initial-state 'calibredb-search-mode 'emacs)
  (evil-set-initial-state 'calibredb-show-mode 'emacs)
  (evil-set-initial-state 'pass-mode 'emacs)
  (evil-set-initial-state 'flutter-mode 'emacs)
  (evil-set-initial-state 'srefactor-ui-menu-mode 'emacs)
  (evil-set-initial-state 'color-rg-mode 'emacs)
  (evil-set-initial-state 'color-rg-search-mode 'emacs)
  (evil-set-initial-state 'lsp-bridge-ref-mode 'emacs)

  ;; remove all keybindings from insert-state keymap,it is VERY VERY important
  (setcdr evil-insert-state-map nil)

  ;;;把emacs模式下的按键绑定到Insert模式下
  (define-key evil-insert-state-map
    (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

  ;; but [escape] should switch back to normal state
  (define-key evil-insert-state-map [escape] 'evil-normal-state)

  (define-key evil-normal-state-map [escape]           'keyboard-quit)
  (define-key evil-visual-state-map [escape]           'keyboard-quit)
  (define-key evil-emacs-state-map  [escape]           'evil-normal-state)
  (define-key evil-motion-state-map [escape]           'evil-normal-state)
  (define-key evil-operator-state-map [escape]         'evil-normal-state)
  (define-key minibuffer-local-map [escape]            'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape]         'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape]    'minibuffer-keyboard-quit)
  (define-key evil-normal-state-map "U" #'undo-tree-visualize)
  (global-set-key [escape]                             'evil-exit-emacs-state)

  (evil-define-key 'normal help-mode-map
    "q" 'quit-window)

  (evil-define-key 'normal helpful-mode-map
    "q" 'quit-window)

  (use-package evil-collection
    :custom (evil-collection-setup-minibuffer t)
    :init
    ;; The list of supported modes is configured by evil-collection-mode=list
    (evil-collection-init '(dired view magit magit-todos magit-section ibuffer custom calendar pdf docker vterm eshell xwidget))))

(use-package evil-leader
  :init
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "SPC" 'execute-extended-command
    "1"   'winum-select-window-1
    "2"   'winum-select-window-2
    "3"   'winum-select-window-3
    "4"   'winum-select-window-4
    "5"   'winum-select-window-5
    "6"   'winum-select-window-6
    "7"   'winum-select-window-7
    "8"   'winum-select-window-8
    "9"   'winum-select-window-9
    "0"   'treemacs-select-window

    "atl" 'global-command-log-mode
    "alt" 'clm/toggle-command-log-buffer
    "ayl" 'ein:notebooklist-login
    "ayo" 'ein:notebooklist-open
    "ayr" 'ein:run
    "ays" 'ein:stop
    "aoo" 'org-agenda
    "aoa" 'org-agenda-list
    "aoc" 'org-capture
    "aot" 'org-todo-list
    "am"  'wl
    "am"  'netease-cloud-music
    "aojj" 'org-journal-new-entry
    "aojs" 'org-journal-search-forever
    "aojt" 'org-journal-new-scheduled-entry
    "aojv" 'org-journal-schedule-view

    "bb"  'consult-buffer
    "bd"  'kill-this-buffer
    "bh"  'open-dashboard
    "bi"  'consult-imenu
    "bm"  'bookmark-set
    "bj"  'bookmark-jump
    "bl"  'xwidget-webkit-browse-url
    "bI"  'ibuffer
    "bp"  'previous-buffer
    "bn"  'next-buffer
    "bt"  'imenu-list-smart-toggle
    "bs"  'switch-to-scratch-buffer

    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    "."  'evilnc-copy-and-comment-operator
    "\\" 'evilnc-comment-operator ; if you prefer backslash key
    "cp" 'compile

    "ff"  'find-file
    "fj"  'dired-jump
    "fCd" 'unix2dos
    "fCu" 'dos2unix
    "fCp" 'copy-current-file
    "fE"  'sudo-edit
    "fd"  'delete-this-file
    "fl"  'format-all-buffer
    "ft"  'treemacs
    "fo"  'open-file-or-directory-in-external-app
    "fr"  'consult-recent-file
    "fR"  'rename-this-file
    "fs"  'save-buffer
    "fa"  'save-some-buffers
    "fyy" 'copy-file-name

    "Fb"  'switch-to-buffer-other-frame
    "FB"  'display-buffer-other-frame
    "Fd"  'delete-frame
    "FD"  'delete-other-frames
    "Fn"  'make-frame
    "Fo"  'other-frame
    "FO"  'dired-other-frame
    "Ff"  'find-file-other-frame

    "gc"  'grab-mac-link-dwim
    "gs"  'magit-status
    "gt"  'git-timemachine
    "gb"  'magit-blame
    "gp"  'git-messenger:popup-message

    "hf"  'find-function-at-point
    "hk"  'find-function-on-key
    "hv"  'find-variable
    "hp"  'describe-package

    "il"  'org-insert-link-global ;; 不在org mode中使用https://orgmode.org/manual/Using-Links-Outside-Org.html
    "ii"  'org-id-get-create
    "lo"  'link-hint-open-link
    "lc"  'link-hint-copy-link
    "ol"  'org-open-at-point-global

    "nf"  'narrow-to-defun
    "np"  'narrow-to-page
    "nr"  'narrow-to-region
    "nw"  'widen

    ;; Project
    "pb"  'project-switch-to-buffer
    "pc"  'project-compile
    "pd"  'project-find-dir
    "pf"  'project-find-file
    "pk"  'project-kill-buffers
    "pp"  'project-switch-project
    "pv"  'project-vc-dir
    "px"  'project--remove-from-project-list

    "po"  'poetry                       ;; Python poetry

    "Pc"  'password-store-copy
    "Pg"  'password-store-generate
    "Pr"  'password-store-remove
    "Pe"  'password-store-edit
    "PR"  'password-store-rename
    "Pi"  'password-store-insert
    "Pu"  'password-store-url

    "rn"  'cargo-process-new
    "rs"  'restart-emacs
    "rf"  'org-roam-node-find
    "ri"  'org-roam-node-insert
    "ra"  'org-roam-db-autosync-mode
    "rl"  'org-roam-alias-add

    "sa"  'mark-whole-buffer
    "sp"  'consult-ripgrep
    "ss"  'consult-line

    "te"  'toggle-corfu-english-helper
    "tg"  'go-translate
    "tp"  'go-translate-popup
    "tt"  'insert-translated-name-insert
    "tl"  'toggle-truncate-lines
    "tw"  'toggle-frame-maximized
    "tf"  'toggle-frame-fullscreen
    "th"  'english-teacher-follow-mode
    "tt"  'toggle-transparency
    "ts"  'consult-theme

    "ww"  'other-window
    "wm"  'toggle-maximize-buffer
    "wo"  'other-frame
    "w/"  'split-window-right
    "w-"  'split-window-below
    "wd"  'delete-window
    "w="  'balance-windows
    "wh"  'evil-window-left
    "wH"  'evil-window-move-far-left
    "wl"  'evil-window-right
    "wL"  'evil-window-move-far-right
    "wj"  'evil-window-down
    "wJ"  'evil-window-move-very-bottom
    "wk"  'evil-window-up
    "wK"  'evil-window-move-very-top

    "v"   'er/expand-region

    "xu"  'downcase-region
    "xU"  'upcase-region
    "u"   'downcase-word
    "U"   'upcase-word

    "yp"  'youdao-dictionary-search-at-point-posframe
    "yi"  'youdao-dictionary-search-from-input
    "yv"  'youdao-dictionary-play-voice-at-point
    "ys"  'youdao-dictionary-play-voice-from-input

    "qq"  'save-buffers-kill-terminal

    "'"   'shell-pop-posframe-toggle
    "TAB" 'switch-to-prev-buffer))


(use-package evil-lisp-state
  :init
  (setq evil-lisp-state-global t)
  :hook (prog-mode . (lambda ()(require 'evil-lisp-state)))
  :config
  (evil-lisp-state-leader "SPC k"))

;; 对齐
;; https://github.com/edkolev/evil-lion
(use-package evil-lion
  :hook (after-init . evil-lion-mode))

(use-package evil-surround
  :commands
  (evil-surround-edit
   evil-Surround-edit
   evil-surround-region
   evil-Surround-region)
  :config
  (evil-define-key 'operator global-map "s" 'evil-surround-edit)
  (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
  (evil-define-key 'visual global-map "S"   'evil-surround-region)
  (evil-define-key 'visual global-map "gS"  'evil-Surround-region)
  :hook (after-init . global-evil-surround-mode))

;; evil NERD commenter, commenting awesomeness!
(use-package evil-nerd-commenter
  :init (evilnc-default-hotkeys))

(use-package evil-matchit
  :after evil
  :commands evilmi--region-to-select-or-delete
  :hook ((prog-mode org-mode) . turn-on-evil-matchit-mode))

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


(provide 'init-evil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-evil.el ends here
