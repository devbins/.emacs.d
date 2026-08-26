;;; init-evil.el --- -*- lexical-binding: t no-byte-compile: t; -*-
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
;;     Update #: 294
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
        evil-want-integration t
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
  (evil-set-initial-state 'telega-root-mode 'emacs)
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
  (evil-set-initial-state 'achive-visual-mode 'emacs)
  (evil-set-initial-state 'fanyi-mode 'emacs)
  (evil-set-initial-state 'mcp-hub-mode 'emacs)
  (evil-set-initial-state 'agent-shell-viewport-view-mode 'emacs)
  (evil-set-initial-state 'agent-shell-viewport-edit-mode 'emacs)
  (evil-set-initial-state 'clutch-result-mode 'emacs)

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
  (global-set-key [escape]                             'evil-exit-emacs-state)

  (evil-define-key 'normal help-mode-map
    "q" 'quit-window)

  (evil-define-key 'normal helpful-mode-map
    "q" 'quit-window))

(use-package evil-collection
  :custom (evil-collection-setup-minibuffer t)
  :commands (evil-collection-init)
  :after evil
  :init
  (setq evil-collection-calendar-want-org-bindings t)
  ;; The list of supported modes is configured by evil-collection-mode=list
  ;; (evil-collection-init '(dired view magit magit-todos magit-section magit-repos forge ibuffer custom calendar pdf nov docker vterm eshell xwidget markdown mu4e mu4e-conversation ))
  (evil-collection-init))

(use-package evil-leader)


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
