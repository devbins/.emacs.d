;;; init-edit.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-edit.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:22:19 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 120
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

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Pass a URL to a WWW browser
(use-package browse-url
  :ensure nil
  :bind (("C-c C-z ." . browse-url-at-point)
         ("C-c C-z b" . browse-url-of-buffer)
         ("C-c C-z r" . browse-url-of-region)
         ("C-c C-z u" . browse-url)
         ("C-c C-z e" . browse-url-emacs)
         ("C-c C-z v" . browse-url-of-file))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map)))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

;; Kill text between the point and the character CHAR
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package link-hint)

;; Jump to Chinese characters
(use-package ace-pinyin
  :diminish
  :hook (after-init . ace-pinyin-global-mode))

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :defines desktop-minor-mode-table
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

;; Increase selected region by semantic units
(use-package expand-region
  :bind (:map evil-visual-state-map
         ("v" . er/expand-region)
         ("V" . er/contract-region)))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)))

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init (setq flyspell-issue-message-flag nil
              ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

;; Imenu list in slide
(use-package imenu-list
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t)
  (evil-define-key 'normal imenu-list-major-mode-map
    (kbd "RET") 'imenu-list-goto-entry
    (kbd "TAB") 'hs-toggle-hiding
    "," 'imenu-list-display-entry
    "j" 'next-line
    "k" 'previous-line
    "gr" 'imenu-list-refresh
    "q" 'imenu-list-quit-window))

;; Treat undo history as a tree
(if emacs/>=28p
    (use-package vundo
      :bind ("C-x u" . vundo)
      :config (setq vundo-glyph-alist vundo-unicode-symbols))
  (use-package undo-tree
    :diminish
    :hook (after-init . global-undo-tree-mode)
    :init (setq undo-tree-visualizer-timestamps t
                undo-tree-visualizer-diff t
                undo-tree-enable-undo-in-region nil
                undo-tree-auto-save-history nil)))

;; Open files as another user
(unless sys/win32p
  (use-package sudo-edit))

;; https://github.com/DogLooksGood/emacs-rime
(use-package rime
  :init
  (setq rime-disable-predicates '(rime-predicate-prog-in-code-p
                                  rime-predicate-after-alphabet-char-p)
        rime-inline-predicates '(rime-predicate-space-after-cc-p
                                 rime-predicate-current-uppercase-letter-p)
        rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>")
        rime-posframe-properties (list :font "STKaiti"
                                       :internal-border-width 10)
        default-input-method "rime"
        rime-cursor "|"
        rime-show-candidate 'posframe
        rime-librime-root (cond (sys/macp (expand-file-name "librime/dist" user-emacs-directory))
                                (sys/linuxp nil)))
  :config
  (set-face-attribute 'rime-default-face nil :foreground "#FFFFFF" :background "#3d424d")
  :bind
  (:map rime-active-mode-map
   ("<tab>" . 'rime-inline-ascii)
   :map rime-mode-map
   ("C-`" . 'rime-send-keybinding)
   ("M-j" . 'rime-force-enable)
   ("C-SPC" . 'toggle-input-method)))

;; The selected region of text can be deleted
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package docstr
  :init
  (setq docstr-key-support t
        docstr-desc-summary ""
        docstr-desc-param ""
        docstr-desc-return ""
        docstr-desc-typename "")
  (global-docstr-mode 1)
  :config
  (docstr-faces-apply))

(use-package kill-ring-search)


(use-package emt
  :if sys/macp
  :load-path "site-lisp/emt"
  :hook (after-init . emt-mode))

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
