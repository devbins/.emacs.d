;;; init-utils.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-utils.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:33:35 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 3
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
;; Display available keybindings in popup
(use-package which-key
  :diminish
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :hook (after-init . which-key-mode))

;; Youdao Dictionary
(use-package youdao-dictionary
  :commands youdao-dictionary-play-voice-of-current-word
  :bind (("C-c y" . my-youdao-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point)
         :map youdao-dictionary-mode-map
         ("h" . youdao-dictionary-hydra/body)
         ("?" . youdao-dictionary-hydra/body)
         :map evil-normal-state-map
         ("q" . quit-window))
  :init
  (setq url-automatic-caching t
        youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词

  (with-no-warnings
    (defun my-youdao-search-at-point ()
      "Search word at point and display result with `posframe', `pos-tip', or buffer."
      (interactive)
      (if (display-graphic-p)
          (if emacs/>=26p
              (youdao-dictionary-search-at-point-posframe)
            (youdao-dictionary-search-at-point-tooltip))
        (youdao-dictionary-search-at-point))))
  :config
  (with-eval-after-load 'hydra
    (defhydra youdao-dictionary-hydra (:color blue)
      ("p" youdao-dictionary-play-voice-of-current-word "play voice of current word")
      ("y" youdao-dictionary-play-voice-at-point "play voice at point")
      ("q" quit-window "quit")
      ("C-g" nil nil)
      ("h" nil nil)
      ("?" nil nil))))

;;
;; Search tools
;;

;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; Fast search tool: `ripgrep'
(use-package rg
  :defines projectile-command-map
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
         ("c" . rg-dwim-current-dir)
         ("f" . rg-dwim-current-file)
         ("m" . rg-menu)
         :map rg-mode-map
         ("m" . rg-menu))
  :init (setq rg-group-result t
              rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

  (with-eval-after-load 'projectile
    (defalias 'projectile-ripgrep #'rg-project)
    (bind-key "s R" #'rg-project projectile-command-map))

  (with-eval-after-load 'counsel
    (bind-keys
     :map rg-global-map
     ("R" . counsel-rg)
     ("F" . counsel-fzf))))

;; Docker
(use-package docker
  :defines docker-image-run-arguments
  :bind ("C-c d" . docker)
  :init (setq docker-image-run-arguments '("-i" "-t" "--rm")
              docker-container-shell-file-name "/bin/bash"))

;; Docker tramp
(use-package docker-tramp)

;; Persistent the scratch buffer
(use-package persistent-scratch
  :diminish
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode)))

;; PDF reader
(use-package pdf-view
  :if (display-graphic-p)
  :ensure pdf-tools
  :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
  :defines pdf-annot-activate-created-annotations
  :functions (my-pdf-view-set-midnight-colors my-pdf-view-set-dark-theme)
  :commands pdf-view-midnight-minor-mode
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
         ("C-s" . isearch-forward))
  :init (setq pdf-annot-activate-created-annotations t)
  :config
  ;; WORKAROUND: Fix compilation errors on macOS.
  ;; @see https://github.com/politza/pdf-tools/issues/480
  (when sys/macp
    (setenv "PKG_CONFIG_PATH"
            "/usr/local/lib/pkgconfig:/usr/local/opt/libffi/lib/pkgconfig"))
  (pdf-tools-install t nil t t)

  ;; Set dark theme
  (defun my-pdf-view-set-midnight-colors ()
    "Set pdf-view midnight colors."
    (setq pdf-view-midnight-colors
          `(,(face-foreground 'default) . ,(face-background 'default))))

  (defun my-pdf-view-set-dark-theme ()
    "Set pdf-view midnight theme as color theme."
    (my-pdf-view-set-midnight-colors)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'pdf-view-mode)
          (pdf-view-midnight-minor-mode (if pdf-view-midnight-minor-mode 1 -1))))))

  (my-pdf-view-set-midnight-colors)
  (add-hook 'after-load-theme-hook #'my-pdf-view-set-dark-theme)

  ;; FIXME: Support retina
  ;; @see https://emacs-china.org/t/pdf-tools-mac-retina-display/10243/
  ;; and https://github.com/politza/pdf-tools/pull/501/
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (with-no-warnings
    (defun pdf-view-use-scaling-p ()
      "Return t if scaling should be used."
      (and (or (and (eq system-type 'darwin) (>= emacs-major-version 27))
               (memq (pdf-view-image-type) '(imagemagick image-io)))
           pdf-view-use-scaling))
    (defun pdf-view-create-page (page &optional window)
      "Create an image of PAGE for display on WINDOW."
      (let* ((size (pdf-view-desired-image-size page window))
             (width (if (not (pdf-view-use-scaling-p))
                        (car size)
                      (* 2 (car size))))
             (data (pdf-cache-renderpage
                    page width width))
             (hotspots (pdf-view-apply-hotspot-functions
                        window page size)))
        (pdf-view-create-image data
                               :width width
                               :scale (if (pdf-view-use-scaling-p) 0.5 1)
                               :map hotspots
                               :pointer 'arrow))))


  (evil-leader/set-key-for-mode 'pdf-view-mode
    ;; Slicing image
    "msm" 'pdf-view-set-slice-using-mouse
    "msb" 'pdf-view-set-slice-from-bounding-box
    "msr" 'pdf-view-reset-slice
    ;; Annotations
    "maD" 	'pdf-annot-delete
    "mat" 	'pdf-annot-attachment-dired
    "mah" 	'pdf-annot-add-highlight-markup-annotation
    "mal" 	'pdf-annot-list-annotations
    "mam" 	'pdf-annot-add-markup-annotation
    "mao" 	'pdf-annot-add-strikeout-markup-annotation
    "mas" 	'pdf-annot-add-squiggly-markup-annotation
    "mat" 	'pdf-annot-add-text-annotation
    "mau" 	'pdf-annot-add-underline-markup-annotation
    ;; Fit image to window
    "mfw" 'pdf-view-fit-width-to-window
    "mfh" 'pdf-view-fit-height-to-window
    "mfp" 'pdf-view-fit-page-to-window
    ;; Other
    "mss" 'pdf-occur
    "mp" 'pdf-misc-print-document
    "mO" 'pdf-outline
    "mn" 'pdf-view-midnight-minor-mode)

  (evil-define-key 'normal pdf-view-mode-map
    ;; Navigation
    "0"  'image-bol
    "$"  'image-eol
    "j"  'pdf-view-next-line-or-next-page
    "k"  'pdf-view-previous-line-or-previous-page
    "l"  'image-forward-hscroll
    "h"  'image-backward-hscroll
    "J"  'pdf-view-next-page
    "K"  'pdf-view-previous-page
    "gg"  'pdf-view-first-page
    "G"  'pdf-view-last-page
    "gt"  'pdf-view-goto-page
    "gl"  'pdf-view-goto-label
    "u" 'pdf-view-scroll-down-or-previous-page
    "d" 'pdf-view-scroll-up-or-next-page
    (kbd "C-u") 'pdf-view-scroll-down-or-previous-page
    (kbd "C-d") 'pdf-view-scroll-up-or-next-page
    (kbd "``")  'pdf-history-backward
    ;; Search
    "/" 'isearch-forward
    "?" 'isearch-backward
    ;; Actions
    "r"   'pdf-view-revert-buffer
    "o"   'pdf-links-action-perform
    "O"   'pdf-outline
    "zr"  'pdf-view-scale-reset)

  (evil-define-key 'visual pdf-view-mode-map "y" 'pdf-view-kill-ring-save)

  (evil-define-key 'normal pdf-outline-buffer-mode-map
    "-"                'negative-argument
    "j"                'next-line
    "k"                'previous-line
    "gk"               'outline-backward-same-level
    "gj"               'outline-forward-same-level
    (kbd "<backtab>")  'show-all
    "gh"               'pdf-outline-up-heading
    "gg"               'beginning-of-buffer
    "G"                'pdf-outline-end-of-buffer
    (kbd "TAB")              'outline-toggle-children
    (kbd "RET")              'pdf-outline-follow-link
    (kbd "M-RET")      'pdf-outline-follow-link-and-quit
    "f"                'pdf-outline-display-link
    [mouse-1]          'pdf-outline-mouse-display-link
    "o"                'pdf-outline-select-pdf-window
    "``"               'pdf-outline-move-to-current-page
    "''"               'pdf-outline-move-to-current-page
    "Q"                'pdf-outline-quit-and-kill
    "q"                'quit-window
    "F"                'pdf-outline-follow-mode)

  (evil-define-key 'normal pdf-annot-list-mode-map
    "f"                'pdf-annot-list-display-annotation-from-id
    "d"                'tablist-flag-forward
    "x"                'tablist-do-flagged-delete
    "u"                'tablist-unmark-forward
    "q"                'tablist-quit)


  (evil-define-key 'normal pdf-occur-buffer-mode-map
    "q"              'tablist-quit
    "g"              'pdf-occur-revert-buffer-with-args
    "r"              'pdf-occur-revert-buffer-with-args
    "?"              'evil-search-backward)

  ;; Recover last viewed position
  (when emacs/>=26p
    (use-package pdf-view-restore
      :hook (pdf-view-mode . pdf-view-restore-mode)
      :init (setq pdf-view-restore-filename
                  (locate-user-emacs-file ".pdf-view-restore")))))

;; Epub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :functions read-mode
  :hook (nov-mode . my-nov-setup)
  :init
  (defun my-nov-setup ()
    "Setup `nov-mode' for better reading experience."
    (visual-line-mode 1)
    (read-mode)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5))
  :config
  ;; FIXME: errors while opening `nov' files with Unicode characters
  ;; @see https://github.com/wasamasa/nov.el/issues/63
  (with-no-warnings
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (when-let* ((name (nov-content-unique-identifier-name content))
                  (selector (format "package>metadata>identifier[id='%s']"
                                    (regexp-quote name)))
                  (id (car (esxml-node-children (esxml-query selector content)))))
        (intern id)))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier))

  ;; Fix encoding issue on Windows
  (when sys/win32p
    (setq process-coding-system-alist
          (cons `(,nov-unzip-program . (gbk . gbk))
                process-coding-system-alist))))

;; Nice writing
(use-package olivetti
  :diminish
  :bind ("<f7>" . olivetti-mode)
  :hook (olivetti-mode . (lambda ()
                           (if olivetti-mode
                               (text-scale-set +2)
                             (text-scale-set 0))))
  :init (setq olivetti-body-width 0.618))

;; Edit text for browsers with GhostText or AtomicChrome extension
(use-package atomic-chrome
  :hook ((emacs-startup . atomic-chrome-start-server)
         (atomic-chrome-edit-mode . delete-other-windows))
  :init (setq atomic-chrome-buffer-open-style 'frame)
  :config
  (if (fboundp 'gfm-mode)
      (setq atomic-chrome-url-major-mode-alist
            '(("github\\.com" . gfm-mode)))))

;; Music player
(use-package bongo
  :functions (bongo-add-dired-files
              dired-get-filename
              dired-marker-regexp
              dired-move-to-filename)
  :commands (bongo-buffer
             bongo-library-buffer-p
             bongo-library-buffer)
  :bind ("C-<f9>" . bongo)
  :init
  (with-eval-after-load 'dired
    (defun bongo-add-dired-files ()
      "Add marked files to Bongo library"
      (interactive)
      (bongo-buffer)
      (let (file (files nil))
        (dired-map-over-marks
         (setq file (dired-get-filename)
               files (append files (list file)))
         nil t)
        (with-bongo-library-buffer
         (mapc 'bongo-insert-file files)))
      (bongo-switch-buffers))
    (bind-key "b" #'bongo-add-dired-files dired-mode-map)))

;; IRC
(use-package erc
  :ensure nil
  :defines erc-autojoin-channels-alist
  :init (setq erc-rename-buffers t
              erc-interpret-mirc-color t
              erc-lurker-hide-list '("JOIN" "PART" "QUIT")
              erc-autojoin-channels-alist '(("freenode.net" "#emacs"))))

;; A stackoverflow and its sisters' sites reader
(when emacs/>=26p
  (use-package howdoyou
    :bind (:map howdoyou-mode-map
           ("q" . kill-buffer-and-window))
    :hook (howdoyou-mode . read-only-mode)))

;; text mode directory tree
(use-package ztree
  :custom-face
  (ztreep-header-face ((t (:inherit diff-header))))
  (ztreep-arrow-face ((t (:inherit font-lock-comment-face))))
  (ztreep-leaf-face ((t (:inherit diff-index))))
  (ztreep-node-face ((t (:inherit font-lock-variable-name-face))))
  (ztreep-expand-sign-face ((t (:inherit font-lock-function-name-face))))
  (ztreep-diff-header-face ((t (:inherit (diff-header bold)))))
  (ztreep-diff-header-small-face ((t (:inherit diff-file-header))))
  (ztreep-diff-model-normal-face ((t (:inherit font-lock-doc-face))))
  (ztreep-diff-model-ignored-face ((t (:inherit font-lock-doc-face :strike-through t))))
  (ztreep-diff-model-diff-face ((t (:inherit diff-removed))))
  (ztreep-diff-model-add-face ((t (:inherit diff-nonexistent))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Ztree" 'octicon "diff" :height 1.2 :v-adjust 0)
    :color pink :quit-key "q")
   ("Diff"
    (("C" ztree-diff-copy "copy" :exit t)
     ("h" ztree-diff-toggle-show-equal-files "show/hide equals" :exit t)
     ("H" ztree-diff-toggle-show-filtered-files "show/hide ignores" :exit t)
     ("D" ztree-diff-delete-file "delete" :exit t)
     ("v" ztree-diff-view-file "view" :exit t)
     ("d" ztree-diff-simple-diff-files "simple diff" :exit t)
     ("r" ztree-diff-partial-rescan "partial rescan" :exit t)
     ("R" ztree-diff-full-rescan "full rescan" :exit t))
    "View"
    (("RET" ztree-perform-action "expand/collapse or view" :exit t)
     ("SPC" ztree-perform-soft-action "expand/collapse or view in other" :exit t)
     ("TAB" ztree-jump-side "jump side" :exit t)
     ("g" ztree-refresh-buffer "refresh" :exit t)
     ("x" ztree-toggle-expand-subtree "expand/collapse" :exit t)
     ("<backspace>" ztree-move-up-in-tree "go to parent" :exit t))))
  :bind (:map ztreediff-mode-map
         ("C-<f5>" . ztree-hydra/body))
  :init (setq ztree-draw-unicode-lines t
              ztree-show-number-of-children t))

(use-package snails
  :defer t
  :load-path "~/.emacs.d/site-lisp/snails/"
  :if (display-graphic-p)
  :custom-face
  (snails-content-buffer-face ((t (:background "#111" :height 110))))
  (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110))))
  (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
  :config
  ;; Functions for specific backends
  (defun snails-current-project ()
    (interactive)
    (snails '(snails-backend-projectile snails-backend-rg snails-backend-fd snail-backend-imenu)))
  (defun snails-active-recent-buffers ()
    (interactive)
    (snails '(snails-backend-buffer snails-backend-recentf)))
  :bind
  (("M-s s" . snails)
   ("M-s g" . snails-current-project)
   ("M-s b" . snails-active-recent-buffers)))

;; Misc
(use-package copyit)                    ; copy path, url, etc.
(use-package daemons)                   ; system services/daemons
(use-package diffview)                  ; side-by-side diff view
(use-package esup)                      ; Emacs startup profiler
(use-package focus)                     ; Focus on the current region
(use-package list-environment)
(use-package memory-usage)
(use-package tldr)

(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
