;;; init-window.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-window.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:29:27 2020 (+0800)
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

;; window number
(use-package winum
  :hook (after-init . winum-mode))

;; Directional window-selection routines
(use-package windmove
  :ensure nil
  :hook (after-init . windmove-default-keybindings))

;; Restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

;; Enforce rules for popups
(when emacs/>=26p
  (use-package popper
    :defines popper-echo-dispatch-actions
    :commands popper-group-by-projectile
    :bind (:map popper-mode-map
           ("C-h z" . popper-toggle-latest)
           ("C-<tab>"   . popper-cycle)
           ("C-M-<tab>" . popper-toggle-type))
    :hook (after-init . popper-mode)
    :init
    (setq popper-reference-buffers
          '("\\*Messages\\*"
            "Output\\*$" "\\*Pp Eval Output\\*$"
            "\\*Compile-Log\\*"
            "\\*Completions\\*"
            "\\*Warnings\\*"
            "\\*Async Shell Command\\*"
            "\\*Apropos\\*"
            "\\*Backtrace\\*"
            "\\*Calendar\\*"
            "\\*Finder\\*"
            "\\*Embark Actions\\*"

            bookmark-bmenu-mode
            comint-mode
            compilation-mode
            help-mode helpful-mode
            tabulated-list-mode
            Buffer-menu-mode

            gnus-article-mode devdocs-mode
            grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
            ivy-occur-mode ivy-occur-grep-mode
            process-menu-mode list-environment-mode cargo-process-mode
            youdao-dictionary-mode osx-dictionary-mode fanyi-mode

            "^\\*eshell.*\\*.*$" eshell-mode
            "^\\*shell.*\\*.*$"  shell-mode
            "^\\*terminal.*\\*.*$" term-mode
            "^\\*vterm.*\\*.*$"  vterm-mode

            "\\*DAP Templates\\*$" dap-server-log-mode
            "\\*ELP Profiling Restuls\\*" profiler-report-mode
            "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
            "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
            "\\*[Wo]*Man.*\\*$"
            "\\*ert\\*$" overseer-buffer-mode
            "\\*gud-debug\\*$"
            "\\*lsp-help\\*$" "\\*lsp session\\*$"
            "\\*quickrun\\*$"
            "\\*tldr\\*$"
            "\\*vc-.*\\*$"
            "^\\*elfeed-entry\\*$"
            "^\\*macro expansion\\**"

            "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
            "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
            "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*"
            "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
            "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
            rustic-cargo-outdated-mode rustic-cargo-test-moed))

    (with-eval-after-load 'projectile
      (setq popper-group-function #'popper-group-by-projectile))

    (when (display-grayscale-p)
      (setq popper-mode-line
            '(:eval
              (format " %s " (all-the-icons-octicon "pin" :height 0.9 :v-adjust 0.0 :face 'mode-line-emphasis)))))

    (setq popper-echo-dispatch-actions t)
    :config
    (popper-echo-mode 1)

    (with-no-warnings
      (defun my-popper-fit-window-height (win)
        "Determine the height of popup window WIN by fitting it to the buffer's content."
        (fit-window-to-buffer
         win
         (floor (frame-height) 3)
         (floor (frame-height) 3)))
      (setq popper-window-height #'my-popper-fit-window-height)

      (defun popper-close-window-hack (&rest _)
        "Close popper window via `C-g'."
        ;; `C-g' can deactivate region
        (when (and (called-interactively-p 'interactive)
                   (not (region-active-p))
                   popper-open-popup-alist)
          (let ((window (caar popper-open-popup-alist)))
            (when (window-live-p window)
              (delete-window window)))))
      (advice-add #'keyboard-quit :before #'popper-close-window-hack))))

(provide 'init-window)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-window.el ends here
