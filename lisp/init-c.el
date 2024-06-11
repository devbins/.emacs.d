;;; init-c.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-c.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:37:31 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 53
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

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
         ("C-c c" . compile)
         (:map c-mode-map
	      ("M-RET" . srefactor-refactor-at-point))
         (:map c++-mode-map
	       ("M-RET" . srefactor-refactor-at-point)))
  :hook (c-mode-common . (lambda () (c-set-style "bsd")
                           (setq c-basic-offset 4
                                 tab-width 4)))
  :config
  (evil-leader/set-key-for-mode 'c-mode
    "mfo" 'ff-find-other-file
    "mfO" 'ff-find-other-file-other-window)
  (evil-leader/set-key-for-mode 'c++-mode
    "mfo" 'ff-find-other-file
    "mfO" 'ff-find-other-file-other-window)
  :custom
  (c-comment-prefix-regexp '((c-mode   . "//+!?\\|\\**")
                             (c++-mode . "//+!?\\|\\**")
                             (awk-mode . "#+")
                             (other    . "//+\\|\\**")))
  (c-doc-comment-style `((c-mode   . gtkdoc)
                         ,(if (>= emacs-major-version 28)
                              '(c++-mode . doxygen)
                            '(c++-mode . gtkdoc))))
  (c-offsets-alist '((inline-open           . 0)
                     (brace-list-open       . 0)
                     (inextern-lang         . 0)
                     (statement-case-open   . 4)
                     (statement-cont        . (c-lineup-ternary-bodies +))
                     (access-label          . -)
                     (case-label            . 0)
                     (member-init-intro     . +)
                     (topmost-intro         . 0)
                     (inlambda              . 0) ;; better indentation for lambda
                     (innamespace           . -) ;; no indentation after namespace
                     (arglist-cont-nonempty . +))))

(use-package c-ts-mode
    :init (setq c-ts-mode-indent-offset 4))

(use-package clang-format
    :commands (clang-format
               clang-format-region
               clang-format-buffer))

(use-package modern-cpp-font-lock
  :diminish
  :init (modern-c++-font-lock-global-mode t))

;; Disassemble C/C++ code under cursor
(use-package disaster
  :commands (disaster))

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode)))

(use-package rtags
  :hook (c++-mode . rtags-start-process-unless-running)
  :config (setq rtags-completions-enabled t
                rtags-autostart-diagnostics t))

;; Highlight "#if 0" as comments
(use-package hideif
  :ensure nil
  :hook ((c-mode c++-mode) . hide-ifdef-mode)
  :config
  (when sys/linuxp
    (add-to-list 'hide-ifdef-env '(__linux__ . 1))
    (add-to-list 'hide-ifdef-env '(__GNUC__ . 11)))
  (when sys/macp
    (add-to-list 'hide-ifdef-env '(__APPLE__ . 1))
    (add-to-list 'hide-ifdef-env '(__clang__ . 1))
    (add-to-list 'hide-ifdef-env '(__llvm__ . 1)))
  :custom
  (hide-ifdef-initially t)
  (hide-ifdef-shadow t))


(use-package cpp-auto-include
  :config
  (evil-leader/set-key-for-mode 'c++-mode
    "moi" 'cpp-auto-include))

;;Live code checking.
(use-package srefactor
  :config
  (semantic-mode 1))

;; Expand C macros
;;
;; Useful when writing quick tests.
(use-package cmacexp
  :ensure nil
  :commands c-macro-expand
  :custom
  (c-macro-prompt-flag t)
  (c-macro-shrink-window-flag t))

(provide 'init-c)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
