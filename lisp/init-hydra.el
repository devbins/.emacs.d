;;; init-hydra.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-hydra.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:19:29 2020 (+0800)
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

(use-package pretty-hydra
  :bind ("<f6>" . toggles-hydra/body)
  :init
  (with-no-warnings
    (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                        &key face height v-adjust)
      "Add an icon in the hydra title."
      (let ((face (or face `(:foreground ,(face-background 'highlight))))
            (height (or height 1.0))
            (v-adjust (or v-adjust 0.0)))
        (concat
         (when (and (display-graphic-p) icon-type icon-name)
           (let ((f (intern (format "all-the-icons-%s" icon-type))))
             (when (fboundp f)
               (concat
                (apply f (list icon-name :face face :height height :v-adjust v-adjust))
                " "))))
         (propertize title 'face face))))

    ;; Global toggles
    (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on")
                                        :color amaranth :quit-key "q")
      ("Basic"
       (("n" (if (fboundp 'display-line-numbers-mode)
                 (display-line-numbers-mode (if display-line-numbers-mode -1 1))
               (global-linum-mode (if global-linum-mode -1 1)))
         "line number" :toggle (if (fboundp 'display-line-numbers-mode)
                                   display-line-numbers-mode
                                 global-linum-mode))
        ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
        ("h" global-hungry-delete-mode "hungry delete" :toggle t)
        ("e" electric-pair-mode "electric pair" :toggle t)
        ("c" flyspell-mode "spell check" :toggle t)
        ("S" prettify-symbols-mode "pretty symbol" :toggle t)
        ("L" global-page-break-lines-mode "page break lines" :toggle t)
        ("M" doom-modeline-mode "modern mode-line" :toggle t))
       "Highlight"
       (("l" global-hl-line-mode "line" :toggle t)
        ("P" show-paren-mode "paren" :toggle t)
        ("s" symbol-overlay-mode "symbol" :toggle t)
        ("r" rainbow-mode "rainbow" :toggle t)
        ("w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
         "whitespace" :toggle show-trailing-whitespace)
        ("d" rainbow-delimiters-mode "delimiter" :toggle t)
        ("i" highlight-indent-guides-mode "indent" :toggle t)
        ("T" global-hl-todo-mode "todo" :toggle t))
       "Coding"
       (("f" global-flycheck-mode "flycheck" :toggle t)
        ("F" flymake-mode "flymake" :toggle t)
        ("o" origami-mode "folding" :toggle t)
        ("O" hs-minor-mode "hideshow" :toggle t)
        ("u" subword-mode "subword" :toggle t)
        ("W" which-function-mode "which function" :toggle t)
        ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
        ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))
       "Version Control"
       (("v" global-diff-hl-mode "gutter" :toggle t)
        ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
        ("m" diff-hl-margin-mode "margin gutter" :toggle t)
        ("D" diff-hl-dired-mode "dired gutter" :toggle t))))))

(provide 'init-hydra)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hydra.el ends here
