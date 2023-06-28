;;; init-dart.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-dart.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:39:00 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 16
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

;; Dart
(use-package dart-mode
  :defines (projectile-project-root-files-bottom-up)
  :init (setq dart-format-on-save t)
  :config
  (when (eq my-lsp 'lsp-mode)
    (use-package lsp-dart
      :init (setq lsp-dart-sdk-dir "~/flutter/bin/cache/dart-sdk")
      :hook (dart-mode . lsp)))

  (defun flutter-add-package (package)
    "run flutter pub add package"
    (interactive "spackage name:")
    (let* ((cmd (executable-find "flutter")))
      (if cmd
          (shell-command (format "flutter pub add %s" package))
        (message "flutter binary not found"))))

  (evil-leader/set-key-for-mode 'dart-mode
    "mxx" 'flutter-run-or-hot-reload
    "mxp" 'flutter-add-package)

  (which-key-add-major-mode-key-based-replacements 'dart-mode
    "SPC m x" "flutter")

  (with-eval-after-load "projectile"
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD")))

(use-package flutter
  :defer t
  :after dart-mode
  :custom
  (flutter-sdk-path "~/flutter/"))

(provide 'init-dart)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dart.el ends here
