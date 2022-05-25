;;; init-rust.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-rust.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:38:03 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 23
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

;; Rust
(use-package rustic
  :config
  (setq rustic-lsp-format t
        rustic-lsp-client 'eglot)
  (use-package cargo
    :diminish cargo-minor-mode
    :hook (rustic-mode . cargo-minor-mode)
    :config
    ;; To render buttons correctly, keep it at the last
    (setq compilation-filter-hook
          (append compilation-filter-hook '(cargo-process--add-errno-buttons)))
    (evil-leader/set-key-for-mode 'rustic-mode
      "mrr" 'cargo-process-run
      "mrb" 'cargo-process-run-bin
      "mre" 'cargo-process-run-example
      "mrm" 'cargo-process-rm
      "ma"  'cargo-process-add
      "mi"  'cargo-process-init
      "mn"  'cargo-process-new
      "mb"  'cargo-process-build
      "mf"  'cargo-process-fmt)))

(use-package rust-playground)

(provide 'init-rust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here
