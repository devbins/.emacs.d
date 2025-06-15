;;; init-go.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-go.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:37:51 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 54
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

;; Golang
(use-package go-mode
  :functions go-update-tools
  :commands godoc-gogetdoc
  :bind (:map go-ts-mode-map
         ("C-c R" . go-remove-unused-imports)
         ("<f1>" . godoc-at-point))
  :init (setq godoc-at-point-function #'godoc-gogetdoc)
  :config
  ;; Install or update tools
  (defvar go--tools '("golang.org/x/tools/gopls"
                      "golang.org/x/tools/cmd/goimports"
                      "honnef.co/go/tools/cmd/staticcheck"
                      "github.com/godoctor/godoctor"
                      "github.com/go-delve/delve/cmd/dlv"
                      "github.com/zmb3/gogetdoc"
                      "github.com/josharian/impl"
                      "golang.org/x/tools/cmd/godoc"
                      "github.com/cweill/gotests/..."
                      "github.com/fatih/gomodifytags"
                      "github.com/davidrjenni/reftools/cmd/fillstruct")
    "All necessary go tools.")

  (defun go-update-tools ()
    "Install or update go tools."
    (interactive)
    (unless (executable-find "go")
      (user-error "Unable to find `go' in `exec-path'!"))

    (message "Installing go tools...")

    (dolist (pkg go--tools)
      (set-process-sentinel
       (start-process "go-tools" "*Go Tools*" "go" "install" "-v" "-x" (concat pkg "@latest"))
       (lambda (proc _)
         (let ((status (process-exit-status proc)))
           (if (= 0 status)
               (message "Installed %s" pkg)
             (message "Failed to install %s: %d" pkg status)))))))

  ;; Try to install go tools if `gopls' is not found
  (unless (executable-find "gopls")
    (go-update-tools))

  ;; Misc
  (use-package go-dlv) ;; debug go
  (use-package go-fill-struct)
  (use-package go-impl)
  (use-package godoctor)

  ;; go install github.com/fatih/gomodifytags@latest
  (use-package go-tag
    :init (setq go-tag-args (list "-transform" "camelcase")))

  (use-package go-gen-test)

  (use-package gotest)

  (evil-leader/set-key-for-mode 'go-ts-mode
    "mig" 'go-goto-imports
    "mir" 'go-remove-unused-imports
    "mia" 'go-import-add
    "meb" 'go-play-buffer
    "med" 'go-download-play
    "mer" 'go-play-region
    "mga" 'ff-find-other-file
    "mgc" 'go-coverage
    "mri" 'go-impl
    "mrf" 'go-fill-struct
    "mrd" 'godoctor-godoc
    "mre" 'godoctor-extract
    "mrn" 'godoctor-rename
    "m="  'gofmt
    "mxx" 'go-run
    "mta" 'go-tag-add
    "mtr" 'go-tag-remove
    "mtd" 'go-gen-test-dwim
    "mtt" 'go-test-current-test
    "mtf" 'go-test-current-file
    "mtp" 'go-test-current-project
    "mhh" 'godoc-at-point
    "mhd" 'godef-describe)
  (which-key-add-major-mode-key-based-replacements 'go-ts-mode
    "SPC m i" "import"
    "SPC m e" "playground"
    "SPC m g" "coverage/gen"
    "SPC m h" "doc"
    "SPC m r" "refactor"
    "SPC m t" "tag/test"
    "SPC m x" "run"))

;; Local Golang playground for short snippets
(use-package go-playground
  :diminish)

(use-package go-ts-mode
  :init (setq go-ts-mode-indent-offset 4))

(provide 'init-go)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-go.el ends here
