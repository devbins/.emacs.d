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
;;     Update #: 10
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
  :bind (:map go-mode-map
         ("C-c R" . go-remove-unused-imports)
         ("<f1>" . godoc-at-point))
  :init (setq godoc-at-point-function #'godoc-gogetdoc)
  :config
  ;; Install or update tools
  (defvar go--tools '("golang.org/x/tools/gopls"
                      "golang.org/x/tools/cmd/goimports"
                      "honnef.co/go/tools/cmd/staticcheck"
                      "github.com/go-delve/delve/cmd/dlv"
                      "github.com/zmb3/gogetdoc"
                      "github.com/josharian/impl"
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
  (use-package go-dlv)
  (use-package go-fill-struct)
  (use-package go-impl)
  (use-package go-rename)
  (use-package godoctor)

  ;; Install: See https://github.com/golangci/golangci-lint#install
  (use-package flycheck-golangci-lint
    :if (executable-find "golangci-lint")
    :after flycheck
    :defines flycheck-disabled-checkers
    :hook (go-mode . (lambda ()
                       "Enable golangci-lint."
                       (setq flycheck-disabled-checkers '(go-gofmt
                                                          go-golint
                                                          go-vet
                                                          go-build
                                                          go-test
                                                          go-errcheck))
                       (flycheck-golangci-lint-setup))))

  (use-package go-tag
    :bind (:map go-mode-map
           ("C-c t t" . go-tag-add)
           ("C-c t T" . go-tag-remove))
    :init (setq go-tag-args (list "-transform" "camelcase")))

  (use-package go-gen-test
    :bind (:map go-mode-map
           ("C-c t g" . go-gen-test-dwim)))

  (use-package gotest
    :bind (:map go-mode-map
           ("C-c t a" . go-test-current-project)
           ("C-c t m" . go-test-current-file)
           ("C-c t ." . go-test-current-test)
           ("C-c t x" . go-run)))

  (defun go-run-main ()
    (interactive)
    (shell-command
     (format "go run %s"
             (shell-quote-argument (buffer-file-name)))))

  (evil-leader/set-key-for-mode 'go-mode
    "mig" 'go-goto-imports
    "mir" 'go-remove-unused-imports
    "mia" 'go-import-add
    "meb" 'go-play-buffer
    "med" 'go-download-play
    "mer" 'go-play-region
    "mga" 'ff-find-other-file
    "mgc" 'go-coverage
    "mhh" 'godoc-at-point
    "mrf" 'go-tag-add
    "mrF" 'go-tag-remove
    "mrN" 'go-rename
    "mri" 'go-impl
    "mrs" 'go-fill-struct
    "mrd" 'godoctor-godoc
    "mre" 'godoctor-extract
    "mrn" 'godoctor-rename
    "mrt" 'godoctor-toggle
    "m="  'gofmt
    "mxx" 'go-run-main
    "mhd" 'godef-describe))

;; Local Golang playground for short snippets
(use-package go-playground
  :diminish)

(provide 'init-go)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-go.el ends here
