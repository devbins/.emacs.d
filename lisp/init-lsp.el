;;; init-lsp.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-lsp.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:35:57 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 109
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

(use-package lsp-bridge
  :quelpa (eaf :fetcher github :repo "manateelazycat/lsp-bridge" :files ("*"))
  :commands (lsp-bridge-mode)
  :config
  (require 'lsp-bridge-orderless)
           (require 'lsp-bridge-icon)
  (dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'python-mode-hook
               'rust-mode-hook
               'go-mode-hook
               'dart-mode-hook
               'typescript-mode-hook
               'js2-mode-hook
               'js-mode-hook))
    (add-hook hook (lambda ()
                     (lsp-bridge-enable)))))

;; Debug
(use-package dap-mode
  :diminish
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((after-init . dap-auto-configure-mode)
         (dap-stopped . (lambda (_args) (dap-hydra)))
         (dap-terminated . (lambda (_args) (dap-hydra/nil)))

         (python-mode . (lambda () (require 'dap-python)))
         (ruby-mode . (lambda () (require 'dap-ruby)))
         (go-mode . (lambda () (require 'dap-go)))
         (java-mode . (lambda () (require 'dap-java)))
         ((c-mode c++-mode objc-mode swift-mode) . (lambda ()
                                                     (require 'dap-gdb-lldb)
                                                     (setq dap-lldb-debug-program '("/usr/local/opt/llvm/bin/lldb-vscode"))))
         (php-mode . (lambda () (require 'dap-php)))
         ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
         (powershell-mode . (lambda () (require 'dap-pwsh)))))

;; `lsp-mode' and `treemacs' integration
(use-package lsp-treemacs
  :after lsp-mode
  :bind (:map lsp-mode-map
         ("C-<f8>" . lsp-treemacs-errors-list)
         ("M-<f8>" . lsp-treemacs-symbols)
         ("s-<f8>" . lsp-treemacs-java-deps-list))
  :config
  (with-eval-after-load 'ace-window
    (when (boundp 'aw-ignored-buffers)
      (push 'lsp-treemacs-symbols-mode aw-ignored-buffers)
      (push 'lsp-treemacs-java-deps-mode aw-ignored-buffers))))

;; Python: pyright
(use-package lsp-pyright
  :preface
  ;; Use yapf to format
  (defun lsp-pyright-format-buffer ()
    (interactive)
    (when (and (executable-find "yapf") buffer-file-name)
      (call-process "yapf" nil nil nil "-i" buffer-file-name)))
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3"))
  :config
  (defun expand-absolute-name (name)
    (if (file-name-absolute-p name)
        (tramp-file-local-name
         (expand-file-name
          (concat (file-remote-p default-directory) name)))
      name))

   (lsp-register-custom-settings
   `(("python.analysis.stubPath" (lambda () (expand-absolute-name lsp-pyright-stub-path)))
     ("python.venvPath" (lambda () (if lsp-pyright-venv-path
                                  (expand-absolute-name lsp-pyright-venv-path) "")))))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection (lambda ()
                                                            (cons "pyright-langserver"
                                                                  lsp-pyright-langserver-command-args)))
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyright-remote
                    :multi-root lsp-pyright-multi-root
                    :initialization-options (lambda () (ht-merge (lsp-configuration-section "pyright")
                                                            (lsp-configuration-section "python")))
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                        (lsp--set-configuration
                                         (make-hash-table :test 'equal))))
                    :download-server-fn (lambda (_client callback error-callback _update?)
                                          (lsp-package-ensure 'pyright callback error-callback))
                    :notification-handlers (lsp-ht ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
                                                   ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
                                                   ("pyright/endProgress" 'lsp-pyright--end-progress-callback)))))

;; C/C++/Objective-C support
(use-package ccls
  :defines projectile-project-root-files-top-down-recurring
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
  :config
  (setq ccls-sem-highlight-method 'font-lock)
  (ccls-use-default-rainbow-sem-highlight)
  ;; https://github.com/maskray/ccls/blob/master/src/config.h
  (setq ccls-initialization-options
        `(:clang
          (:excludeArgs
           ;; Linux's gcc options. See ccls/wiki
           ["-falign-jumps=1" "-falign-loops=1" "-fconserve-stack" "-fmerge-constants" "-fno-code-hoisting" "-fno-schedule-insns" "-fno-var-tracking-assignments" "-fsched-pressure"
            "-mhard-float" "-mindirect-branch-register" "-mindirect-branch=thunk-inline" "-mpreferred-stack-boundary=2" "-mpreferred-stack-boundary=3" "-mpreferred-stack-boundary=4" "-mrecord-mcount" "-mindirect-branch=thunk-extern" "-mno-fp-ret-in-387" "-mskip-rax-setup"
            "--param=allow-store-data-races=0" "-Wa arch/x86/kernel/macros.s" "-Wa -"]
           :extraArgs []
           :pathMappings [])
          :completion
          (:include
           (:blacklist
            ["^/usr/(local/)?include/c\\+\\+/[0-9\\.]+/(bits|tr1|tr2|profile|ext|debug)/"
             "^/usr/(local/)?include/c\\+\\+/v1/"
             ]))
          :index (:initialBlacklist [] :parametersInDeclarations :json-false :trackDependency 1)))
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
    (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json"
                    ".ccls")
                  projectile-project-root-files-top-down-recurring)))
  (with-no-warnings
         ;; FIXME: fail to call ccls.xref
         ;; @see https://github.com/emacs-lsp/emacs-ccls/issues/109
         (cl-defmethod my-lsp-execute-command
           ((_server (eql ccls)) (command (eql ccls.xref)) arguments)
           (when-let ((xrefs (lsp--locations-to-xref-items
                              (lsp--send-execute-command (symbol-name command) arguments))))
             (xref--show-xrefs xrefs nil)))
         (advice-add #'lsp-execute-command :override #'my-lsp-execute-command)))

;; Swift/C/C++/Objective-C
(use-package lsp-sourcekit
  :if sys/macp
  :init (setq lsp-sourcekit-executable
              "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

;; Java support
(use-package lsp-java
  :hook (java-mode . (lambda () (require 'lsp-java))))

;; Enable LSP in org babel
;; https://github.com/emacs-lsp/lsp-mode/issues/377
(cl-defmacro lsp-org-babel-enable (lang)
      "Support LANG in org source code block."
      (cl-check-type lang stringp)
      (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
             (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
        `(progn
           (defun ,intern-pre (info)
             (setq buffer-file-name (or (->> info caddr (alist-get :file))
                                       "org-src-babel.tmp"))
             (when (fboundp 'lsp-deferred)
               ;; Avoid headerline conflicts
               (setq-local lsp-headerline-breadcrumb-enable nil)
               (lsp-deferred)))
           (put ',intern-pre 'function-documentation
                (format "Enable lsp in the buffer of org source block (%s)." (upcase ,lang)))

           (if (fboundp ',edit-pre)
               (advice-add ',edit-pre :after ',intern-pre)
             (progn
               (defun ,edit-pre (info)
                 (,intern-pre info))
               (put ',edit-pre 'function-documentation
                    (format "Prepare local buffer environment for org source block (%s)."
                            (upcase ,lang))))))))

(defvar org-babel-lang-list
  '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "c" "cpp" "c++" "rust" "java"))
(add-to-list 'org-babel-lang-list (if emacs/>=26p "shell" "sh"))
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))

(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
