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
;;     Update #: 188
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

(pcase my-lsp
  ('lsp-bridge
   (use-package lsp-bridge
     :quelpa (lsp-bridge :fetcher github :repo "manateelazycat/lsp-bridge" :files ("*"))
     :hook (after-init . global-lsp-bridge-mode)
     :bind (:map lsp-bridge-mode-map
            ("C-c C-t" . lsp-bridge-toggle-sdcv-helper))
     :init
     (setq lsp-bridge-enable-mode-line nil)
     :config
     (require 'lsp-bridge-jdtls)
     (setq lsp-bridge-c-lsp-server "ccls"
           lsp-bridge-org-babel-lang-list '("go" "python" "ipython" "ruby" "js" "css" "sass" "c" "rust" "java" "cpp" "c++")))
   (use-package tree-sitter
     :hook (prog-mode . tree-sitter-hl-mode)
     :config
     (use-package tree-sitter-langs)
     (global-tree-sitter-mode)))
  ('eglot
   (use-package eglot
       :hook ((prog-mode . (lambda ()
                             (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode)
                               (eglot-ensure))))
              (markdown-mode . lsp-deferred))))
  ('lsp-mode
   ;; Emacs client for the Language Server Protocol
   ;; https://github.com/emacs-lsp/lsp-mode#supported-languages
   (use-package lsp-mode
     :defines (lsp-clients-python-library-directories)
     :commands (lsp-enable-which-key-integration
                lsp-format-buffer
                lsp-organize-imports)
     :diminish
     :hook ((prog-mode . (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                             (lsp-deferred))))
            (lsp-mode . (lambda ()
                          ;; Integrate `which-key'
                          (lsp-enable-which-key-integration)

                          ;; Format and organize imports
                          (unless (apply #'derived-mode-p '(c-mode c++-mode))
                            (add-hook 'before-save-hook #'lsp-format-buffer t t)
                            (add-hook 'before-save-hook #'lsp-organize-imports t t))))
            (lsp-completion-mode . my/lsp-mode-setup-completion))
     :bind (:map lsp-mode-map
            ("C-c C-d" . lsp-describe-thing-at-point)
            ([remap xref-find-definitions] . lsp-find-definition)
            ([remap xref-find-references] . lsp-find-references))
     :custom
     (lsp-completion-provider :none) ;; we use Corfu!
     :init
     (defun my/orderless-dispatch-flex-first (_pattern index _total)
       (and (eq index 0) 'orderless-flex))

     (defun my/lsp-mode-setup-completion ()
       (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
             '(orderless)))

     ;; Optionally configure the first word as flex filtered.
     (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

     ;; Optionally configure the cape-capf-buster.
     (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
     (setq read-process-output-max (* 1024 1024)
           lsp-keep-workspace-alive nil ; Auto-kill LSP server
           lsp-prefer-capf t
           lsp-signature-auto-activate nil
           lsp-modeline-code-actions-enable nil

           lsp-enable-file-watchers nil
           lsp-log-io nil
           lsp-eldoc-render-all nil
           lsp-completion-provider t
           lsp-signature-render-documentation nil
           lsp-enable-folding nil
           lsp-lens-enable nil
           lsp-enable-semantic-highlighting nil
           lsp-enable-symbol-highlighting nil
           lsp-enable-text-document-color nil
           lsp-keymap-prefix "C-c l"
           lsp-enable-indentation nil
           lsp-enable-on-type-formatting nil)

     (setq lsp-clients-python-library-directories '("~/anaconda3/bin/" "/usr/local/" "/usr/"))
     (setq lsp-rust-analyzer-cargo-watch-command "clippy"
           lsp-rust-analyzer-server-display-inlay-hints t
           lsp-rust-analyzer-proc-macro-enable t)
     :config
     ;; Configure LSP clients
     (with-no-warnings
       (defun my-lsp--init-if-visible (func &rest args)
         "Not enabling lsp in `git-timemachine-mode'."
         (unless (bound-and-true-p git-timemachine-mode)
           (apply func args)))
       (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)

       ;; Enable `lsp-mode' in sh/bash/zsh
       (defun my-lsp-bash-check-sh-shell (&rest _)
         (and (eq major-mode 'sh-mode)
            (memq sh-shell '(sh bash zsh))))
       (advice-add #'lsp-bash-check-sh-shell :override #'my-lsp-bash-check-sh-shell)

       ;; Only display icons in GUI
       (defun my-lsp-icons-get-symbol-kind (fn &rest args)
         (when (display-graphic-p)
           (apply fn args)))
       (advice-add #'lsp-icons-get-by-symbol-kind :around #'my-lsp-icons-get-symbol-kind)

       (defun my-lsp-icons-get-by-file-ext (fn &rest args)
         (when (display-graphic-p)
           (apply fn args)))
       (advice-add #'lsp-icons-get-by-file-ext :around #'my-lsp-icons-get-by-file-ext)

       (defun my-lsp-icons-all-the-icons-material-icon (icon-name face fallback &optional feature)
         (if (and (display-graphic-p)
                (functionp 'all-the-icons-material)
                (lsp-icons--enabled-for-feature feature))
             (all-the-icons-material icon-name :face face)
           (propertize fallback 'face face)))
       (advice-add #'lsp-icons-all-the-icons-material-icon :override #'my-lsp-icons-all-the-icons-material-icon)))

   (use-package lsp-ui
     :custom-face
     (lsp-ui-sideline-code-action ((t (:inherit warning))))
     :pretty-hydra
     ((:title (pretty-hydra-title "LSP UI" 'faicon "rocket" :face 'all-the-icons-green)
       :color amaranth :quit-key "q")
      ("Doc"
       (("d e" (progn
                 (lsp-ui-doc-enable (not lsp-ui-doc-mode))
                 (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)))
         "enable" :toggle lsp-ui-doc-mode)
        ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
         "signature" :toggle lsp-ui-doc-include-signature)
        ("d t" (setq lsp-ui-doc-position 'top)
         "top" :toggle (eq lsp-ui-doc-position 'top))
        ("d b" (setq lsp-ui-doc-position 'bottom)
         "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
        ("d p" (setq lsp-ui-doc-position 'at-point)
         "at point" :toggle (eq lsp-ui-doc-position 'at-point))
        ("d h" (setq lsp-ui-doc-header (not lsp-ui-doc-header))
         "header" :toggle lsp-ui-doc-header)
        ("d f" (setq lsp-ui-doc-alignment 'frame)
         "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
        ("d w" (setq lsp-ui-doc-alignment 'window)
         "align window" :toggle (eq lsp-ui-doc-alignment 'window)))
       "Sideline"
       (("s e" (progn
                 (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
                 (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)))
         "enable" :toggle lsp-ui-sideline-mode)
        ("s h" (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
         "hover" :toggle lsp-ui-sideline-show-hover)
        ("s d" (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics))
         "diagnostics" :toggle lsp-ui-sideline-show-diagnostics)
        ("s s" (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))
         "symbol" :toggle lsp-ui-sideline-show-symbol)
        ("s c" (setq lsp-ui-sideline-show-code-actions (not lsp-ui-sideline-show-code-actions))
         "code actions" :toggle lsp-ui-sideline-show-code-actions)
        ("s i" (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate))
         "ignore duplicate" :toggle lsp-ui-sideline-ignore-duplicate))
       "Action"
       (("h" backward-char "←")
        ("j" next-line "↓")
        ("k" previous-line "↑")
        ("l" forward-char "→")
        ("C-a" mwim-beginning-of-code-or-line nil)
        ("C-e" mwim-end-of-code-or-line nil)
        ("C-b" backward-char nil)
        ("C-n" next-line nil)
        ("C-p" previous-line nil)
        ("C-f" forward-char nil)
        ("M-b" backward-word nil)
        ("M-f" forward-word nil)
        ("c" lsp-ui-sideline-apply-code-actions "apply code actions"))))
     :bind (("C-c u" . lsp-ui-imenu)
            :map lsp-ui-mode-map
            ("M-<f6>" . lsp-ui-hydra/body)
            ("s-<return>" . lsp-ui-sideline-apply-code-actions)
            ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
            ([remap xref-find-references] . lsp-ui-peek-find-references))
     :hook (lsp-mode . lsp-ui-mode)
     :init (setq lsp-ui-doc-enable t
                 lsp-ui-doc-header t
                 lsp-ui-doc-use-webkit nil
                 lsp-ui-doc-delay 0.5
                 lsp-ui-doc-include-signature t
                 ;;lsp-ui-doc-border (face-foreground 'default)
                 lsp-ui-doc-border "violet"
                 lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer

                 lsp-ui-flycheck-enable t
                 lsp-ui-peek-always-show t
                 lsp-ui-sideline-enable t
                 lsp-ui-sideline-show-hover nil
                 lsp-ui-sideline-show-diagnostics nil
                 lsp-ui-sideline-ignore-duplicate t
                 lsp-ui-sideline-show-code-actions nil

                 lsp-ui-imenu-enable t
                 lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                       ,(face-foreground 'font-lock-string-face)
                                       ,(face-foreground 'font-lock-constant-face)
                                       ,(face-foreground 'font-lock-variable-name-face))))

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
   ;; Java
   (use-package lsp-java
     :hook (java-mode . (lambda () (require 'lsp-java))))))

(unless (eq my-lsp 'lsp-bridge)
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
             (pcase my-lsp
               ('eglot
                (when (fboundp 'eglot-ensure)
                  (eglot-ensure)))
               ('lsp-mode
                (when (fboundp 'lsp-deferred)
                  ;; Avoid headerline conflicts
                  (setq-local lsp-headerline-breadcrumb-enable nil)
                  (lsp-deferred)))
               (_
                (user-error "LSP:: invalid `my-lsp' type"))))
           (put ',intern-pre 'function-documentation
                (format "Enable `%s' in the buffer of org source block (%s)."
                        my-lsp (upcase ,lang)))

           (if (fboundp ',edit-pre)
               (advice-add ',edit-pre :after ',intern-pre)
             (progn
               (defun ,edit-pre (info)
                 (,intern-pre info))
               (put ',edit-pre 'function-documentation
                    (format "Prepare local buffer environment for org source block (%s)."
                            (upcase ,lang))))))))

    (defvar org-babel-lang-list
      '("go" "python" "ipython" "ruby" "js" "css" "sass" "c" "rust" "java" "cpp" "c++"))
    (add-to-list 'org-babel-lang-list (if emacs/>=26p "shell" "sh"))
    (dolist (lang org-babel-lang-list)
      (eval `(lsp-org-babel-enable ,lang))))

(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
