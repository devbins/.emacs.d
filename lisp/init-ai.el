;;; init-ai.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-ai.el
;; Description:
;; Author: binsheng
;; Maintainer:
;; Copyright (C) 2019 binsheng
;; Created: Wed Jan  3 10:39:11 2024 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 137
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

(use-package gptel
  :hook ((gptel-post-stream . gptel-auto-scroll)
         (gptel-post-response . gptel-end-of-response))
  :config
  (setq gptel-model 'qwen2.5:14b
        gptel-backend (gptel-make-ollama "Ollama" :host "localhost:11434" :models '(deepseek-r1:14b qwen2.5:14b qwen2.5-coder:latest  llava-llama3:latest) :stream t)
        gptel-track-media t
        gptel-default-mode 'org-mode
        gptel-prompt-prefix-alist '((markdown-mode . "## ")
                                    (org-mode . "** ")
                                    (text-mode . "## ")))
  (gptel-make-openai "awoi"
    :key (password-store-get "awoi")
    :models '(gpt-4o
              gpt-4-turbo)
    :host "api.awoi.me"
    :stream t)

  (gptel-make-openai "kimi"
    :key (password-store-get "kimi")
    :models '(moonshot-v1-8k
              moonshot-v1-32k
              moonshot-v1-128k)
    :host "api.moonshot.cn"
    :stream t)

  (gptel-make-gemini
   "Gemini"
   :key (password-store-get "gemini")
   :stream t))

(use-package magit-gptcommit
  :hook (after-init . magit-gptcommit-status-buffer-setup)
  :config
  (setq magit-gptcommit-llm-provider (make-llm-ollama :chat-model "qwen2.5-coder:latest" :embedding-model "bge-m3:latest"))
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept)))

(use-package ellama
  :init
  (setq ellama-language "Chinese"
        ellama-auto-scroll t)
  (require 'llm-ollama)
  :config
  (setq ellama-provider
        (make-llm-ollama
         :chat-model "qwen2.5:14b" :embedding-model "bge-m3:latest"))
  (setq ellama-providers
        '(("deepseek-r1:14b" . (make-llm-ollama :chat-model "deepseek-r1:14b" :embedding-model "bge-m3:latest"))
          ("qwen2.5-coder:latest" . (make-llm-ollama :chat-model "qwen2.5-coder:latest" :embedding-model "bge-m3:latest"))
          ("llava-llama3:latest" . (make-llm-ollama :chat-model "llava-llama3:latest" :embedding-model "bge-m3:latest")))))

(use-package aider
  :load-path "site-lisp/aider"
  :commands (aider-transient-menu)
  :config
  (setq aider-args '("--model" "ollama/qwen2.5-coder:latest" "--no-auto-commits"))
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
