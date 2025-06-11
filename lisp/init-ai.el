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
;;     Update #: 245
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

;; prompt download to ~/.cache/gptel-crowdsourced-prompts.csv
(use-package gptel
  :hook ((gptel-post-stream . gptel-auto-scroll)
         (gptel-post-response-functions . gptel-end-of-response))
  :config
  (setq gptel-model 'qwen3:14b
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :models '(deepseek-r1:14b
                                  (qwen3:14b :description "Qwen3 is the latest generation of large language models in Qwen series, offering a comprehensive suite of dense and mixture-of-experts (MoE) models."
                                             :capabilities (tool json))
                                  (devstral:latest :description "Devstral excels at using tools to explore codebases, editing multiple files and power software engineering agents."
                                                   :capabilities (tool json))
                                  (gemma3:12b :description "the Gemma 3 models are multimodal—processing text and images—and feature a 128K context window with support for over 140 languages. "
                                              :capabilities (json media)
                                              :mime-types ("image/jpeg" "image/png" "image/webp" "image/heic" "image/heif" "text/plain" "text/csv" "text/html")))
                        :stream t)
        gptel-track-media t
        gptel-use-tools t
        gptel-default-mode 'org-mode
        gptel-prompt-prefix-alist '((markdown-mode . "## ")
                                    (org-mode . "** ")
                                    (text-mode . "## ")))

    (add-to-list 'gptel-directives
                 `(translate . ,(concat "You are a large language model and a writing assistant. Respond concisely."
                                       "  Follow my instructions and improve or rewrite the text I provide."
                                       "  Generate ONLY the replacement text,"
                                       " without any explanation or markdown code fences or org code fences."
                                       " translate english to chinese.")))

(defun gptel-translate-to-chinese(&optional dry-run)
  "Use AI to translate the currently selected text into Chinese."
  (interactive "P")
  (gptel-request (list (or (get-char-property (point) 'gptel-rewrite)
                          (buffer-substring-no-properties (region-beginning) (region-end)))
                       "What is the required change?"
                       "Rewrite:")
    :dry-run dry-run
    :system (alist-get 'translate gptel-directives)
    :stream t
    :context
    (let ((ov (or (cdr-safe (get-char-property-and-overlay (point) 'gptel-rewrite))
                 (make-overlay (region-beginning) (region-end) nil t))))
      (overlay-put ov 'category 'gptel)
      (overlay-put ov 'evaporate t)
      (cons ov (generate-new-buffer "*gptel-rewrite*")))
    :callback #'gptel--rewrite-callback))

(with-eval-after-load 'gptel-transient
  (transient-append-suffix 'gptel-menu '(2 -1)
    ["Quick Tools"
     ("q t" "Translate select regions to chinese" gptel-translate-to-chinese)]))

  (require 'gptel-integrations)

  (gptel-make-openai "awoi"
    :key (auth-source-pass-get 'secret "awoi")
    :models '(gpt-4o
              gpt-4-turbo)
    :host "api.awoi.me"
    :stream t)

  (gptel-make-openai "kimi"
    :key (auth-source-pass-get 'secret "kimi")
    :models '(moonshot-v1-8k
              moonshot-v1-32k
              moonshot-v1-128k)
    :host "api.moonshot.cn"
    :stream t)

  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key (auth-source-pass-get 'secret "openrouter")
    :models '(google/gemini-2.5-pro-exp-03-25:free
              qwen/qwen3-30b-a3b:free
              deepseek/deepseek-chat-v3-0324:free
              deepseek/deepseek-r1-0528:free
              deepseek/deepseek-r1-0528-qwen3-8b:free
              mistralai/devstral-small:free
              google/gemini-2.0-flash-exp:free
              google/gemini-2.0-flash-thinking-exp:free
              google/gemma-3-27b-it:free))

  (gptel-make-openai "siliconflow"
    :host "api.siliconflow.cn"
    :key (auth-source-pass-get 'secret "siliconflow")
    :models '(deepseek-ai/DeepSeek-R1) :stream t)

  (gptel-make-gemini
   "Gemini"
   :key (auth-source-pass-get 'secret "gemini")
   :stream t))

(use-package mcp
  :after gptel
  :config
  (require 'mcp-hub)
  (setq mcp-hub-servers
        '(
          ("ddg-search" . (:command "uvx" :args ("duckduckgo-mcp-server")))
          ;; https://github.com/nickclyde/duckduckgo-mcp-server
          ("playwright" . (:command "npx" :args ("@playwright/mcp@latest")))
          ("github" . (:command "docker"
                       :args ("run"
                              "--name" "github-mcp"
                              "--interactive"
                              "--rm"
                              "--env"
                              "GITHUB_PERSONAL_ACCESS_TOKEN"
                              "ghcr.io/github/github-mcp-server")
                       :env (:GITHUB_PERSONAL_ACCESS_TOKEN (auth-source-pass-get 'secret "github"))))
          )))

(use-package magit-gptcommit
  :hook (after-init . magit-gptcommit-status-buffer-setup)
  :config
  (setq magit-gptcommit-llm-provider (make-llm-ollama :chat-model "qwen3:14b" :embedding-model "bge-m3:latest"))
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
         :chat-model "qwen3:14b" :embedding-model "bge-m3:latest"))
  (setq ellama-providers
        '(("deepseek-r1:14b" . (make-llm-ollama :chat-model "deepseek-r1:14b" :embedding-model "bge-m3:latest"))
          ("qwen2.5-coder:latest" . (make-llm-ollama :chat-model "qwen2.5-coder:latest" :embedding-model "bge-m3:latest")))))

(use-package aidermacs
  :commands (aidermacs-transient-menu)
  :config
  (require 'aidermacs-backend-vterm nil t)
  (setopt aidermacs-vterm-use-theme-colors nil)
  (setq aidermacs-backend 'vterm)
  (setq aidermacs-default-model "opentrouter/qwen/qwen3-30b-a3b:free")
  (setq aidermacs-use-architect-mode t) ;; set t default-mode will be ignore
  (setq aidermacs-architect-model "openrouter/deepseek/deepseek-r1-0528:free"
        aidermacs-editor-model "openrouter/mistralai/devstral-small:free")
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")
  (setenv "OPENROUTER_API_KEY" (auth-source-pass-get 'secret "openrouter"))
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c C-a") 'aidermacs-transient-menu))

(use-package gptel-aibo
  :load-path "site-lisp/gptel-aibo"
  :commands (gptel-aibo)
  :bind ((:map gptel-aibo-mode-map ("C-c /" . gptel-aibo-apply-last-suggestions))
         (:map gptel-aibo-complete-mode-map ("C-c i" . gptel-aibo-complete-at-point))))

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
