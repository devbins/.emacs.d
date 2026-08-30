;;; init-ai.el --- -*- lexical-binding: t no-byte-compile: t; -*-
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
;;     Update #: 396
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
                                  (deepseek-ocr:latest :description "DeepSeek-OCR is a vision-language model that can perform token-efficient OCR." :capabilities (json media) :mime-types ("image/jpeg" "image/png"))
                                  (gemma4:latest :description "Gemma 4 models are designed to deliver frontier-level performance at each size. They are well-suited for reasoning, agentic workflows, coding, and multimodal understanding."
                                                 :capabilities (json media)
                                                 :mime-types ("image/jpeg" "image/png" "image/webp" "image/heic" "image/heif" "text/plain" "text/csv" "text/html")))
                        :stream t)
        gptel-track-media t
        gptel-use-tools t
        gptel-expert-commands t
        gptel-default-mode 'org-mode
        gptel-prompt-prefix-alist '((markdown-mode . "# ")
                                    (org-mode . "* ")
                                    (text-mode . "# ")))

  (add-to-list 'gptel-directives
               `(translate . ,(concat "You are a large language model and a writing assistant. Respond concisely."
                                      "  Follow my instructions and improve or rewrite the text I provide."
                                      "  Generate ONLY the replacement text,"
                                      " without any explanation or markdown code fences or org code fences."
                                      " translate english to chinese.")))

  (defun toggle-gptel-proxy()
    "toggle gptel proxy"
    (interactive)
    (setq gptel-proxy (if (string-empty-p gptel-proxy)
                          http-proxy
                        ""))
    (message (if (string-empty-p gptel-proxy)
                 "gptel proxy disabled"
               "enabled gptel proxy")))

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

  (gptel-make-openai "lm studio"
    :protocol "http"
    :host "localhost:1234"
    :stream t
    :models '((openai/gpt-oss-20b :capabilities (tool json url))))

  (gptel-make-anthropic "omlx"
    :protocol "http"
    :host "localhost:8000"
    :stream t
    :models '((Qwen3.8-27B-4bit :capabilities (media tool json) :mime-types ("image/jpeg" "image/png" "image/webp" "image/heic" "image/heif" "text/plain" "text/csv" "text/html"))))

  (gptel-make-openai "bailian"
    :key (auth-source-pass-get 'secret "bailian")
    :stream t
    :host "dashscope.aliyuncs.com"
    :endpoint "/compatible-mode/v1/chat/completions"
    :models '(tongyi-xiaomi-analysis-pro qwen3.5-plus qwen3-coder-plus))

  (gptel-make-openai "nvidia"
    :key (auth-source-pass-get 'secret "nvidia")
    :host "integrate.api.nvidia.com"
    :stream t
    :models '(z-ai/glm-5.2 minimaxai/minimax-m3 minimaxai/minimax-m2.7 moonshotai/kimi-k2.6 qwen/qwen3.5-397b-a17b))

  (gptel-make-openai "modelscope"
    :key (auth-source-pass-get 'secret "modelscope")
    :models '(Qwen/Qwen3.5-397B-A17B
              Qwen/Qwen3-Next-80B-A3B-Instruct
              Qwen/Qwen3-Coder-480B-A35B-Instruct
              deepseek-ai/DeepSeek-V4-Pro
              deepseek-ai/DeepSeek-V4-Flash
              ZhipuAI/GLM-5.1
              MiniMax/MiniMax-M2.7
              moonshotai/Kimi-K2.5)
    :host "api-inference.modelscope.cn"
    :stream t)


  (gptel-make-openai "kimi"
    :key (auth-source-pass-get 'secret "kimi")
    :models '(kimi-latest
              kimi-k2-0711-preview
              kimi-k2-turbo-preview)
    :host "api.moonshot.cn"
    :stream t)

  (gptel-make-openai "mimo"
    :host "api.xiaomimimo.com"
    :key (auth-source-pass-get 'secret "mimo")
    :models  '(mimo-v2.5-pro mimo-v2.5)
    :stream t)

  (gptel-make-openai "Github"
    :host "models.github.ai"
    :endpoint "/inference/chat/completions"
    :stream t
    :key (auth-source-pass-get 'secret "github")
    :models '(gpt-4o))

  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key (auth-source-pass-get 'secret "openrouter")
    :models '(deepseek/deepseek-v4-flash
              deepseek/deepseek-v4-pro
              z-ai/glm-5.2
              moonshotai/kimi-k3))

  (gptel-make-openai "siliconflow"
    :host "api.siliconflow.cn"
    :key (auth-source-pass-get 'secret "siliconflow")
    :models '(deepseek-ai/DeepSeek-R1 Pro/zai-org/GLM-5 Pro/MiniMaxAI/MiniMax-M2.5 Pro/moonshotai/Kimi-K2.5) :stream t)

  (gptel-make-openai "providers"
    :host "http://localhost:8317"
    :key (auth-source-pass-get 'secret "providers")
    :models '(mimo-v2.5 deepseek-v4-flash deepseek-v4-pro) :stream t)


  (gptel-make-gemini "Gemini"
    :key (auth-source-pass-get 'secret "gemini")
    :stream t))


(use-package mcp
  :config
  (require 'mcp-hub)
  (setq mcp-hub-servers
        `(("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
          ("ddg-search" . (:command "uvx" :args ("duckduckgo-mcp-server"))) ;; https://github.com/nickclyde/duckduckgo-mcp-server
          ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
          ("anysearch" . (:url "https://api.anysearch.com/mcp" :headers (("Authorization" . ,(auth-source-pass-get 'secret "anysearch")))))
          ("playwright" . (:command "npx" :args ("@playwright/mcp@latest")))
          ("context7" . (:url "https://mcp.context7.com/mcp"))
          ("amap-maps" . (:command "npx" :args ("-y" "@amap/amap-maps-mcp-server") :env (:AMAP_MAPS_API_KEY ,(auth-source-pass-get 'secret "amap"))))
          ("chrome-devtools" . (:command "npx" :args ("-y" "chrome-devtools-mcp@latest")))
          ("github" . (:command "docker"
                       :args ("run" "--name" "github-mcp" "--interactive" "--rm" "--env" "GITHUB_PERSONAL_ACCESS_TOKEN" "ghcr.io/github/github-mcp-server")
                       :env (:GITHUB_PERSONAL_ACCESS_TOKEN ,(auth-source-pass-get 'secret "github")))))))

;; (require 'llm-openai)
;; (setq magit-gptcommit-llm-provider (make-llm-openai-compatible :url "http://localhost:1234/v1/" :chat-model "openai/gpt-oss-20b"))
(use-package magit-gptcommit
  :after magit
  :commands (magit-gptcommit-status-buffer-setup)
  :bind (:map git-commit-mode-map
         ("C-c C-g" . magit-gptcommit-commit-accept))
  :init
  (require 'llm-ollama)
  (setq magit-gptcommit-llm-provider (make-llm-ollama :chat-model "gemma4:latest" :embedding-model "bge-m3:latest"))
  (magit-gptcommit-status-buffer-setup))

(defun my-ai-code-notify (title message)
  "Display a macOS notification with sound."
  (call-process "osascript" nil nil nil
                "-e" (format "display notification \"%s\" with title \"%s\" sound name \"Glass\""
                             message title)))

;; LLM 提供商配置
(defvar my-llm-provider
  '(:base-url "http://localhost:8317"
    :auth-key "providers"
    :opus-model "deepseek-v4-pro[1m]"
    :sonnet-model "deepseek-v4-flash[1m]"
    :haiku-model "deepseek-v4-flash[1m]")
  "当前 LLM 提供商配置。
:base-url  - API 地址
:auth-key  - auth-source-pass 中的密钥名称
:auth-token - 直接指定的 token (与 :auth-key 二选一)
:opus-model - Opus 层级默认模型
:sonnet-model - Sonnet 层级默认模型
:haiku-model - Haiku 层级默认模型")

(defun my-llm--auth-token ()
  "获取当前 provider 的认证 token。"
  (let ((key (plist-get my-llm-provider :auth-key)))
    (if (and key (not (string-empty-p key)))
        (auth-source-pass-get 'secret key)
      (plist-get my-llm-provider :auth-token))))

(defun my-llm--fetch-models ()
  "从 API 获取可用模型列表。"
  (let* ((base-url (string-trim-right (plist-get my-llm-provider :base-url) "/"))
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " (my-llm--auth-token))))))
    (with-current-buffer
        (url-retrieve-synchronously (concat base-url "/v1/models") nil t 10)
      (goto-char (point-min))
      (re-search-forward "\n\n" nil t)
      (let* ((json-object-type 'alist)
             (data (json-read)))
        (kill-buffer)
        ;; 兼容 {"data": [{"id": "xxx"}]} 和 ["xxx"] 两种格式
        (if (listp (car data))
            (mapcar (lambda (m) (cdr (assoc 'id m)))
                    (cdr (assoc 'data data)))
          data)))))

(defun my-llm-switch-model ()
  "从 API 获取模型列表，为 Opus/Sonnet/Haiku 各选一个模型并设置环境变量。"
  (interactive)
  (let ((models (my-llm--fetch-models)))
    (unless models (user-error "No models available"))
    (let ((default (car models)))
      (dolist (tier '("Opus" "Sonnet" "Haiku"))
        (let* ((model (completing-read (format "%s: " tier)
                                       models nil t nil nil default))
               (key (intern (concat ":" (downcase tier) "-model"))))
          (setenv (concat "ANTHROPIC_DEFAULT_" (upcase tier) "_MODEL") (concat model "[1m]"))
          (setq my-llm-provider (plist-put my-llm-provider key (concat model "[1m]"))))))
    (my-llm-update-agent-shell-env)
    (message "Models set: opus=%s sonnet=%s haiku=%s"
             (getenv "ANTHROPIC_DEFAULT_OPUS_MODEL")
             (getenv "ANTHROPIC_DEFAULT_SONNET_MODEL")
             (getenv "ANTHROPIC_DEFAULT_HAIKU_MODEL"))))

(defun my-llm-get-env-for-agent-shell ()
  "获取当前环境变量配置，用于 agent-shell。
从 my-llm-provider 读取所有配置。"
  (let* ((base-url (plist-get my-llm-provider :base-url))
         (auth-token (my-llm--auth-token))
         (opus_model (plist-get my-llm-provider :opus-model))
         (sonnet_model (plist-get my-llm-provider :sonnet-model))
         (haiku_model (plist-get my-llm-provider :haiku-model)))
    (apply #'agent-shell-make-environment-variables
           `("ANTHROPIC_BASE_URL" ,base-url
             "ANTHROPIC_AUTH_TOKEN" ,auth-token
             "ANTHROPIC_DEFAULT_OPUS_MODEL" ,opus_model
             "ANTHROPIC_DEFAULT_SONNET_MODEL" ,sonnet_model
             "ANTHROPIC_DEFAULT_HAIKU_MODEL" ,haiku_model))))

(defun my-llm-update-agent-shell-env ()
  "刷新 agent-shell 的环境变量配置。"
  (when (boundp 'agent-shell-anthropic-claude-environment)
    (setq agent-shell-anthropic-claude-environment
          (my-llm-get-env-for-agent-shell))))

(use-package claude-code-ide
  :load-path "site-lisp/claude-code-ide"
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (setenv "CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC" "1")
  (setq claude-code-ide-terminal-backend 'ghostel)
  ;; 初始化 provider 环境变量 (不弹选择)
  (setenv "ANTHROPIC_BASE_URL" (plist-get my-llm-provider :base-url))
  (setenv "ANTHROPIC_AUTH_TOKEN" (my-llm--auth-token))
  (setenv "ANTHROPIC_DEFAULT_OPUS_MODEL" (plist-get my-llm-provider :opus-model))
  (setenv "ANTHROPIC_DEFAULT_SONNET_MODEL" (plist-get my-llm-provider :sonnet-model))
  (setenv "ANTHROPIC_DEFAULT_HAIKU_MODEL" (plist-get my-llm-provider :haiku-model))
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(use-package gemini-cli
  :load-path "site-lisp/gemini-cli"
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*gemini"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 90)))
  (setq gemini-cli-program "qwen")
  (setq gemini-cli-terminal-backend 'vterm)
  (setq gemini-cli-notification-function #'my-ai-code-notify)
  (gemini-cli-mode)
  :bind-keymap ("C-c g" . gemini-cli-command-map))

(use-package codex-ide
  :load-path "site-lisp/emacs-codex-ide"
  :bind ("C-c C-;" . codex-ide-menu))

(use-package agent-shell
  :bind (:map agent-shell-mode-map
         ("M-RET" . newline))
  :config
  (setq agent-shell-anthropic-claude-environment (my-llm-get-env-for-agent-shell)
        agent-shell-prefer-viewport-interaction t)
  (use-package agent-shell-macext
    :if sys/macp
    :load-path "site-lisp/agent-shell-macext"
    :hook (agent-shell-mode . agent-shell-macext-setup)
    :custom
    (agent-shell-macext-file-copy-policy 'auto)    ; auto, always-copy, always-original
    (agent-shell-macext-notifications t)           ; enable native notifications
    (agent-shell-macext-notify-current-buffer nil)) ; nil = suppress when shell/viewport is current and Emacs is focused

  (use-package agent-shell-pet
    :load-path "site-lisp/agent-shell-pet"
    :commands (global-agent-shell-pet-mode)
    :init (global-agent-shell-pet-mode 1)
    :config
    (setq agent-shell-pet-renderer (if sys/macp 'macos-native 'child-frame)
          agent-shell-pet-speech-bubble-theme 'light
          agent-shell-pet-size 'medium)))

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
