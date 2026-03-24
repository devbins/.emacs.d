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
;;     Update #: 395
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
                                  (gemma3:12b :description "the Gemma 3 models are multimodal—processing text and images—and feature a 128K context window with support for over 140 languages. "
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
    :models '((Qwen3.5-27B-Claude-4.6-Opus-Distilled-MLX-4bit :capabilities (media tool json))))

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
    :models '(z-ai/glm5 minimaxai/minimax-m2.5 qwen/qwen3.5-397b-a17b))

  (gptel-make-openai "modelscope"
    :key (auth-source-pass-get 'secret "modelscope")
    :models '(Qwen/Qwen3.5-397B-A17B
              Qwen/Qwen3-Next-80B-A3B-Instruct
              Qwen/Qwen3-Coder-480B-A35B-Instruct
              ZhipuAI/GLM-5
              MiniMax/MiniMax-M2.5
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
    :models  '(mimo-v2-flash)
    :stream t)

  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key (auth-source-pass-get 'secret "openrouter")
    :models '(qwen/qwen3-32b:free
              qwen/qwen3-235b-a22b:free
              qwen/qwen3-coder:free
              openai/gpt-oss-20b:free
              deepseek/deepseek-chat-v3-0324:free
              deepseek/deepseek-r1-0528:free
              deepseek/deepseek-r1-0528-qwen3-8b:free
              z-ai/glm-4.5-air:free
              cognitivecomputations/dolphin-mistral-24b-venice-edition:free
              tngtech/deepseek-r1t2-chimera:free
              moonshotai/kimi-k2:free
              moonshotai/kimi-dev-72b:free
              moonshotai/kimi-vl-a3b-thinking:free
              tencent/hunyuan-a13b-instruct:free
              minimax/minimax-m2:free
              mistralai/devstral-small:free
              mistralai/mistral-small-3.2-24b-instruct:free
              google/gemini-2.0-flash-exp:free
              google/gemma-3-27b-it:free))

  (gptel-make-openai "siliconflow"
    :host "api.siliconflow.cn"
    :key (auth-source-pass-get 'secret "siliconflow")
    :models '(deepseek-ai/DeepSeek-R1) :stream t)

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
  (setq magit-gptcommit-llm-provider (make-llm-ollama :chat-model "gemma3:12b" :embedding-model "bge-m3:latest"))
  (magit-gptcommit-status-buffer-setup))


(defun my-ai-code-notify (title message)
  "Display a macOS notification with sound."
  (call-process "osascript" nil nil nil
                "-e" (format "display notification \"%s\" with title \"%s\" sound name \"Glass\""
                             message title)))

;; Anthropic API 提供商配置
(defvar my-anthropic-providers
  '((anyrouter . (:base-url "https://pmpjfbhq.cn-nb1.rainapp.top"
                  :auth-key "anyrouter"))
    (mimo . (:base-url "https://api.xiaomimimo.com/anthropic"
             :auth-key "mimo"
             :models (:opus "mimo-v2-flash"
                      :sonnet "mimo-v2-flash"
                      :haiku "mimo-v2-flash")))
    (local . (:base-url "http://127.0.0.1:3456"
              :auth-key nil
              :auth-token "test")))
  "Anthropic API 提供商配置列表。
每个提供商包含:
  :base-url   - API 基础 URL
  :auth-key   - auth-source-pass 中的密钥名称
  :auth-token - 直接指定的 token (优先级低于 auth-key)
  :models     - 可选的模型配置 (:opus :sonnet :haiku)")

(defvar my-anthropic-current-provider 'anyrouter
  "当前使用的 Anthropic API 提供商。")

(defun my-anthropic-switch-provider (provider)
  "切换 Anthropic API 提供商。"
  (interactive
   (list (intern (completing-read "Select provider: "
                                  (mapcar #'car my-anthropic-providers)
                                  nil t))))
  (let* ((config (alist-get provider my-anthropic-providers))
         (base-url (plist-get config :base-url))
         (auth-key (plist-get config :auth-key))
         (auth-token (plist-get config :auth-token))
         (models (plist-get config :models)))
    (unless config
      (user-error "Unknown provider: %s" provider))
    (setenv "ANTHROPIC_BASE_URL" base-url)
    (setenv "ANTHROPIC_AUTH_TOKEN"
            (if auth-key
                (auth-source-pass-get 'secret auth-key)
              auth-token))
    ;; 清除或设置模型配置
    (if models
        (progn
          (when-let ((opus (plist-get models :opus)))
            (setenv "ANTHROPIC_DEFAULT_OPUS_MODEL" opus))
          (when-let ((sonnet (plist-get models :sonnet)))
            (setenv "ANTHROPIC_DEFAULT_SONNET_MODEL" sonnet))
          (when-let ((haiku (plist-get models :haiku)))
            (setenv "ANTHROPIC_DEFAULT_HAIKU_MODEL" haiku)))
      ;; 没有模型配置时清除环境变量
      (setenv "ANTHROPIC_DEFAULT_OPUS_MODEL" nil)
      (setenv "ANTHROPIC_DEFAULT_SONNET_MODEL" nil)
      (setenv "ANTHROPIC_DEFAULT_HAIKU_MODEL" nil))
    (setq my-anthropic-current-provider provider)
    (message "Switched to %s: %s" provider base-url)))

(defun my-anthropic-get-env-for-agent-shell ()
  "获取当前提供商的环境变量配置，用于 agent-shell。"
  (let* ((config (alist-get my-anthropic-current-provider my-anthropic-providers))
         (base-url (plist-get config :base-url))
         (auth-key (plist-get config :auth-key))
         (auth-token (plist-get config :auth-token))
         (models (plist-get config :models)))
    (apply #'agent-shell-make-environment-variables
           `("ANTHROPIC_BASE_URL" ,base-url
             "ANTHROPIC_AUTH_TOKEN" ,(if auth-key
                                         (auth-source-pass-get 'secret auth-key)
                                       auth-token)
             ,@(when-let ((opus (plist-get models :opus)))
                 `("ANTHROPIC_MODEL" ,opus
                   "ANTHROPIC_SMALL_FAST_MODEL" ,opus))))))


(use-package claude-code-ide
  :load-path "site-lisp/claude-code-ide"
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (setenv "CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC" "1")
  ;; 使用默认提供商初始化
  (my-anthropic-switch-provider my-anthropic-current-provider)
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

(use-package agent-shell
  :config
  (setq agent-shell-anthropic-claude-environment
        (my-anthropic-get-env-for-agent-shell)))

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
