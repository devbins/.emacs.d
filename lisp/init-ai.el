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
;;     Update #: 84
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
  (setq gptel-model "qwen2:latest"
        gptel-backend (gptel-make-ollama "Ollama" :host "localhost:11434" :models '("qwen2:latest" "llama2-chinese:13b" "codestral:latest" "dolphin-mixtral:8x7b-v2.7-q3_K_M") :stream t)
        gptel-default-mode 'org-mode
        gptel-prompt-prefix-alist '((markdown-mode . "## ")
                                    (org-mode . "** ")
                                    (text-mode . "## ")))
  (gptel-make-openai "kimi"
    :key (password-store-get "kimi")
    :models '("moonshot-v1-8k"
              "moonshot-v1-32k"
              "moonshot-v1-128k")
    :host "api.moonshot.cn"
    :stream t)
  (gptel-make-gemini
   "Gemini"
   :key (password-store-get "gemini")
   :stream t))

(use-package magit-gptcommit
  :hook (after-init . magit-gptcommit-status-buffer-setup)
  :config
  (setq magit-gptcommit-llm-provider (make-llm-ollama :chat-model "codestral:latest" :embedding-model "codestral:latest"))
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept)))

(use-package ellama
  :init
  (setopt ellama-language "Chinese"
                ellama-auto-scroll t)
  (require 'llm-ollama)
  :config
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "llama2-chinese:13b" :embedding-model "llama2-chinese:13b"))
  (setopt ellama-providers
		  '(("codestral" . (make-llm-ollama :chat-model "codestral:latest" :embedding-model "codestral:latest"))
			("dolphin-mixtral" . (make-llm-ollama :chat-model "dolphin-mixtral:8x7b-v2.7-q3_K_M" :embedding-model "dolphin-mixtral:8x7b-v2.7-q3_K_M")))))

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
