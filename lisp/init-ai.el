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
;;     Update #: 51
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
  (setq gptel-model "llama2-chinese:13b"
        gptel-backend (gptel-make-ollama "Ollama" :host "localhost:11434" :models '("llama2-chinese:13b" "codestral:latest" "dolphin-mixtral:8x7b-v2.7-q3_K_M") :stream t)
        gptel-default-mode 'org-mode)
  (gptel-make-gemini
   "Gemini"
   :key (password-store-get "gemini")
   :stream t))

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
