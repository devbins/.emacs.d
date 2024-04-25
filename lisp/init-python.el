;;; init-python.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-python.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:38:44 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 55
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

;; Python Mode
;; Install:
;;   pip install pyflakes
;;   pip install autoflake
(use-package python
  :ensure nil
  :bind(:map python-mode-map ("C-c c" . compile))
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil
        python-indent-guess-indent-offset-verbose nil
        python-indent-offset 4)
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
           (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  (evil-leader/set-key-for-mode 'python-mode
    "mva" 'pyenv-mode-set
    "mvc" 'pyenv-mode-unset
    "mrI" 'py-isort-buffer)

  ;; Live Coding in Python
  (use-package live-py-mode)

  ;; Format using YAPF
  ;; Install: pip install yapf
  (use-package yapfify
    :if (executable-find "yapf")
    :diminish yapf-mode
    :hook (python-mode . yapf-mode))

  ;; Google docstring style
  (use-package python-insert-docstring
    :bind (:map python-mode-map
           ("C-c i" . python-insert-docstring-with-google-style-at-point)))

  ;; sphinx doc style
  (use-package sphinx-doc
    :hook (python-mode . sphinx-doc-mode))

  ;; sort python import
  (use-package py-isort
    :hook (before-save . py-isort-before-save)))

(use-package conda
  :if (executable-find "conda")
  :config
  (setq conda-anaconda-home (expand-file-name "~/anaconda3/")
        conda-env-home-directory (expand-file-name "~/anaconda3/")
        conda-env-subdirectory "envs")

  (unless (getenv "CONDA_DEFAULT_ENV")
    (conda-env-activate "base")))

(use-package poetry)
(use-package ein
  :defer t
  :commands (ein:notebooklist-open ein:notebooklist-login ein:run ein:stop)
  :config
  (defun devbins/ein:worksheet-merge-cell-next ()
    (interactive)
    (ein:worksheet-merge-cell (ein:worksheet--get-ws-or-error) (ein:worksheet-get-current-cell) t t))

  ;; keybindings for ipython notebook traceback mode
  (evil-leader/set-key-for-mode 'ein:traceback-mode
    "RET" 'ein:tb-jump-to-source-at-point-command
    "mn" 'ein:tb-next-item
    "mp" 'ein:tb-prev-item
    "mq" 'bury-buffer)

  ;; keybindings mirror ipython web interface behavior
  (evil-define-key 'insert ein:notebook-multilang-mode-map
    (kbd "<C-return>") 'ein:worksheet-execute-cell
    (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next)

  ;; keybindings mirror ipython web interface behavior
  (evil-define-key 'hybrid ein:notebook-multilang-mode-map
    (kbd "<C-return>") 'ein:worksheet-execute-cell
    (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next)

  (evil-define-key 'normal ein:notebook-multilang-mode-map
    ;; keybindings mirror ipython web interface behavior
    (kbd "<C-return>") 'ein:worksheet-execute-cell
    (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next
    "gj" 'ein:worksheet-goto-next-input
    "gk" 'ein:worksheet-goto-prev-input))

(use-package jupyter)

(defun pyuic(file)
  "use pyuic convert .ui FILE to .py"
  (interactive "fchoose .ui extension file: ")
  (let* ((command (or (executable-find "pyuic6") (executable-find "pyuic5")))
         (output (file-name-sans-extension (expand-file-name file)))
         (ext (file-name-extension file)))
    (if command
        (if (string= ext "ui")
            (shell-command (format "%s %s -o %s.py" command file output))
          (message "not .ui file, require .ui type file"))
      (message "can not found pyuic, please install PyQt6-tools"))))

(defun pyuic-current-buffer()
  "convert current .ui file to .py"
  (interactive)
  (pyuic (buffer-file-name)))

(defun uic(file)
  "use uic convert .ui FILE to .h"
  (interactive "fchoose .ui extension file: ")
  (let* ((command (executable-find "uic"))
         (output (file-name-sans-extension (expand-file-name file)))
         (ext (file-name-extension file)))
    (if command
        (if (string= ext "ui")
            (shell-command (format "%s %s -o %s.h" command file output))
          (message "not .ui file, require .ui type file"))
      (message "can not found pyuic, please install Qt"))))

(defun uic-current-buffer()
  "convert current .ui file to .h"
  (interactive)
  (uic (buffer-file-name)))

(provide 'init-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
