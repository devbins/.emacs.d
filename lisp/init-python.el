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
;;     Update #: 16
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
;;   pip install autopep8
(use-package python
  :ensure nil
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
    :diminish yapf-mode
    :hook (python-mode . yapf-mode)))

(use-package conda
  :config
  (setq conda-anaconda-home (expand-file-name "/usr/local/anaconda3/")
        conda-env-home-directory (expand-file-name "/usr/local/anaconda3/")
        conda-env-subdirectory "envs"))

(unless (getenv "CONDA_DEFAULT_ENV")
  (conda-env-activate "base"))

(use-package ein
  :defer t
  :commands (ein:notebooklist-open ein:notebooklist-login ein:run ein:stop)
  :config
  (progn
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
      "gk" 'ein:worksheet-goto-prev-input)))

(defun pyuic()
  "use pyuic convert .ui file to .py"
  (interactive)
  (let ((name (file-relative-name (buffer-file-name))))
    (shell-command
     (format "%s %s -o %s.py" (or (executable-find "pyuic6") (executable-find "pyuic5")) name (file-name-sans-extension name)))))

(defun uic()
  "use uic convert .ui file to .h"
  (interactive)
  (if (executable-find "uic")
      (let ((name (file-relative-name (buffer-file-name))))
        (shell-command
         (format "uic %s -o %s.h" name (file-name-sans-extension name))))
    (message "can not find uic")))

(provide 'init-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
