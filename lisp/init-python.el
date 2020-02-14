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
;;     Update #: 1
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
  (setq python-shell-completion-native-enable nil)
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

(use-package ein
  :defer t
  :commands (ein:notebooklist-open ein:notebooklist-login ein:run ein:stop)
  :config
  (progn
    (defun devbins/ein:worksheet-merge-cell-next ()
      (interactive)
      (ein:worksheet-merge-cell (ein:worksheet--get-ws-or-error) (ein:worksheet-get-current-cell) t t))

    (evil-leader/set-key-for-mode 'ein:notebook-multilang-mode
      "my" 'ein:worksheet-copy-cell
      "mp" 'ein:worksheet-yank-cell
      "md" 'ein:worksheet-kill-cell
      "mh" 'ein:notebook-worksheet-open-prev-or-last
      "mi" 'ein:worksheet-insert-cell-below
      "mI" 'ein:worksheet-insert-cell-above
      "mj" 'ein:worksheet-goto-next-input
      "mk" 'ein:worksheet-goto-prev-input
      "ml" 'ein:notebook-worksheet-open-next-or-first
      "mH" 'ein:notebook-worksheet-move-prev
      "mJ" 'ein:worksheet-move-cell-down
      "mK" 'ein:worksheet-move-cell-up
      "mL" 'ein:notebook-worksheet-move-next
      "mt" 'ein:worksheet-toggle-output
      "mR" 'ein:worksheet-rename-sheet
      "RET" 'ein:worksheet-execute-cell-and-goto-next
      ;; Output
      "C-l" 'ein:worksheet-clear-output
      "C-S-l" 'ein:worksheet-clear-all-output
      ;;Console
      "C-o" 'ein:console-open
      ;; Merge cells
      "C-k" 'ein:worksheet-merge-cell
      "C-j" 'devbins/ein:worksheet-merge-cell-next
      "s" 'ein:worksheet-split-cell-at-point
      ;; Notebook
      "C-s" 'ein:notebook-save-notebook-command
      "C-r" 'ein:notebook-rename-command
      "m1" 'ein:notebook-worksheet-open-1th
      "m2" 'ein:notebook-worksheet-open-2th
      "m3" 'ein:notebook-worksheet-open-3th
      "m4" 'ein:notebook-worksheet-open-4th
      "m5" 'ein:notebook-worksheet-open-5th
      "m6" 'ein:notebook-worksheet-open-6th
      "m7" 'ein:notebook-worksheet-open-7th
      "m8" 'ein:notebook-worksheet-open-8th
      "m9" 'ein:notebook-worksheet-open-last
      "m+" 'ein:notebook-worksheet-insert-next
      "m-" 'ein:notebook-worksheet-delete
      "mx" 'ein:notebook-close
      "mu" 'ein:worksheet-change-cell-type
      "mfs" 'ein:notebook-save-notebook-command)

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
      "gk" 'ein:worksheet-goto-prev-input)

    ;; if this is not required then the following keygindings fail
    (require 'ein-multilang)
    (define-key ein:notebook-multilang-mode-map (kbd "M-j") 'ein:worksheet-move-cell-down)
    (define-key ein:notebook-multilang-mode-map (kbd "M-k") 'ein:worksheet-move-cell-up)))

(defun pyuic()
  "use pyuic5 convert .ui file to .py"
  (interactive)
  (let ((name (file-relative-name (buffer-file-name))))
    (shell-command
     (format "pyuic5 %s -o %s.py" name (file-name-sans-extension name)))))

(provide 'init-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
