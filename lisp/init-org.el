;;; init-org.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-org.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:32:07 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 14
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

(use-package org
  :ensure nil
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :preface
  (defun hot-expand (str &optional mod)
    "Expand org template.
STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org")
    :color blue :quit-key "q")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         :map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))
  :hook ((org-mode . (lambda ()
                       "Beautify org symbols."
                       (push '("[ ]" . ?☐) prettify-symbols-alist)
                       (push '("[X]" . ?☑) prettify-symbols-alist)
                       (push '("[-]" . ?⛝) prettify-symbols-alist)

                       (push '("#+ARCHIVE:" . ?📦) prettify-symbols-alist)
                       (push '("#+AUTHOR:" . ?👤) prettify-symbols-alist)
                       (push '("#+CREATOR:" . ?💁) prettify-symbols-alist)
                       (push '("#+DATE:" . ?📆) prettify-symbols-alist)
                       (push '("#+DESCRIPTION:" . ?⸙) prettify-symbols-alist)
                       (push '("#+EMAIL:" . ?🖂) prettify-symbols-alist)
                       (push '("#+OPTIONS:" . ?⛭) prettify-symbols-alist)
                       (push '("#+SETUPFILE:" . ?⛮) prettify-symbols-alist)
                       (push '("#+TAGS:" . ?🏷) prettify-symbols-alist)
                       (push '("#+TITLE:" . ?🕮) prettify-symbols-alist)

                       (push '("#+BEGIN_SRC" . ?✎) prettify-symbols-alist)
                       (push '("#+END_SRC" . ?□) prettify-symbols-alist)
                       (push '("#+begin_src" . ?✎) prettify-symbols-alist)
                       (push '("#+end_src" . ?□) prettify-symbols-alist)

                       (push '("#+BEGIN_QUOTE" . ?») prettify-symbols-alist)
                       (push '("#+END_QUOTE" . ?«) prettify-symbols-alist)
                       (push '("#+begin_quote" . ?») prettify-symbols-alist)
                       (push '("#+end_quote" . ?«) prettify-symbols-alist)

                       (push '("#+HEADERS" . ?☰) prettify-symbols-alist)
                       (push '("#+RESULTS:" . ?💻) prettify-symbols-alist)

                       (prettify-symbols-mode 1)))
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; WORKAROUND: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))
  :init (setq org-agenda-files '("~/org")
              org-todo-keywords
              '((sequence "TODO(!)" "DOING(!)" "|" "DONE(!)" "ABORT(@/!)")
                (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
              org-todo-keyword-faces '(("TODO")
                                       ("DOING" . "yellow")
                                       ("DONE" . "green")
                                       ("ABORT" . "grey")
                                       ("❓" . warning))
              org-priority-faces '((?A . error)
                                   (?B . warning)
                                   (?C . success))

              ;; define the refile targets
              org-refile-targets '((org-agenda-files :maxlevel . 3))
              org-refile-use-outline-path 'file
              org-outline-path-complete-in-steps nil
              org-tags-column -80
              org-log-done 'notevery

              org-log-into-drawer t
              org-drawers (quote ("PROPERTIES" "LOGBOOK"))
              org-clock-into-drawer t
              org-clock-idle-time 30
              org-clock-out-when-done t
              org-clock-in-switch-to-state "DOING"
              org-columns-default-format "%50ITEM(Task) %8PRIORITY(Priority) %6TODO(Status) %6Effort(Effort){:} %8CLOCKSUM %16SCHEDULED %16DEADLINE"
              org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                            ("STYLE_ALL" . "habit")))


              org-agenda-dir "~/.org/.agenda/"
              org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir)
              org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir)
              org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir)
              org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir)
              org-agenda-file-punch (expand-file-name "punch.org" org-agenda-dir)
              org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir)
              org-agenda-files (list org-agenda-dir)

              org-capture-templates '(("i" "inbox" entry (file+headline org-agenda-file-inbox "inbox")
                                       "* %?\n  %i\n %U"
                                       :empty-lines 1)
                                      ("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
                                       "* TODO [#B] %?\n  %i\n"
                                       :empty-lines 1)
                                      ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
                                       "* %?\n  %i\n %U"
                                       :empty-lines 1)
                                      ("N" "notes" entry (file+headlie org-agenda-file-note "Browser notes")
                                       "* %U - %:annotation %^g\n\n  %?"
                                       :empty-lines 1 :kill-buffer t)
                                      ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
                                       "* TODO [#B] %?\n  %i\n %U"
                                       :empty-lines 1)
                                      ("B" "Protocol Bookmarks" entry (file+headline org-agenda-file-inbox "Bookmarks")
                                       "* %U - %:annotation"
                                       :immediate-finish t :kill-buffer t :empty-lines 1)
                                      ("s" "Code Snippet" entry
                                       (file org-agenda-file-code-snippet)
                                       "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
                                      ("w" "work" entry (file+headline org-agenda-file-gtd "work")
                                       "* TODO [#A] %?\n  %i\n %U"
                                       :empty-lines 1[[zsh:1: command not found: osascript]])
                                      ;; org-mac-chrome-get-frontmost-url org-mac-chrome-insert-frontmost-url
                                      ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
                                       "* TODO [#C] %?\n %(org-mac-chrome-get-frontmost-url)\n %i\n %U"
                                       :empty-lines 1)
                                      ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
                                       "* TODO [#C] %?\n  %i\n %a \n %U"
                                       :empty-lines 1)
                                      ("p" "punch" entry (file+olp+datetree org-agenda-file-punch)
                                       "* %^{想法}%? %U")
                                      ("j" "Journal Entry" entry (file+olp+datetree org-agenda-file-journal)
                                       "* %?"
                                       :empty-lines 1))

              ;; #+CAPTION: 设定图片宽度为100
              ;; #+ATTR_HTML: :width 100
              ;; file:data/2013/pict/test.png
              org-image-actual-width '(300)
              org-startup-with-inline-images t

              org-export-with-broken-links                  'mark
              org-export-with-sub-superscripts              '{}
              org-use-sub-superscripts                      '{}

              org-catch-invisible-edits 'smart

              ;; org-startup-indented t
              ;; org-startup-folded    'content
              org-startup-truncated nil
              org-ellipsis (if (char-displayable-p ?) "  " "  ▼")
              org-tag-alist (quote ((:startgroup)
                                    ("@errand" . ?e)
                                    ("@office" . ?o)
                                    ("@home" . ?H)
                                    ("@farm" . ?f)
                                    (:endgroup)
                                    ("WAITING" . ?w)
                                    ("HOLD" . ?h)
                                    ("READING" . ?r)
                                    ("PERSONAL" . ?P)
                                    ("WORK" . ?W)
                                    ("FARM" . ?F)
                                    ("ORG" . ?O)
                                    ("BLOG" . ?B)
                                    ("NORANG" . ?N)
                                    ("crypt" . ?E)
                                    ("NOTE" . ?n)
                                    ("CANCELLED" . ?c)
                                    ("FLAGGED" . ??)))
              org-pretty-entities nil
              org-hide-emphasis-markers t)
  :config
  ;; Add new template
  (add-to-list 'org-structure-template-alist '("n" . "note"))

  ;; Use embedded webkit browser if possible
  (when (featurep 'xwidget-internal)
    (push '("\\.\\(x?html?\\|pdf\\)\\'"
            .
            (lambda (file _link)
              (xwidget-webkit-browse-url (concat "file://" file))
              (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
                (when (buffer-live-p buf)
                  (and (eq buf (current-buffer)) (quit-window))
                  (pop-to-buffer buf)))))
          org-file-apps))

  ;; Add gfm/md backends
  (use-package ox-gfm)
  (use-package ox-hugo)
  (add-to-list 'org-export-backends 'md)

  (advice-add 'org-todo :after 'org-save-all-org-buffers)
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

  ;; Prettify UI
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :init (setq org-bullets-bullet-list '("◉" "○" "✸" "✿" "☯" "☭" "♥" "✜" "♠" "☢" "❀" "★")))

  (use-package org-fancy-priorities
    :diminish
    :hook (org-mode . org-fancy-priorities-mode)
    :init (setq org-fancy-priorities-list
                (if (display-graphic-p)
                    '("⚡" "⬆" "⬇" "☕")
                  '("HIGH" "MEDIUM" "LOW" "OPTIONAL"))))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  ;; ob-sh renamed to ob-shell since 26.1.
  (if emacs/>=26p
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-list))

  (use-package ob-ipython
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)
  ;; Org babel extensions
  ;; HTTP client
  ;; usage: BEGIN_SRC http :pretty
  (use-package ob-http
    :init (cl-pushnew '(http . t) load-language-list))

  ;; Async src_block execution
  ;; usage: begin_src sh :async
  (use-package ob-async
    :init (cl-pushnew '(async . t) load-language-list))


  (use-package ob-kotlin
    :init (cl-pushnew '(kotlin . t) load-language-list))

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
           ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Export text/html MIME emails
  (use-package org-mime
    :bind (:map message-mode-map
           ("C-c M-o" . org-mime-htmlize)
           :map org-mode-map
           ("C-c M-o" . org-mime-org-buffer-htmlize)))


  (use-package htmlize)

  (use-package org-ref
    :after org)

  ;; Preview
  (use-package org-preview-html
    :diminish)

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
           ("C-<f7>" . org-tree-slide-mode)
           :map org-tree-slide-mode-map
           ("<left>" . org-tree-slide-move-previous-tree)
           ("<right>" . org-tree-slide-move-next-tree)
           ("S-SPC" . org-tree-slide-move-previous-tree)
           ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :config
    (org-tree-slide-simple-profile)
    (setq org-tree-slide-skip-outline-level 2))


  (evil-leader/set-key-for-mode 'org-mode
    "mou" 'org-update-all-dblocks
    "moT" 'org-set-tags
    "mov" 'org-columns
    "moq" 'org-columns-quit

    "'" 'org-edit-special
    "mc" 'org-capture

    ;; Clock
    ;; These keybindings should match those under the "aoC" prefix (below)
    "mCc" 'org-clock-cancel
    "mCd" 'org-clock-display
    "mCe" 'org-evaluate-time-range
    "mCg" 'org-clock-goto
    "mCi" 'org-clock-in
    "mCI" 'org-clock-in-last
    "mCo" 'org-clock-out
    "mCp" 'org-pomodoro
    "mCR" 'org-clock-report
    "mCr" 'org-resolve-clocks

    "mdd" 'org-deadline
    "mds" 'org-schedule
    "mdt" 'org-time-stamp
    "mdT" 'org-time-stamp-inactive
    "mee" 'org-export-dispatch
    "mfi" 'org-feed-goto-inbox
    "mfu" 'org-feed-update-all

    "ma" 'org-agenda

    "mp" 'org-priority

    "mTc" 'org-toggle-checkbox
    "mTe" 'org-toggle-pretty-entities
    "mTi" 'org-toggle-inline-images
    "mTl" 'org-toggle-link-display
    "mTt" 'org-show-todo-tree
    "mTT" 'org-todo
    "mTx" 'org-toggle-latex-fragment

    ;; More cycling options (timestamps, headlines, items, properties)
    "mL" 'org-shiftright
    "mH" 'org-shiftleft
    "mJ" 'org-shiftdown
    "mK" 'org-shiftup

    ;; Change between TODO sets
    "mC-S-l" 'org-shiftcontrolright
    "mC-S-h" 'org-shiftcontrolleft
    "mC-S-j" 'org-shiftcontroldown
    "mC-S-k" 'org-shiftcontrolup

    ;; Subtree editing
    "msa" 'org-toggle-archive-tag
    "msA" 'org-archive-subtree
    "msb" 'org-tree-to-indirect-buffer
    "msd" 'org-cut-subtree
    "msh" 'org-promote-subtree
    "msj" 'org-move-subtree-down
    "msk" 'org-move-subtree-up
    "msl" 'org-demote-subtree
    "msn" 'org-narrow-to-subtree
    "msN" 'widen
    "msr" 'org-refile
    "mss" 'org-sparse-tree
    "msS" 'org-sort

    ;; tables
    "mta" 'org-table-align
    "mtb" 'org-table-blank-field
    "mtc" 'org-table-convert
    "mtdc" 'org-table-delete-column
    "mtdr" 'org-table-kill-row
    "mte" 'org-table-eval-formula
    "mtE" 'org-table-export
    "mth" 'org-table-previous-field
    "mtH" 'org-table-move-column-left
    "mtic" 'org-table-insert-column
    "mtih" 'org-table-insert-hline
    "mtiH" 'org-table-hline-and-move
    "mtir" 'org-table-insert-row
    "mtI" 'org-table-import
    "mtj" 'org-table-next-row
    "mtJ" 'org-table-move-row-down
    "mtK" 'org-table-move-row-up
    "mtl" 'org-table-next-field
    "mtL" 'org-table-move-column-right
    "mtn" 'org-table-create
    "mtN" 'org-table-create-with-table.el
    "mtr" 'org-table-recalculate
    "mts" 'org-table-sort-lines
    "mttf" 'org-table-toggle-formula-debugger
    "mtto" 'org-table-toggle-coordinate-overlays
    "mtw" 'org-table-wrap-region

    ;; Source blocks / org-babel
    "mbp"     'org-babel-previous-src-block
    "mbn"     'org-babel-next-src-block
    "mbe"     'org-babel-execute-maybe
    "mbo"     'org-babel-open-src-block-result
    "mbv"     'org-babel-expand-src-block
    "mbu"     'org-babel-goto-src-block-head
    "mbg"     'org-babel-goto-named-src-block
    "mbr"     'org-babel-goto-named-result
    "mbb"     'org-babel-execute-buffer
    "mbs"     'org-babel-execute-subtree
    "mbd"     'org-babel-demarcate-block
    "mbt"     'org-babel-tangle
    "mbf"     'org-babel-tangle-file
    "mbc"     'org-babel-check-src-block
    "mbj"     'org-babel-insert-header-arg
    "mbl"     'org-babel-load-in-session
    "mbi"     'org-babel-lob-ingest
    "mbI"     'org-babel-view-src-block-info
    "mbz"     'org-babel-switch-to-session
    "mbZ"     'org-babel-switch-to-session-with-code
    "mba"     'org-babel-sha1-hash
    "mbx"     'org-babel-do-key-sequence-in-edit-buffer
    ;; Multi-purpose keys
    "m*" 'org-ctrl-c-star
    "m-" 'org-ctrl-c-minus
    "m#" 'org-update-statistics-cookies
    "mRET"   'org-ctrl-c-ret
    "mM-RET" 'org-meta-return
    ;; attachments
    "mA" 'org-attach
    ;; insertion
    "mib" 'org-insert-structure-template
    "mid" 'org-insert-drawer
    "mie" 'org-set-effort
    "mif" 'org-footnote-new
    "mih" 'org-insert-heading
    "miH" 'org-insert-heading-after-current
    "mii" 'org-insert-item
    "mil" 'org-insert-link
    "min" 'org-add-note
    "mip" 'org-set-property
    "mis" 'org-insert-subheading
    "mit" 'org-set-tags-command)

  (evil-leader/set-key-for-mode 'org-src-mode
    "c" 'org-edit-src-exit
    "k" 'org-edit-src-abort)

  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)

  ;; Pomodoro
  (use-package org-pomodoro
    :preface
    ;; brew install terminal-notifier
    ;; brew linkapps
    (defun notify-osx (title msg)
      (call-process "terminal-notifier"
                    nil 0 nil
                    "-group" "Emacs"
                    "-title" title
                    "-sender" "org.gnu.Emacs"
                    "-message" msg
                    "-active" "org.gnu.Emacs"))

    (defun notify-linux (title msg)
      (call-process "notify-send"
                    nil 0 nil
                    "-i" "face-monkey"
                    title
                    msg))


    (defun pomodoro-notify (title msg)
      (if (eq system-type 'darwin)
          (notify-osx title msg)
        (notify-linux title msg)))
    :bind (:map org-agenda-mode-map
           ("P" . org-pomodoro))
    :hook ((org-pomodoro-started . (lambda ()(when sys/macp (do-applescript "tell application \"JustFocus\"\n    launch\n    start pomodoro\nend tell"))))
           (org-pomodoro-finished . (lambda () (pomodoro-notify "Pomodoro Completed!" "Time for a break.")))
           (org-pomodoro-break-finished . (lambda () (pomodoro-notify "Pomodoro Short Break Finished" "Ready for Another?")))
           (org-pomodoro-long-break-finished . (lambda () (pomodoro-notify "Pomodoro Long Break Finished" "Ready for Another?")))
           (org-pomodoro-killed . (lambda () (progn (pomodoro-notify "Pomodoro Killed" "One does not simply kill a pomodoro!")
                                               (when sys/macp (do-applescript "tell application \"JustFocus\"\n    stop\nend tell")))))))

  (use-package org-download
    :commands (org-download-enable
               org-download-yank
               org-download-screenshot)
    :hook (org-mode . org-download-enable))

  ;; 加密文章
  ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
  (use-package org-crypt
    :ensure nil
    :demand
    :init
    ;; 设置要加密的 tag 標籤為 secret
    (setq org-crypt-tag-matcher "crypt"
          ;; 避免 secret 这个 tag 被子項目继承造成重复加密
          org-tags-exclude-from-inheritance (quote ("crypt"))
          ;; 设置用於加密的 GPG ID 设置为 nil 使用对称加密 (symmetric encryption)
          org-crypt-key nil)
    :config
    ;; 当被加密的部份被保存时，自動加密回去
    (org-crypt-use-before-save-magic))


  ;; pinentry-start 要使用的时候 Mac 下需要
  ;; 把allow-emacs-pinentry 加入 .gnupg/gpg-agent.conf
  ;; http://elpa.gnu.org/packages/pinentry.html
  ;; This will force Emacs to use its own internal password prompt instead of an external pin entry program.
  (use-package pinentry
    :defer t
    :if sys/macp
    :hook(after-init . pinentry-start))

  (use-package orgit)

  (use-package evil-org
    :defer t
    :after org
    :diminish
    :init
    (setq evil-org-use-additional-insert t
          evil-org-key-theme `(textobjects
                               navigation
                               additional
                               ,@(when t '(todo))))
    :hook (org-mode . (lambda ()
                        (evil-org-mode)
                        (evil-normalize-keymaps))))

  (use-package org-agenda
    :ensure nil
    :defer t
    :preface
    (defun org-agenda-time-grid-spacing ()
      "Set different line spacing w.r.t. time duration."
      (save-excursion
        (let* ((background (alist-get 'background-mode (frame-parameters)))
               (background-dark-p (string= background "dark"))
               (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
               pos
               duration)
          (nconc colors colors)
          (goto-char (point-min))
          (while (setq pos (next-single-property-change (point) 'duration))
            (goto-char pos)
            (when (and (not (equal pos (point-at-eol)))
                       (setq duration (org-get-at-bol 'duration)))
              (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                    (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
                (overlay-put ov 'face `(:background ,(car colors)
                                        :foreground
                                        ,(if background-dark-p "black" "white")))
                (setq colors (cdr colors))
                (overlay-put ov 'line-height line-height)
                (overlay-put ov 'line-spacing (1- line-height))))))))
    :hook (org-agenda-finalize . org-agenda-time-grid-spacing)
    :init
    (setq org-agenda-restore-windows-after-quit t
          org-agenda-window-setup 'current-window
          org-agenda-inhibit-startup t   ;; ~50x speedup
          org-agenda-use-tag-inheritance nil ;; 3-4x speedup
          ;;https://sachachua.com/blog/2015/06/adding-calculations-based-on-time-to-the-org-agenda-clock-report/
          org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))

          org-agenda-include-diary t

          ;; org-agenda-log-mode-items (quote (closed state))
          org-agenda-compact-blocks t
          org-agenda-sticky t
          org-agenda-span 'day
          org-agenda-sorting-strategy '((agenda habit-down time-up user-defined-up effort-up priority-down)
                                        (todo priority-down category-keep)
                                        (tags priority-down category-keep)))
    :config
    (require 'org-habit)
    (setq org-habit-show-done-always-green t
          ;; 减少显示天数，使其可以放在任务条的左边
          org-habit-graph-column 1
          org-habit-preceding-days 10
          org-habit-following-days 2
          ;; 恢复默认日历行为
          org-habit-show-habits-only-for-today nil)

    (require 'org-projectile)
    (mapcar #'(lambda (file)
                (when (file-exists-p file)
                  (push file org-agenda-files)))
            (org-projectile-todo-files))
    (evil-define-key 'normal org-agenda-mode-map
      (kbd "RET") 'org-agenda-switch-to
      "c" 'org-agenda-goto-calendar
      "d" 'org-agenda-day-view
      "e" 'org-agenda-set-effort
      "gd" 'org-agenda-toggle-time-grid
      "gh" 'org-agenda-holidays
      "gj" 'org-agenda-goto-date
      "gJ" 'org-agenda-clock-goto
      "gk" 'org-agenda-action
      "gm" 'org-agenda-bulk-mark
      "go" 'org-agenda-open-link
      "gO" 'delete-other-windows
      "gr" 'org-agenda-redo
      "gv" 'org-agenda-view-mode-dispatch
      "gw" 'org-agenda-week-view
      "g/" 'org-agenda-filter-by-tag
      "t" 'org-agenda-todo
      "h" 'org-agenda-earlier
      "i" 'org-agenda-diary-entry
      "j" 'org-agenda-next-line
      "k" 'org-agenda-previous-line
      "l" 'org-agenda-later
      "L" 'org-agenda-log-mode
      "R" 'org-agenda-clockreport-mode
      "M-j" 'org-agenda-next-item
      "M-k" 'org-agenda-previous-item
      "M-h" 'org-agenda-earlier
      "M-l" 'org-agenda-later
      "C-h" nil
      "q" 'org-agenda-quit)
    (evil-leader/set-key-for-mode 'org-agenda-mode
      "ma" 'org-agenda
      "mCc" 'org-agenda-clock-cancel
      "mCi" 'org-agenda-clock-in
      "mCo" 'org-agenda-clock-out
      "mCp" 'org-pomodoro
      "mdd" 'org-agenda-deadline
      "mds" 'org-agenda-schedule
      "mie" 'org-agenda-set-effort
      "mit" 'org-agenda-set-tags
      "msr" 'org-agenda-refile)))


(use-package deft
  :defer t
  :init (setq deft-directory "~/Nextcloud/"
              deft-extensions '("md" "org" "txt")
              deft-recursive t
              deft-use-filename-as-title t))

(defun org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-directory (buffer-file-name))
                  "imgs/" (format-time-string "%Y%m%d_%H%M%S_"))) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  ;; take screenshot
  (if (eq system-type 'darwin)
      (progn
        (call-process-shell-command "screencapture" nil nil nil nil " -s " (concat "\"" filename "\"" ))
        (call-process-shell-command "convert" nil nil nil nil (concat "\"" filename "\" -resize  \"50%\"" ) (concat "\"" filename "\"" ))))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
  ;; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (concat "[[file:" filename "]]")))
  (org-display-inline-images))

;; https://github.com/jjasghar/alfred-org-capture
(defun make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 16)
                (top . 400) (left . 300)
                (font . "Source Code Pro")
                ))
  (select-frame-by-name "remember")
  (org-capture))

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
