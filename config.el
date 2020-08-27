;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jon Irving"
      user-mail-address "j@lollyshouse.ca")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(if (eql system-type 'darwin)
    (setq doom-font (font-spec :family "Fira Code Retina" :size 13 :weight 'semi-light)
          doom-variable-pitch-font (font-spec :family "Lucida Grande" :size 13)
          ns-right-option-modifier 'meta
          mac-command-modifier 'meta)
  (setq doom-font (font-spec :family "Fira Code" :size 21 :weight 'semi-light)
        doom-variable-pitch-font (font-spec :family "sans" :size 21)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(load! "draculapro-theme.el")
;;(load! "doom-draculapro-theme.el")
;;(setq doom-theme 'doom-wilmersdorf)
;;(setq doom-theme 'doom-sourcerer)
(setq doom-theme 'doom-outrun-electric)
;;(setq doom-theme 'doom-rouge)
;;(setq doom-draculapro-brighter-modeline t)
;;(setq doom-theme 'doom-draculapro)
;;(setq doom-theme 'draculapro)
;;(setq doom-theme nil)
(setq doom-theme 'dracula)

(custom-theme-set-faces! 'dracula
  '(mode-line :background "#373844" :foreground "#f8f8f2")
  '(mode-line-inactive :background "#282a36" :foreground "#ccccc7"))

;; (after! doom-modeline
;;   (nyan-mode 1))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/OrgMode")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq doom-modeline-icon nil)
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

;; (setq doom-modeline-minor-modes t)
(setq confirm-kill-emacs nil)

(setq treemacs-no-png-images nil)
(setq treemacs-is-never-other-window t)

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)
(setq company-idle-delay nil)

(setq lsp-ui-sideline-enable nil)
(setq lsp-enable-symbol-highlighting nil)

;; whitespace
(setq whitespace-line-column 100)
(setq whitespace-style '(face trailing lines-tail tabs))
(add-hook 'prog-mode-hook #'whitespace-mode)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq indent-guide-char "|")
(setq indent-guide-recursive nil)

;; I mean, _seriously_...
(setq sentence-end-double-space nil)

(progn
  (setq display-time-format nil)
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date nil)
  (setq display-time-interval 30)
  (setq display-time-default-load-average nil)
  (setq zoneinfo-style-world-list
        '(("America/Los_Angeles" "San Francisco")
          ("America/New_York" "Toronto")
          ("Europe/London" "London")
          ("Europe/Berlin" "Berlin")
          ("Asia/Hong_Kong" "Hong Kong")
          ("Asia/Tokyo" "Tokyo")))
  (display-time-mode))

;; (after! treemacs
;;   (delq! 'treemacs-mode aw-ignored-buffers))
(fringe-mode nil)

(map! "C-\\" #'company-complete-common-or-cycle)

(setq deft-directory (concat (getenv "HOME") "/Dropbox/OrgMode"))

(map! (:when (featurep! :lang clojure)
       (:map cider-repl-mode-map
        :i "RET" #'cider-repl-newline-and-indent
        :i "C-RET" #'cider-repl-return)))

(setq +format-on-save-enabled-modes '(python-mode))

(defvar my-lisp-modes
  '(emacs-lisp-mode clojure-mode scheme-mode geiser-mode racket-mode lisp-mode))

(defun add-hooks (modes func)
  (dolist (mode modes)
    (add-hook (intern (concat (symbol-name mode) "-hook")) func)))

(add-hooks my-lisp-modes #'paredit-mode)
(add-hooks my-lisp-modes #'evil-paredit-mode)
(add-hooks my-lisp-modes #'indent-guide-mode)

(after! ivy
  ;; I prefer search matching to be ordered; it's more precise
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))

;; Some org-mode setup
(after! org
  ;; Set agenda file(s)
  (setq org-agenda-files (list (concat org-directory "/journal.org")
                               (concat org-directory "/berlin.org")
                               (concat org-directory "/shrieks.org")))

  (setq org-agenda-span 14)
  ;; (setq org-agenda-start-on-weekday nil)

  ;; prevent org-mode hijacking arrow keys
  ;;(setq org-replace-disputed-keys t)

  ;; set our own todo keywords
  (setq org-todo-keywords
        '((sequence "TODO" "WAITING" "QUEUED" "PAUSED" "|" "DONE" "ABANDONED")))

  ;; switch quickly
  (setq org-use-fast-todo-selection t)
  (setq org-priority-default ?C)

  ;; extra indentation
  (setq org-adapt-indentation t)

  ;; Use cider as the clojure execution backend
  ;; (setq org-babel-clojure-backend 'cider)

  ;; Let's have pretty source code blocks
  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil)

  ;; org-capture
  (require 'org-datetree)

  (setq org-default-notes-file (concat org-directory "/berlin.org"))
  (setq org-capture-templates
        `(("j" "Journal" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
           "* %T\n  %?\n\n%a")
          ("s" "Shriek" entry (file+headline ,(concat org-directory "/shrieks.org") "Shrieks")
           "* %T\n%?\n")
          ("t" "Task" entry (file+headline ,(concat org-directory "/berlin.org") "Inbox")
           "* TODO %?\n  %a\n%i")))

  (add-hook 'org-mode-hook 'auto-fill-mode)
  (add-hook 'org-capture-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook
               'before-save-hook 'org-update-all-dblocks nil 'local-only)))

  (org-clock-persistence-insinuate)

  (dolist (tag '(home xapix sanity rachel lauren alice grace family self))'
    (add-to-list 'org-tag-persistent-alist tag)))
