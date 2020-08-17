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
          doom-variable-pitch-font (font-spec :family "Lucida Grande" :size 13))
  (setq doom-font (font-spec :family "Fira Code" :size 13 :weight 'semi-light)
        doom-variable-pitch-font (font-spec :family "sans" :size 13)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(load! "draculapro-theme.el")
;(setq doom-theme 'doom-wilmersdorf)
(setq doom-theme 'doom-sourcerer)
(setq doom-theme 'doom-outrun-electric)
;(setq doom-theme 'doom-rouge)
;(setq doom-theme 'doom-dracula)
;(setq doom-theme 'dracula)

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
;; (setq doom-modeline-minor-modes t)
(setq confirm-kill-emacs nil)

(setq treemacs-no-png-images nil)
(setq treemacs-is-never-other-window t)

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)
(setq company-idle-delay nil)

(setq lsp-ui-sideline-enable nil)
(setq lsp-enable-symbol-highlighting nil)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; (after! treemacs
;;   (delq! 'treemacs-mode aw-ignored-buffers))

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

(after! ivy
  (setq ivy-extra-directories '("../"))
  ;; I prefer search matching to be ordered; it's more precise
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))
