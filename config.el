;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq-default tab-always-indent t)

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


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(load! "draculapro-theme.el")
(load! "doom-draculapro-theme.el")
;;(setq doom-theme 'doom-wilmersdorf)
;;(setq doom-theme 'doom-sourcerer)
;;(setq doom-theme 'doom-outrun-electric)
;;(setq doom-theme 'doom-rouge)
;;(setq doom-draculapro-brighter-modeline t)
;;(setq doom-theme 'doom-draculapro)
;;(setq doom-theme nil)
;;(setq doom-theme 'dracula)

(setq modus-operandi-theme-bold-constructs t)
(setq modus-operandi-theme-mode-line nil)
(setq modus-operandi-theme-faint-syntax nil)
(setq modus-operandi-theme-fringes nil)
(setq modus-operandi-theme-scale-headings t)


(setq modus-vivendi-theme-bold-constructs nil)
(setq modus-vivendi-theme-mode-line nil)
(setq modus-vivendi-theme-faint-syntax nil)
(setq modus-vivendi-theme-fringes nil)
(setq modus-vivendi-theme-scale-headings t)

(custom-theme-set-faces! 'modus-operandi
  '(bold :weight semibold)
  '(indent-guide-face :foreground "#c0c0c0"))

(custom-theme-set-faces! 'modus-vivendi
  '(bold :weight semibold))

(custom-theme-set-faces! '(dracula draculapro doom-dracula-pro)
  '(bold :weight semibold)
  '(mode-line :background "#373844" :foreground "#f8f8f2")
  '(mode-line-inactive :background "#282a36" :foreground "#ccccc7"))

(setq-default indicate-buffer-boundaries 'left)
(setq-default truncate-lines nil)
(setq-default word-wrap nil)

;; for some reason org-mode won't let me type a newline when this is on.
;; https://github.com/hlissner/doom-emacs/issues/3172
(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

(cond
 (IS-LINUX
  (progn
    (setq doom-font (font-spec :family "Iosevka Comfy" :size 19 :weight 'semi-light)
          doom-variable-pitch-font (font-spec :family "sans" :size 21))
    (setq doom-theme 'modus-vivendi)
    ;; (setq fancy-splash-image "~/Dropbox/Home/Pictures/cccp.png")
    (setq fancy-splash-image nil)))
 (IS-MAC
  (progn
    (setq doom-font (font-spec :family "Iosevka Comfy" :size 17 :weight 'light)
         ;; (font-spec :family "Iosevka Comfy" :size 17 :weight 'light)
          line-spacing 0
          doom-variable-pitch-font (font-spec :family "Lucida Grande" :size 13)
          ns-right-option-modifier 'meta
          mac-command-modifier 'meta)
    (setq doom-theme 'modus-vivendi)
    ;;(setq doom-theme 'doom-draculapro)
    ;;(setq doom-theme 'draculapro)
    ;;(setq fancy-splash-image "~/Dropbox/Home/Pictures/cccp.png")
    (setq fancy-splash-image "~/Downloads/rebel.png")
    ;; (setq fancy-splash-image
    ;;       "~/Dropbox/Home/Pictures/Death_Star/000000-death-star-png/000000-death-star-512.png")
    )))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'before-save-hook #'tide-format-before-save)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/OrgMode")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Flycheck is mostly annoying, but only intolerable in Clojure.
(setq-default flycheck-disabled-checkers '(clojure))

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

(setq doom-modeline-height 1)

(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-icon nil)
(setq doom-modeline-buffer-file-name-style 'file-name)
(setq doom-modeline-vcs-max-length 20)
(setq doom-modeline-workspace-name nil)
(setq doom-modeline-persp-name t)
(setq doom-modeline-modal-icon nil)
(setq doom-modeline-irc t)

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

(setq +vc-gutter-default-style nil)

;; (fringe-mode nil)
;; (setq-default fring-mode nil)

(map! "C-\\" #'company-complete-common-or-cycle)

(setq deft-directory (concat (getenv "HOME") "/Dropbox/OrgMode"))

(map! (:leader
       (:prefix "t"
        :nv :desc "Toggle line truncation" "t" #'toggle-truncate-lines
        :nv :desc "Toggle olivetti-mode" "o" #'olivetti-mode
        :nv :desc "Toggle focus-mode" "d" #'focus-mode)))

(after! olivetti
  (setq olivetti-minimum-body-width 120))

(map! (:when (featurep! :lang clojure)
       (:map cider-repl-mode-map
        :i "RET" #'cider-repl-newline-and-indent
        :i "C-RET" #'cider-repl-return)))

(map! (:when (featurep! :lang kotlin)
       (:map kotlin-mode-map
        :localleader
        :nv "z" #'kotlin-repl
        :nv "c" #'kotlin-send-buffer
        :nv "r" #'kotlin-send-region
        :nv "x" #'kotlin-send-block-and-focus
        :nv "B" #'kotlin-send-buffer-and-focus)))

(after! circe
  (set-irc-server! "freenode"
                   `(:host "localhost"
                     :tls nil
                     :port 6777))
  (set-irc-server! "oftc"
                   `(:host "localhost"
                     :tls nil
                     :port 6778)))

(setq ido-use-virtual-buffers t)
(setq ido-ubiquitous-allow-on-functional-collection t)

(setq +format-on-save-enabled-modes '(python-mode rustic-mode))

(defvar my-lisp-modes
  '(emacs-lisp-mode clojure-mode scheme-mode geiser-mode racket-mode lisp-mode))

(defun add-hooks (modes func)
  (dolist (mode modes)
    (add-hook (intern (concat (symbol-name mode) "-hook")) func)))

(add-hooks my-lisp-modes #'paredit-mode)
(add-hooks my-lisp-modes #'evil-paredit-mode)
(add-hooks my-lisp-modes #'indent-guide-mode)

;; doom already includes diff-hl, but it switches it on in the margin rather
;; than fringe - so first switch it off
(diff-hl-margin-mode -1)
;; and add some hooks for it
;; (add-hook 'prog-mode-hook #'turn-on-diff-hl-mode)
;; (add-hook 'org-mode-hook #'turn-on-diff-hl-mode)
;; (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
;; (add-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode)

(use-package! highlight-symbol
  :hook ((prog-mode . highlight-symbol-mode)
         (dired-mode . highlight-symbol-mode)))

(use-package! auto-highlight-symbol
  :init
  (global-auto-highlight-symbol-mode t)
  :config
  (add-to-list 'ahs-modes 'clojure-mode)
  (add-to-list 'ahs-modes 'fennel-mode)
  (add-to-list 'ahs-modes 'typescript-mode))

(after! (:and magit diff-hl)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(after! flycheck
  (setq flycheck-indication-mode 'right-fringe)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

(after! git-gutter-fringe
  (setq-default fringes-outside-margins t))

(after! ivy
  ;; I prefer search matching to be ordered; it's more precise
  ;;(add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus))
  (setq ivy-extra-directories nil)
  (setq ivy-use-virtual-buffers t))

(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))
;; (after! fennel-mode
;;   (add-hook 'fennel-mode-hook
;;             (lambda ()
;;               (setq-local inferior-lisp-program
;;                           (if IS-MAC "/usr/local/bin/fennel --repl"
;;                             "fennel --repl")))))

(after! cider
  ;; (remove-hook 'clojure-mode-hook #'cider-mode)
  ;; (remove-hook 'clojure-mode-hook #'er/add-clojure-mode-expansions)
  ;; (remove-hook 'clojure-mode-hook #'clj-refactor-mode)
  (setq cider-use-fringe-indicators nil)
  (setq cider-prompt-for-symbol nil))

(after! inf-clojure
  (inf-clojure-update-feature 'clojure 'completion "(complete.core/completions \"%s\")"))

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
        '((sequence "TODO" "WAITING" "QUEUED" "INPROGRESS" "PAUSED" "|" "DONE" "ABANDONED")))

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

  (dolist (tag '(home xapix sanity rachel lauren alice grace family self))
    (add-to-list 'org-tag-persistent-alist tag)))

(if IS-MAC
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e"))

(after! mu4e
  ;; use it everywhere to send mail
  (setq mail-user-agent 'mu4e-user-agent)

  ;; universal
  (setq mu4e-root-maildir "~/Maildir")

  ;; See http://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html
  ;; for the templates - doco copied into comments below:

  ;; Using mu4e with multiple email accounts is fairly easy. Although
  ;; variables such as user-mail-address, mu4e-sent-folder, message-*,
  ;; smtpmail-*, etc. typically only take one value, it is easy to
  ;; change their values using mu4e-compose-pre-hook. The setup
  ;; described here is one way of doing this (though certainly not the
  ;; only way).

  ;; This setup assumes that you have multiple mail accounts under
  ;; mu4e-maildir. As an example, we’ll use ~/Maildir/Account1 and
  ;; ~/Maildir/Account2, but the setup works just as well if
  ;; mu4e-maildir points to something else.

  ;; First, you need to make sure that all variables that you wish to
  ;; change based on user account are set to some initial value. So set
  ;; up your environment with e.g., your main account:

  ;; (setq mu4e-sent-folder "/Account1/Saved Items"
  ;;       mu4e-drafts-folder "/Account1/Drafts"
  ;;       user-mail-address "my.address@account1.tld"
  ;;       smtpmail-default-smtp-server "smtp.account1.tld"
  ;;       smtpmail-local-domain "account1.tld"
  ;;       smtpmail-smtp-server "smtp.account1.tld"
  ;;       smtpmail-stream-type starttls
  ;;       smtpmail-smtp-service 25)

  ;; default
  (setq mu4e-decryption-policy t
        mu4e-update-interval 300
        mu4e-index-update-in-background nil
        mu4e-get-mail-command "true"
        mu4e-hide-index-messages t
        mu4e-confirm-quit nil
        mu4e-use-fancy-chars nil ;; they actually look shit
        ;; mu4e-html2text-command "pandoc -f html -t plain"
        ;; mu4e-html2text-command "w3m -dump -T text/html"
        ;; mu4e-html2text-command "links -force-html -dump"
        ;; mu4e-html2text-command "html2text -utf8 -width 72"
        ;; mu4e-html2text-command 'mu4e-shr2text
        ;; shr-color-visible-luminance-min 70
        mu4e-headers-sort-direction 'ascending
        mu4e-headers-skip-duplicates t
        mu4e-change-filenames-when-moving t
        mu4e-headers-hide-predicate nil
        mu4e-headers-include-related nil
        mu4e-split-view 'single-window
        mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:mailing-list . 16)
                              (:from . 25)
                              (:thread-subject))
        mu4e-compose-complete-only-after "2012-01-01"
        mu4e-view-show-addresses t
        mu4e-date-format-long "%FT%T%z"
        mu4e-headers-date-format "%F"
        mu4e-headers-time-format "%T"
        mu4e-headers-long-date-format "%FT%T%z"
        ;; mu4e-view-use-gnus nil
        mm-inline-large-images 'resize)

  ;; (setq mu4e-html2text-command 'mu4e-shr2text)

  ;; something about ourselves
  (setq mu4e-personal-addresses '("j@lollyshouse.ca"
                                  "hi@mhcat.ca"
                                  "jonathan.irving@gmail.com"
                                  "jon@xapix.io"
                                  "j0ni@fastmail.com"
                                  "j0ni@protonmail.com"
                                  "jon@arity.ca"))

  (setq mu4e-bookmarks
        '(("date:7d..now AND (maildir:/Fastmail/INBOX OR maildir:/Fastmail/sent-mail) AND NOT flag:trashed"
           "Last 7 days (Fastmail)"
           ?f)

          ("date:30d..now AND (maildir:/Fastmail/INBOX OR maildir:/Fastmail/sent-mail) AND NOT flag:trashed"
           "Last 30 days (Fastmail)"
           ?m)

          ("date:185d..now AND (maildir:/Fastmail/INBOX OR maildir:/Fastmail/sent-mail) AND NOT flag:trashed"
           "Last 6 months"
           ?h)

          ("date:1y..now AND (maildir:/Fastmail/INBOX OR maildir:/Fastmail/sent-mail) AND NOT flag:trashed"
           "Last year"
           ?y)

          ("date:24h..now" "Last day's messages (with trash)" ?T)
          ("date:7d..now" "Last 7 days (with trash)" ?W)


          ))

  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program (if IS-MAC
                             "/usr/local/bin/msmtp"
                           "/usr/bin/msmtp")
        message-sendmail-envelope-from 'header)

  ;; account management

  (set-email-account!
   "Fastmail"
   '((mu4e-sent-messages-behavior . sent)
     (message-sendmail-extra-arguments . ("-a" "fastmail"))
     (mu4e-sent-folder . "/Fastmail/sent-mail")
     (mu4e-trash-folder . "/Fastmail/trash")
     (mu4e-drafts-folder . "/Fastmail/drafts")
     (mu4e-refile-folder . "/Fastmail/all-mail")
     (mu4e-maildir-shortcuts . (("/Fastmail/INBOX"     . ?i)
                                ("/Fastmail/sent-mail" . ?s)
                                ("/Fastmail/drafts"    . ?d)
                                ("/Fastmail/trash"     . ?t)))
     (mu4e-compose-signature . "Jonathan Irving\nhttps://j0ni.ca\nhttps://keybase.io/j0ni"))
   t)

  ;; end of account management stuff

  ;; From http://zmalltalker.com/linux/mu.html again:

  ;; Wouldn't it be awesome to be able to send files from dired using your mail
  ;; client?

  ;; I'll need a special version of the gnus-dired-mail-buffers function so it
  ;; understands mu4e buffers as well:

  ;; (require 'gnus-dired)

  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode

  ;; (defun gnus-dired-mail-buffers ()
  ;;   "Return a list of active message buffers."
  ;;   (let (buffers)
  ;;     (save-current-buffer
  ;;       (dolist (buffer (buffer-list t))
  ;;         (set-buffer buffer)
  ;;         (when (and (derived-mode-p 'message-mode)
  ;;                    (null message-sent-message-via))
  ;;           (push (buffer-name buffer) buffers))))
  ;;     (nreverse buffers)))

  ;; (setq gnus-dired-mail-mode 'mu4e-user-agent)
  ;; (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

  ;; With this, I can attach a file as an attachment to a new email
  ;; message by entering C-c RET C-a, and I'm good to go.

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; Set up compose mode

  (add-hook 'mu4e-compose-mode-hook #'turn-on-auto-fill)
  (add-hook 'message-mode-hook #'turn-on-auto-fill)

  ;; Citation
  ;; (setq message-indentation-spaces 3)
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-citation-line-format "  On %e %B %Y %R %Z, %f wrote:\n")
  (setq message-yank-prefix "  > ")
  (setq message-yank-cited-prefix "  > ")
  (setq message-yank-empty-prefix "  > ")

  ;; Eliding
  (setq message-elide-ellipsis "[... elided ...]")

  )
