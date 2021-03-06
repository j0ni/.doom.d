;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq-default tab-always-indent t)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.

(setq user-full-name "Jon Irving"
      user-mail-address "j@lollyshouse.ca")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (load! "draculapro-theme.el")
;; (load! "doom-draculapro-theme.el")
;;(setq doom-draculapro-brighter-modeline t)
;;(setq doom-theme 'doom-draculapro)

(setq shr-color-visible-luminance-min 90)
(advice-add #'shr-colorize-region :around (defun shr-no-colorise-region (&rest ignore)))

(column-number-mode 1)

(use-package! ibuffer
  :custom
  (ibuffer-expert t)
  (ibuffer-display-summary nil)
  (ibuffer-use-other-window nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-movement-cycle nil)
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-use-header-line t)
  (ibuffer-default-shrink-to-minimum-size nil)
  (ibuffer-saved-filter-groups nil)
  (ibuffer-old-time 48))

(use-package! diff-hl
  :config
  ;; doom already includes diff-hl, but it switches it on in the margin rather
  ;; than fringe - so first switch it off
  (diff-hl-margin-mode -1)
  :hook ((after-init . global-diff-hl-mode)))

(use-package! ibuffer-vc
  :hook ((ibuffer . j0ni/ibuffer-vc-hook))
  :custom
  (ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           (vc-status 16 16 :left)
           " "
           vc-relative-file)))
  :init
  (defun j0ni/ibuffer-vc-hook ()
    (ibuffer-auto-mode 1)
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic))))

(use-package! modus-themes
  :init
  (modus-themes-load-themes)
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-slanted-constructs t)
  ;; (modus-themes-syntax 'faint-yellow-comments)
  (modus-themes-syntax nil)
  (modus-themes-fringes nil)
  (modus-themes-scale-headings t)
  (modus-themes-completions 'opinionated)
  (modus-themes-mode-line nil)
  (modus-themes-paren-match 'intense-bold)
  :config
  (custom-theme-set-faces! '(modus-operandi modus-vivendi)
    '(bold :weight semibold)))

(use-package! moody
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(setq geiser-active-implementations '(chicken guile racket))

(custom-theme-set-faces! '(dracula draculapro doom-dracula-pro)
  '(bold :weight semibold)
  '(mode-line :background "#373844" :foreground "#f8f8f2")
  '(mode-line-inactive :background "#282a36" :foreground "#ccccc7"))

(setq-default indicate-buffer-boundaries 'left)
(setq-default truncate-lines nil)
(setq-default word-wrap nil)

(defun j0ni/toggle-visual-line-mode ()
  (interactive)
  (if visual-line-mode
      (visual-line-mode -1)
    (visual-line-mode 1)))

(map! (:leader
       (:prefix "t"
        :desc "Toggle visual-line-mode" :n "v" #'j0ni/toggle-visual-line-mode)))

(after! telega
  (telega-mode-line-mode +1)
  (telega-notifications-mode +1)
  (evil-set-initial-state 'telega-chat-mode 'emacs))

(use-package! wolfram
  :custom
  (wolfram-alpha-app-id "Q4EE7L-PLWVXJWUVT"))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string.

(cond
 (IS-LINUX
  (progn
    (setq ;; doom-font (font-spec :family "PragmataPro Liga" :size 36 :weight 'light)
          doom-font (font-spec :family "Monoid" :size 18)
          ;; doom-font (font-spec :family "Iosevka Snuggle" :size 40 :weight 'light)
          doom-unicode-font (font-spec :family "Symbola")
          doom-variable-pitch-font (font-spec :family "sans" :size 20))
    (setq doom-theme 'modus-vivendi)
    ;; (setq doom-theme 'almost-mono-black)
    ;; (setq doom-theme 'almost-mono-gray)
    ;; (setq doom-theme 'cyberpunk)
    ;; (setq fancy-splash-image "~/Dropbox/Home/Pictures/cccp.png")
    (setq fancy-splash-image nil)
    (setq x-super-keysym 'meta)))
 (IS-MAC
  (progn
    (setq doom-font (font-spec :family "Iosevka Snuggle" :size 18 :weight 'light)
          line-spacing 0
          doom-variable-pitch-font (font-spec :family "Lucida Grande" :size 18)
          ns-right-option-modifier 'meta
          mac-command-modifier 'meta)
    (setq doom-theme 'modus-vivendi)
    (setq fancy-splash-image "~/Dropbox/Home/Pictures/cccp.png"))))

;; (custom-set-faces! '(bold :weight bold))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/OrgMode")
(setq org-roam-directory (expand-file-name "org-roam" org-directory))
(setq org-roam-dailies-directory "daily/")
(setq org-roam-capture-templates
      `(("d" "daily" entry #'org-roam-capture--get-point "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n\n")
        ("n" "regular note" plain #'org-roam--capture-get-point "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+title: ${title}\n"
         :unnarrowed t)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Flycheck is mostly annoying, but only intolerable in Clojure.
(setq-default flycheck-disabled-checkers '(clojure))
;; (setq-default flycheck-disabled-checkers nil)

;; This is a little too vague for me, not seeing the throbber
(plist-put +popup-defaults :modeline t)

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
;; (setq doom-modeline-height 25)
;; (setq doom-modeline-bar-width 3)

;; (setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-icon nil)
;; (setq doom-modeline-modal-icon nil)
;; (setq doom-modeline-major-mode-icon nil)
;; (setq doom-modeline-major-mode-color-icon nil)
;; (setq doom-modeline-persp-icon nil)
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)
(setq doom-modeline-vcs-max-length 20)
(setq doom-modeline-workspace-name nil)
(setq doom-modeline-persp-name t)
(setq doom-modeline-irc t)

(setq image-scaling-factor 1.5)
;; (setq right-margin-width)

(when (not (featurep! :ui modeline))
  (defmacro j0ni/diminish (feature mode &optional to-what)
    `(eval-after-load ,feature
       '(diminish ,mode ,to-what)))

  (j0ni/diminish 'git-gutter 'git-gutter-mode)
  (j0ni/diminish 'ws-butler 'ws-butler-mode)
  (j0ni/diminish 'highlight-symbol 'highlight-symbol-mode)
  (j0ni/diminish 'smartparens 'smartparens-mode " ()")
  (j0ni/diminish 'better-jumper 'better-jumper-local-mode)
  (j0ni/diminish 'company 'company-mode)
  (j0ni/diminish 'ivy 'ivy-mode)
  (j0ni/diminish 'org-roam 'org-roam-mode)
  (j0ni/diminish 'gcmh 'gcmh-mode)
  (j0ni/diminish 'evil-traces 'evil-traces-mode)
  (j0ni/diminish 'evil-snipe 'evil-snipe-mode)
  (j0ni/diminish 'evil-snipe 'evil-snipe-local-mode)
  (j0ni/diminish 'outline 'outline-minor-mode)
  (j0ni/diminish 'evil-escape 'evil-escape-mode)
  (j0ni/diminish 'evil-goggles 'evil-goggles-mode)
  (j0ni/diminish 'whitespace 'whitespace-mode)
  (j0ni/diminish 'which-key 'which-key-mode)
  (j0ni/diminish 'projectile 'projectile-mode)
  (j0ni/diminish 'eldoc 'eldoc-mode))

;; (setq projectile-dynamic-mode-line nil)
;; (setq doom-modeline-minor-modes t)
(setq confirm-kill-emacs nil)

(setq treemacs-no-png-images nil)
(setq treemacs-is-never-other-window t)

(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message "\
;; Well I guess you can write anything here huh? Do some lisp here. If you want.

")
(setq company-idle-delay 0.8)

;; whitespace
(setq whitespace-line-column 100)
(setq whitespace-style '(face trailing lines-tail tabs))
(add-hook 'prog-mode-hook #'whitespace-mode)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

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
  ;; (display-time-mode)
  )

(setq +vc-gutter-default-style nil)

;; (fringe-mode nil)
;; (setq-default fringe-mode nil)
;; Shamelessly lifted from @zarkone's config, and tweaked
(defun j0ni/delete-whitespace (&optional backward-only)
  "Replaces all spaces, tabs and newlinesaround point with a single space.
If BACKWARD-ONLY is non-nil, only delete them before point."
  (interactive "*P")
  (let ((orig-pos (point)))
    (delete-region
     (if backward-only
         orig-pos
       (progn
         (skip-chars-forward " \t\n")
         (constrain-to-field nil orig-pos t)))
     (progn
       (skip-chars-backward " \t\n")
       (constrain-to-field nil orig-pos)))
    (unless backward-only (insert " "))))

;; Been missing yooooou
(defun j0ni/insert-shrug ()
  (interactive)
  (insert "¯\\_(ツ)_/¯"))

;; Don't know where I found this, but it I didn't write it
(defun j0ni/toggle-window-split ()
  "Vertical split shows more of each line, horizontal split shows
more lines. This code toggles between them. It only works for
frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(map! :ei "M-\\" #'company-complete
      :ein "C-c s" #'j0ni/insert-shrug
      :ei "C-x |" #'j0ni/toggle-window-split
      :n "g |" #'j0ni/toggle-window-split
      :ei "C-c ." #'j0ni/delete-whitespace
      :n "g ." #'j0ni/delete-whitespace
      (:map company-active-map
       :ei "M-\\" #'company-complete-common-or-cycle
       :ei "C-j" #'company-complete-selection
       :ei "C-n" #'company-select-next
       :ei "C-p" #'company-select-previous))


(setq deft-directory (concat (getenv "HOME") "/Dropbox/OrgMode"))

(defvar j0ni/unicode-mapping-alist
  '((U . "Ü")
    (u . "ü")
    (A . "Ä")
    (a . "ä")
    (O . "Ö")
    (o . "ö")
    (S . "ẞ")
    (s . "ß")
    (l . "λ")))

(defun unicode-fn (k)
  (lambda ()
    (interactive)
    (insert
     (alist-get k j0ni/unicode-mapping-alist))))

;; unicode conveniences
(map! (:leader
       (:prefix "U"
        :desc "Upper-case Ü" :nv "U" (unicode-fn 'U)
        :desc "Lower-case ü" :nv "u" (unicode-fn 'u)
        :desc "Upper-case Ä" :nv "A" (unicode-fn 'A)
        :desc "Lower-case ä" :nv "a" (unicode-fn 'a)
        :desc "Upper-case Ö" :nv "O" (unicode-fn 'O)
        :desc "Lower-case ö" :nv "o" (unicode-fn 'o)
        :desc "Upper-case ẞ" :nv "S" (unicode-fn 'S)
        :desc "Lower-case ß" :nv "s" (unicode-fn 's)
        :desc "Lower-case λ" :nv "l" (unicode-fn 'l))))

(map! (:prefix "C-;"
        :desc "Upper-case Ü" :i "U" (unicode-fn 'U)
        :desc "Lower-case ü" :i "u" (unicode-fn 'u)
        :desc "Upper-case Ä" :i "A" (unicode-fn 'A)
        :desc "Lower-case ä" :i "a" (unicode-fn 'a)
        :desc "Upper-case Ö" :i "O" (unicode-fn 'O)
        :desc "Lower-case ö" :i "o" (unicode-fn 'o)
        :desc "Upper-case ẞ" :i "S" (unicode-fn 'S)
        :desc "Lower-case ß" :i "s" (unicode-fn 's)
        :desc "Lower-case λ" :i "l" (unicode-fn 'l)))

(map! (:leader
       (:prefix "t"
        :desc "Line truncation" :nv "t" #'toggle-truncate-lines
        :desc "Olivetti mode" :nv "o" #'olivetti-mode
        :desc "Focus mode" :nv "d" #'focus-mode)))

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
                   `(:host "irc.freenode.net"
                     :tls t
                     :port 6697)))

(use-package! ctrlf
  :init (ctrlf-mode 1))

;; (setq +format-on-save-enabled-modes '(rustic-mode python-mode))
;; do this instead
(add-hook 'rustic-mode-hook #'format-all-mode)
(add-hook 'python-mode-hook #'format-all-mode)

(use-package! rustic
  :custom
  ;; (rustic-format-trigger 'on-save)
  (rustic-lsp-server 'rust-analyzer)
  (rustic-lsp-format t)
  ;; (rustic-lsp-client 'eglot)
  (rustic-indent-method-chain nil))

(setq indent-tabs-mode nil)

(use-package! lsp-mode
  :custom
  ;; rust customizations
  (lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-proc-macro-enable t)
  (lsp-rust-server 'rust-analyzer)
  (lsp-completion-provider :capf)
  ;; tweaks and matters of concern
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-completion-at-point t)
  (lsp-file-watch-threshold 10000)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all nil)
  (lsp-enable-indentation t)
  (lsp-auto-guess-root t)
  (lsp-enable-snippet nil)
  (lsp-auto-configure t)
  (lsp-lens-enable t)
  (lsp-enable-xref t))

(use-package! lsp-ui
  :config
  (require 'lsp-ui-imenu)
  :custom
  (lsp-ui-autoconfigure t)
  ;; sideline
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-enable t)
  ;; docs
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-alignment 'frame)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-include-signature t)
  ;; peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-peek-always-show nil)
  ;; imenu
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-auto-refresh t))

(defvar my-lisp-modes
  '(emacs-lisp-mode
    cider-repl-mode
    clojure-mode
    scheme-mode
    geiser-mode
    racket-mode
    racket-repl-mode
    lisp-mode))

(defun add-hooks (modes func)
  (dolist (mode modes)
    (add-hook (intern (concat (symbol-name mode) "-hook")) func)))

(use-package! paredit
  :commands (enable-paredit-mode)
  :init
  (add-hooks my-lisp-modes #'enable-paredit-mode))

(use-package! evil-paredit
  :hook ((paredit-mode . evil-paredit-mode)))

(after! (:and clojure-mode lsp-mode)
  (defun j0ni/clojure-mode-hook ()
    (setq lsp-enable-indentation nil))
  (add-hook 'clojure-mode-hook #'j0ni/clojure-mode-hook))

(after! highlight-sexp
  (setq hl-sexp-background-color "#201020"))

(use-package! highlight-symbol
  :hook ((prog-mode . highlight-symbol-mode)
         (dired-mode . highlight-symbol-mode)))

(use-package! browse-kill-ring
  :init
  (browse-kill-ring-default-keybindings))

(use-package! auto-highlight-symbol
  :init
  (global-auto-highlight-symbol-mode t)
  :config
  (add-to-list 'ahs-modes 'clojure-mode)
  (add-to-list 'ahs-modes 'fennel-mode)
  (add-to-list 'ahs-modes 'typescript-mode))

(add-hook 'haskell-mode-hook #'haskell-indent-mode)
(setq haskell-indentation-electric-flag t)

(tooltip-mode -1)

(use-package! typescript-mode
  :custom
  (typescript-indent-level 2))

(use-package! flycheck
  :config
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-help-echo-function nil)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled)))

(use-package! flycheck-posframe
  :custom
  (flycheck-posframe-border-width 0)
  (flycheck-posframe-position 'window-bottom-right-corner))

(after! flycheck
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

(setq +vc-gutter-default-style nil)
(after! git-gutter
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  ;; (define-fringe-bitmap 'git-gutter-fr:added [224]
  ;;   nil nil '(center repeated))
  ;; (define-fringe-bitmap 'git-gutter-fr:modified [224]
  ;;   nil nil '(center repeated))
  ;; (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
  ;;   nil nil 'bottom)
  ;; (setq git-gutter:modified-sign " ")
  ;; (setq git-gutter:added-sign " ")
  ;; (setq git-gutter:deleted-sign " ")
  )

(after! elfeed
  (setq elfeed-feeds '("https://pluralistic.net/feed/")))

(after! ivy
  ;;(add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus))
  (setq ivy-extra-directories nil)
  (setq ivy-use-virtual-buffers t))

;; (use-package! ivy-rich
;;   :hook ((after-init-hook . ivy-rich-mode))
;;   :custom
;;   (ivy-rich-display-transformers-list
;;    '(ivy-switch-buffer
;;      (:columns
;;       ((ivy-rich-candidate (:width 60))
;;        (ivy-rich-switch-buffer-size (:width 7))
;;        (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
;;        (ivy-rich-switch-buffer-major-mode (:width 30 :face warning))
;;        (ivy-rich-switch-buffer-project (:width 20 :face success))
;;        (ivy-rich-switch-buffer-path
;;         (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path
;;                              x (ivy-rich-minibuffer-width 0.4))))))
;;       :predicate
;;       (lambda (cand) (get-buffer cand)))
;;      counsel-find-file
;;      (:columns
;;       ((ivy-read-file-transformer)
;;        (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
;;      counsel-M-x
;;      (:columns
;;       ((counsel-M-x-transformer (:width 60))
;;        (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
;;      counsel-describe-function
;;      (:columns
;;       ((counsel-describe-function-transformer (:width 60))
;;        (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
;;      counsel-describe-variable
;;      (:columns
;;       ((counsel-describe-variable-transformer (:width 60))
;;        (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
;;      counsel-recentf
;;      (:columns
;;       ((ivy-rich-candidate (:width 0.8))
;;        (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
;;      package-install
;;      (:columns
;;       ((ivy-rich-candidate (:width 60))
;;        (ivy-rich-package-version (:width 16 :face font-lock-comment-face))
;;        (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
;;        (ivy-rich-package-install-summary (:face font-lock-doc-face))))))
;;   :config
;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

(after! cider
  ;; (remove-hook 'clojure-mode-hook #'cider-mode)
  ;; (remove-hook 'clojure-mode-hook #'er/add-clojure-mode-expansions)
  ;; (remove-hook 'clojure-mode-hook #'clj-refactor-mode)
  (setq cider-use-fringe-indicators nil)
  (setq cider-prompt-for-symbol nil)
  (setq cider-save-file-on-load t)
  (setq cider-prefer-local-resources t)
  (setq cider-eldoc-display-context-dependent-info t)

  (add-to-list 'cider-test-defining-forms "defruns")
  (evil-set-initial-state 'cider-repl-mode 'emacs)

  (map! :map cider-repl-mode-map
        :ei "RET" #'cider-repl-newline-and-indent
        :ei "C-RET" #'cider-repl-return))

;; (after! inf-clojure
;;   (inf-clojure-update-feature 'clojure 'completion "(complete.core/completions \"%s\")"))

;; Some org-mode setup

;; (add-to-list '+ligatures-in-modes 'org-mode t)
(setq +ligatures-in-modes '(prog-mode))
(remove-hook 'org-mode-hook #'org-superstar-mode)

(after! org
  ;; make it short to start with
  (setq org-startup-folded t)

  ;; set our own todo keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)"
                    "WAITING(w!)"
                    "PAUSED(p!)"
                    "|"
                    "DONE(d!)"
                    "ABANDONED(a!)")))

  ;; switch quickly
  (setq org-use-fast-todo-selection 'auto)
  (setq org-priority-default ?c)

  (setq org-log-done t)
  (setq org-log-into-drawer t)

  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)

  ;; extra indentation
  ;; (setq org-adapt-indentation t)

  ;; let's have pretty source code blocks
  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil)

  (setq org-use-speed-commands t)

  ;; org-capture
  (require 'org-datetree)
  (require 'org-tempo)

  (setq org-default-notes-file (concat org-directory "/berlin.org"))
  (setq org-capture-templates
        `(("j" "Journal" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
           "* %T\n  %?\n\n%a")
          ("s" "Shriek" entry (file+headline ,(concat org-directory "/shrieks.org") "Shrieks")
           "* %T\n%?\n")
          ("n" "Bean" entry (file+olp+datetree ,(concat org-directory "/beans.org") "Beans")
           "* %T\n%?\n")
          ("t" "Task" entry (file+headline ,(concat org-directory "/berlin.org") "Inbox")
           "* TODO %?\n  %a\n%i")
          ("b" "BP Journal" entry (file+olp+datetree ,(concat org-directory "/bp.org") "Blood Pressure")
           "* %T\n** Systolic: %^{systolic}\n** Diastolic: %^{diastolic}\n** Pulse: %^{pulse}\n** Notes\n%?\n")))

  (dolist (tag '(home xapix sanity rachel lauren alice grace family self))
    (add-to-list 'org-tag-persistent-alist `(,tag))))

(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(add-hook! org-mode :append
           #'visual-line-mode
           (lambda () (add-hook 'before-save-hook 'org-update-all-dblocks nil 'local-only))
           (lambda () (electric-indent-local-mode -1)))

(add-hook! org-capture-mode :append #'visual-line-mode)

(after! org-clock
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate))

(after! org-agenda
  ;; set agenda file(s)
  (setq org-agenda-files (list (concat org-directory "/journal.org")
                               (concat org-directory "/berlin.org")
                               (concat org-directory "/shrieks.org")
                               (concat org-directory "/org-roam/")
                               (concat org-directory "/org-roam/daily/")))
  (setq org-agenda-span 14))

(use-package! org-super-agenda
  :after org-agenda
  :custom
  ((org-super-agenda-groups '((:auto-dir-name t))))
  :hook
  ((org-agenda-mode . org-super-agenda-mode)))

(after! (:and org org-roam)
  (setq org-roam-directory (expand-file-name "org-roam" org-directory))
  (setq org-roam-completion-system 'ivy)
  (setq org-roam-buffer-position 'bottom))

(map! (:leader
       (:prefix "n"
        :desc "Open org-roam daily for today" :n "d" #'org-roam-dailies-capture-today)))

;; "borrowed" from Anatoly
(defun j0ni/vertical-three-windows-layout ()
  "Vertical, three window layout"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

(if IS-MAC
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e"))

(after! mu4e
  ;; use it everywhere to send mail
  (setq mail-user-agent 'mu4e-user-agent)

  ;; universal
  (setq mu4e-root-maildir "~/Maildir")

  (defun j0ni/mu4e-bookmark (sub-maildir days char)
    (list (concat "date:" days "d..now AND (maildir:/" sub-maildir "/INBOX OR maildir:/"
                  sub-maildir "/sent-mail) AND NOT flag:trashed")
          (concat "Last " days " days (" sub-maildir ")")
          char))

  ;; default
  (setq mu4e-decryption-policy t
        mu4e-update-interval nil
        mu4e-index-update-in-background nil
        mu4e-get-mail-command "true"
        mu4e-hide-index-messages t
        mu4e-confirm-quit nil
        mu4e-use-fancy-chars nil ;; they actually look shit
        mu4e-headers-sort-direction 'ascending
        mu4e-headers-skip-duplicates t
        mu4e-change-filenames-when-moving t
        mu4e-headers-hide-predicate nil
        mu4e-headers-include-related t
        mu4e-split-view nil
        mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:mailing-list . 16)
                              (:from-or-to . 25)
                              (:thread-subject))
        mu4e-compose-complete-only-after "2012-01-01"
        mu4e-view-show-addresses t
        mu4e-compose-complete-addresses t
        mm-inline-large-images 'resize

        message-send-mail-function 'smtpmail-send-it
        message-kill-buffer-on-exit t
        mail-user-agent 'mu4e-user-agent
        message-citation-line-function 'message-insert-formatted-citation-line
        message-citation-line-format "On %a, %d %b %Y at %T %z, %f wrote:"
        mu4e-personal-addresses '("j@lollyshouse.ca"
                                  "hi@mhcat.ca"
                                  "jonathan.irving@gmail.com"
                                  "jon@xapix.io"
                                  "j0ni@fastmail.com"
                                  "joni@well.com"
                                  "j0ni@protonmail.com"
                                  "jon@arity.ca")
        mml-secure-openpgp-signers '("D6346AC6D110409636A0DBF4F7F645B8CE3F8FA3")
        mml-secure-openpgp-sign-with-sender nil
        mu4e-context-policy 'pick-first
        mu4e-contexts
        (list (make-mu4e-context
               :name "Fastmail"
               :enter-func (lambda ()
                             (when (mu4e-running-p)
                               (mu4e-update-mail-and-index nil))
                             (mu4e-message "Switching to Fastmail context"))
               :leave-func #'mu4e-clear-caches
               :match-func (lambda (msg)
                             (when msg
                               (string-match-p "^/Fastmail" (mu4e-message-field msg :maildir))))
               :vars `((user-mail-address . "j@lollyshouse.ca")
                       (user-full-name . "Jon Irving")
                       (mu4e-sent-messages-behavior . sent)
                       (mu4e-sent-folder . "/Fastmail/sent-mail")
                       (mu4e-trash-folder . "/Fastmail/trash")
                       (mu4e-drafts-folder . "/Fastmail/drafts")
                       (mu4e-refile-folder . "/Fastmail/all-mail")
                       (mu4e-maildir-shortcuts . (("/Fastmail/INBOX" . ?i)
                                                  ("/Fastmail/sent-mail" . ?s)
                                                  ("/Fastmail/drafts" . ?d)
                                                  ("/Fastmail/trash" . ?t)))
                       (mu4e-compose-signature . "In this world / we walk on the roof of hell / gazing at flowers\n    - Kobayashi Issa\n\nhttps://j0ni.ca ~ https://keybase.io/j0ni")
                       (mu4e-bookmarks . ,(list (j0ni/mu4e-bookmark "Fastmail" "7" ?w)
                                                (j0ni/mu4e-bookmark "Fastmail" "30" ?m)))
                       (smtpmail-smtp-user . "j0ni@fastmail.com")
                       (smtpmail-smtp-server . "smtp.fastmail.com")
                       (smtpmail-smtp-service . 587)
                       (smtpmail-stream-type . starttls)))
              (make-mu4e-context
               :name "Xapix"
               :enter-func (lambda ()
                             (when (mu4e-running-p)
                               (mu4e-update-mail-and-index nil))
                             (mu4e-message "Switching to Xapix context"))
               :leave-func #'mu4e-clear-caches
               :match-func (lambda (msg)
                             (when msg
                               (string-match-p "^/Xapix" (mu4e-message-field msg :maildir))))
               :vars `((user-mail-address . "jon@xapix.io")
                       (user-full-name . "Jon Irving")
                       (mu4e-sent-messages-behavior . sent)
                       (mu4e-sent-folder . "/Xapix/sent-mail")
                       (mu4e-trash-folder . "/Xapix/trash")
                       (mu4e-drafts-folder . "/Xapix/drafts")
                       (mu4e-refile-folder . "/Xapix/all-mail")
                       (mu4e-maildir-shortcuts . (("/Xapix/INBOX" . ?i)
                                                  ("/Xapix/sent-mail" . ?s)
                                                  ("/Xapix/drafts" . ?d)
                                                  ("/Xapix/trash" . ?t)))
                       (mu4e-compose-signature . "https://j0ni.ca ~ https://keybase.io/j0ni ~ https://xapix.io")
                       (mu4e-bookmarks . ,(list (j0ni/mu4e-bookmark "Xapix" "7" ?w)
                                                (j0ni/mu4e-bookmark "Xapix" "30" ?m)))
                       (smtpmail-smtp-user . "jon@xapix.io")
                       (smtpmail-smtp-server . "smtp.gmail.com")
                       (smtpmail-smtp-service . 587)
                       (smtpmail-stream-type . starttls)))
              (make-mu4e-context
               :name "Well"
               :enter-func (lambda ()
                             (when (mu4e-running-p)
                               (mu4e-update-mail-and-index nil))
                             (mu4e-message "Switching to the Well context"))
               :leave-func #'mu4e-clear-caches
               :match-func (lambda (msg)
                             (when msg
                               (string-match-p "^/Well" (mu4e-message-field msg :maildir))))
               :vars `((user-mail-address . "joni@well.com")
                       (user-full-name . "Jon Irving")
                       (mu4e-sent-messages-behavior . sent)
                       (mu4e-sent-folder . "/Well/Sent")
                       (mu4e-trash-folder . "/Well/Trash")
                       (mu4e-drafts-folder . "/Well/Drafts")
                       (mu4e-refile-folder . "/Well/Archive")
                       (mu4e-maildir-shortcuts . (("/Well/INBOX" . ?i)
                                                  ("/Well/Sent" . ?s)
                                                  ("/Well/Drafts" . ?d)
                                                  ("/Well/Trash" . ?t)
                                                  ("/Well/Archive" . ?a)))
                       (mu4e-compose-signature . "https://j0ni.ca ~ https://keybase.io/j0ni")
                       (mu4e-bookmarks . ,(list (j0ni/mu4e-bookmark "Well" "7" ?w)
                                                (j0ni/mu4e-bookmark "Well" "30" ?m)))
                       (smtpmail-smtp-user . "joni")
                       (smtpmail-smtp-server . "iris.well.com")
                       (smtpmail-smtp-service . 587)
                       (smtpmail-stream-type . starttls)))))

  ;; Set up compose mode
  ;; (add-hook 'message-mode-hook #'visual-line-mode)
  (add-hook 'message-mode-hook #'auto-fill-mode)
  (add-hook 'message-mode-hook #'mml-secure-message-sign-pgpmime))
