;;; doom-draculapro-theme.el - based on https://draculatheme.com/ -*- no-byte-compile: t; -*-
(require 'doom-themes)

(defgroup doom-draculapro-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-draculapro-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-draculapro-theme
  :type 'boolean)

(defcustom doom-draculapro-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-draculapro-theme
  :type 'boolean)

(defcustom doom-draculapro-comment-bg doom-draculapro-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-draculapro-theme
  :type 'boolean)

(defcustom doom-draculapro-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-draculapro-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-draculapro
  "A dark theme based on Dracula theme"

  ;; name        default   256       16
  ((bg         '("#17161d" "color-235" nil          ))
   (bg-alt     '("#0b0b0f" "color-234" nil          ))
   (base0      '("#0b0b0f" "color-234" "black"      ))
   (base1      '("#282a36" "#1e1e1e"   "brightblack"))
   (base2      '("#373844" "#2e2e2e"   "brightblack"))
   (base3      '("#44475a" "#262626"   "brightblack"))
   (base4      '("#565761" "#3f3f3f"   "brightblack"))
   (base5      '("#6272a4" "#525252"   "brightblack"))
   (base6      '("#b6b6b2" "#bbbbbb"   "brightblack"))
   (base7      '("#ccccc7" "#cccccc"   "brightblack"))
   (base8      '("#f8f8f2" "#dfdfdf"   "white"      ))
   (fg         '("#f8f8f2" "color-239" "white"      ))
   (fg-alt     '("#e2e2dc" "#bfbfbf"   "brightwhite"))
   (current    '("#454158" "color-239" "grey30"     ))
   (comment    '("#7970a9" "color-61"  "grey20"     ))

   (grey       base4)
   (red        '("#ff9580" "color-203" "red"        ))
   (orange     '("#ffca80" "color-215" "brightred"  ))
   (green      '("#8aff80" "color-84"  "green"      ))
   (teal       '("#0189cc" "#0088cc"   "brightgreen"))
   (yellow     '("#ffff80" "color-228" "yellow"     ))
   (blue       '("#61bfff" "#66bbff"   "brightblue" ))
   (dark-blue  '("#0189cc" "#0088cc"   "blue"       ))
   (purple     '("#9580ff" "color-141" "purple"     ))
   (pink       '("#ff80bf" "color-212" "pink"       ))
   (cyan       '("#80ffea" "color-117" "brightcyan" ))
   (dark-cyan  '("#8be9fd" "#88eeff"   "cyan"       ))

   ;; Somehow these are required elsewhere
   (violet     pink)
   (magenta    purple)

   ;; face categories -- required for all themes
   (highlight      pink)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        orange)
   (comments       comment)
   (doc-comments   (doom-lighten comment 0.25))
   (constants      cyan)
   (functions      green)
   (keywords       pink)
   (methods        teal)
   (operators      pink)
   (type           purple)
   (strings        yellow)
   (variables      (doom-lighten fg 0.6))
   (numbers        pink)
   (region         yellow)
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (level1 pink)
   (level2 purple)
   (level3 green)
   (level4 yellow)
   (level5 cyan)
   (level6 orange)
   (level7 comment)
   (level8 fg)
   (level9 pink)

   (hidden     bg)
   (-modeline-bright doom-draculapro-brighter-modeline)
   (-modeline-pad
    (when doom-draculapro-padded-modeline
      (if (integerp doom-draculapro-padded-modeline) doom-draculapro-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt comment)

   (modeline-bg
    (if -modeline-bright
        (doom-darken  purple 0.7)
      `(,(car bg) ,@(cdr bg-alt))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken purple 0.6)
      `(,(doom-darken (car bg) 0.15) ,@(cdr bg-alt))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(doom-darken (car bg) 0.075) ,@(cdr bg))))

  ;; --- extra faces ------------------------
  ((cursor :background fg)
   (completions-first-difference :foreground pink :weight 'bold)
   (default :background bg :foreground fg)
   (default-italic :slant 'italic)
   (ffap :foreground fg)
   (fringe :background bg :foreground fg)
   (highlight :background current)
   (hl-line :background (doom-darken current 0.4) :extend t)
   (info-quoted-name :foreground orange)
   (info-string :foreground yellow)
   (lazy-highlight :background purple)
   (link :foreground cyan :underline t)
   (linum :slant 'italic :foreground comment :background bg)
   (line-number :slant 'italic :foreground comment :background bg)
   (match :background yellow :foreground bg)

   (minibuffer-prompt :weight 'bold :foreground pink)
   (read-multiple-choice-face :inherit 'completions-first-difference)
   (region :inherit 'match :extend t)
   (trailing-whitespace :background orange :foreground orange)
   (vertical-border :foreground (doom-darken fg 0.4))
   (success :foreground green)
   (warning :foreground orange)
   (error :foreground red)
   (header-line :background bg)
   ;; syntax
   (font-lock-builtin-face :foreground orange)
   (font-lock-comment-face :foreground comment)
   (font-lock-comment-delimiter-face :foreground comment)
   (font-lock-constant-face :foreground cyan)
   (font-lock-doc-face :foreground comment)
   (font-lock-function-name-face :foreground green :weight 'bold)
   (font-lock-keyword-face :weight 'bold :foreground pink)
   (font-lock-negation-char-face :foreground cyan)
   (font-lock-preprocessor-face :foreground orange)
   (font-lock-reference-face :foreground cyan)
   (font-lock-regexp-grouping-backslash :foreground cyan)
   (font-lock-regexp-grouping-construct :foreground purple)
   (font-lock-string-face :foreground yellow)
   (font-lock-type-face :foreground purple)
   (font-lock-variable-name-face :foreground fg :weight 'bold)
   (font-lock-warning-face :foreground orange :background bg-alt)
   ;; auto-complete
   (ac-completion-face :underline t :foreground pink)
   ;; company
   (company-echo-common :foreground bg :background fg)
   (company-preview :background current :foreground comment)
   (company-preview-common :inherit 'company-preview :foreground pink)
   (company-preview-search :inherit 'company-preview :foreground green)
   (company-scrollbar-bg :background comment)
   (company-scrollbar-fg :foreground comment)
   (company-tooltip :foreground fg :background current)
   (company-tooltip-search :foreground green :underline t)
   (company-tooltip-search-selection :background green :foreground bg)
   (company-tooltip-selection :inherit 'match)
   (company-tooltip-mouse :background bg)
   (company-tooltip-common :foreground pink :weight 'bold)
   (company-tooltip-annotation :foreground cyan)
   ;; diff-hl
   (diff-hl-change :foreground orange :background orange)
   (diff-hl-delete :foreground red :background red)
   (diff-hl-insert :foreground green :background green)
   ;; dired
   (dired-directory :foreground green :weight 'normal)
   (dired-flagged :foreground pink)
   (dired-header :foreground fg :background bg)
   (dired-ignored :inherit 'shadow)
   (dired-mark :foreground fg :weight 'bold)
   (dired-marked :foreground orange :weight 'bold)
   (dired-perm-write :foreground fg :underline t)
   (dired-symlink :foreground yellow :weight 'normal :slant 'italic)
   (dired-warning :foreground orange :underline t)
   (diredp-compressed-file-name :foreground fg)
   (diredp-compressed-file-suffix :foreground fg)
   (diredp-date-time :foreground fg)
   (diredp-deletion-file-name :foreground pink :background current)
   (diredp-deletion :foreground pink :weight 'bold)
   (diredp-dir-heading :foreground fg :background comment)
   (diredp-dir-name :inherit 'dired-directory)
   (diredp-dir-priv :inherit 'dired-directory)
   (diredp-executable-tag :foreground orange)
   (diredp-file-name :foreground fg)
   (diredp-file-suffix :foreground fg)
   (diredp-flag-mark-line :foreground fg :slant 'italic :background current)
   (diredp-flag-mark :foreground fg :weight 'bold :background current)
   (diredp-ignored-file-name :foreground fg)
   (diredp-mode-line-flagged :foreground orange)
   (diredp-mode-line-marked :foreground orange)
   (diredp-no-priv :foreground fg)
   (diredp-number :foreground cyan)
   (diredp-other-priv :foreground orange)
   (diredp-rare-priv :foreground orange)
   (diredp-read-priv :foreground purple)
   (diredp-write-priv :foreground pink)
   (diredp-exec-priv :foreground yellow)
   (diredp-symlink :foreground orange)
   (diredp-link-priv :foreground orange)
   (diredp-autofile-name :foreground yellow)
   (diredp-tagged-autofile-name :foreground yellow)

   (highlight-indentation-face :background bg-alt)
   (icompletep-determined :foreground orange)

   ;; flyspell
   (flyspell-duplicate :underline `(:style wave :color ,orange))
   (flyspell-incorrect :underline `(:style wave :color ,red))
   ;; font-latex
   (font-latex-bold-face :foreground purple)
   (font-latex-italic-face :foreground pink :slant 'italic)
   (font-latex-match-reference-keywords :foreground cyan)
   (font-latex-match-variable-keywords :foreground fg)
   (font-latex-string-face :foreground yellow)
   ;; gnus-group
   (gnus-group-mail-1 :foreground pink :weight 'bold)
   (gnus-group-mail-1-empty :inherit 'gnus-group-mail-1 :weight 'normal)
   (gnus-group-mail-2 :foreground cyan :weight 'bold)
   (gnus-group-mail-2-empty :inherit 'gnus-group-mail-2 :weight 'normal)
   (gnus-group-mail-3 :foreground comment :weight 'bold)
   (gnus-group-mail-3-empty :inherit 'gnus-group-mail-3 :weight 'normal)
   (gnus-group-mail-low :foreground current :weight 'bold)
   (gnus-group-mail-low-empty :inherit 'gnus-group-mail-low :weight 'normal)
   (gnus-group-news-1 :foreground pink :weight 'bold)
   (gnus-group-news-1-empty :inherit 'gnus-group-news-1 :weight 'normal)
   (gnus-group-news-2 :foreground cyan :weight 'bold)
   (gnus-group-news-2-empty :inherit 'gnus-group-news-2 :weight 'normal)
   (gnus-group-news-3 :foreground comment :weight 'bold)
   (gnus-group-news-3-empty :inherit 'gnus-group-news-3 :weight 'normal)
   (gnus-group-news-4 :inherit 'gnus-group-news-low)
   (gnus-group-news-4-empty :inherit 'gnus-group-news-low-empty)
   (gnus-group-news-5 :inherit 'gnus-group-news-low)
   (gnus-group-news-5-empty :inherit 'gnus-group-news-low-empty)
   (gnus-group-news-6 :inherit 'gnus-group-news-low)
   (gnus-group-news-6-empty :inherit 'gnus-group-news-low-empty)
   (gnus-group-news-low :foreground current :weight 'bold)
   (gnus-group-news-low-empty :inherit 'gnus-group-news-low :weight 'normal)
   (gnus-header-content :foreground pink)
   (gnus-header-from :foreground fg)
   (gnus-header-name :foreground purple)
   (gnus-header-subject :foreground green :weight 'bold)
   (gnus-summary-markup-face :foreground cyan)
   (gnus-summary-high-unread :foreground pink :weight 'bold)
   (gnus-summary-high-read :inherit 'gnus-summary-high-unread :weight 'normal)
   (gnus-summary-high-ancient :inherit 'gnus-summary-high-read)
   (gnus-summary-high-ticked :inherit 'gnus-summary-high-read :underline t)
   (gnus-summary-normal-unread :foreground comment :weight 'bold)
   (gnus-summary-normal-read :foreground comment :weight 'normal)
   (gnus-summary-normal-ancient :inherit 'gnus-summary-normal-read :weight 'light)
   (gnus-summary-normal-ticked :foreground pink :weight 'bold)
   (gnus-summary-low-unread :foreground comment :weight 'bold)
   (gnus-summary-low-read :inherit 'gnus-summary-low-unread :weight 'normal)
   (gnus-summary-low-ancient :inherit 'gnus-summary-low-read)
   (gnus-summary-low-ticked :inherit 'gnus-summary-low-read :underline t)
   (gnus-summary-selected :inverse-video t)
   ;; haskell-mode
   (haskell-operator-face :foreground pink)
   (haskell-constructor-face :foreground purple)
   ;; helm
   (helm-bookmark-w3m :foreground purple)
   (helm-buffer-not-saved :foreground purple :background bg)
   (helm-buffer-process :foreground orange :background bg)
   (helm-buffer-saved-out :foreground fg :background bg)
   (helm-buffer-size :foreground fg :background bg)
   (helm-candidate-number :foreground bg :background fg)
   (helm-ff-directory :foreground green :background bg :weight 'bold)
   (helm-ff-dotted-directory :foreground green :background bg :weight 'normal)
   (helm-ff-executable :foreground comment :background bg :weight 'normal)
   (helm-ff-file :foreground fg :background bg :weight 'normal)
   (helm-ff-invalid-symlink :foreground pink :background bg :weight 'bold)
   (helm-ff-prefix :foreground bg :background pink :weight 'normal)
   (helm-ff-symlink :foreground pink :background bg :weight 'bold)
   (helm-grep-cmd-line :foreground fg :background bg)
   (helm-grep-file :foreground fg :background bg)
   (helm-grep-finish :foreground fg :background bg)
   (helm-grep-lineno :foreground fg :background bg)
   (helm-grep-match :foreground 'unspecified :background 'unspecified :inherit 'helm-match)
   (helm-grep-running :foreground green :background bg)
   (helm-header :foreground fg :background bg :underline nil :box nil)
   (helm-moccur-buffer :foreground green :background bg)
   (helm-selection :background bg-alt :underline nil)
   (helm-selection-line :background bg-alt)
   (helm-separator :foreground purple :background bg)
   (helm-source-go-package-godoc-description :foreground yellow)
   (helm-source-header :foreground pink :background bg :underline nil :weight 'bold)
   (helm-time-zone-current :foreground orange :background bg)
   (helm-time-zone-home :foreground purple :background bg)
   (helm-visible-mark :foreground bg :background bg)
   ;; icicle
   (icicle-whitespace-highlight :background fg)
   (icicle-special-candidate :foreground fg)
   (icicle-extra-candidate :foreground fg)
   (icicle-search-main-regexp-others :foreground fg)
   (icicle-search-current-input :foreground pink)
   (icicle-search-context-level-8 :foreground orange)
   (icicle-search-context-level-7 :foreground orange)
   (icicle-search-context-level-6 :foreground orange)
   (icicle-search-context-level-5 :foreground orange)
   (icicle-search-context-level-4 :foreground orange)
   (icicle-search-context-level-3 :foreground orange)
   (icicle-search-context-level-2 :foreground orange)
   (icicle-search-context-level-1 :foreground orange)
   (icicle-search-main-regexp-current :foreground fg)
   (icicle-saved-candidate :foreground fg)
   (icicle-proxy-candidate :foreground fg)
   (icicle-mustmatch-completion :foreground purple)
   (icicle-multi-command-completion :foreground fg :background bg-alt)
   (icicle-msg-emphasis :foreground green)
   (icicle-mode-line-help :foreground fg)
   (icicle-match-highlight-minibuffer :foreground orange)
   (icicle-match-highlight-Completions :foreground green)
   (icicle-key-complete-menu-local :foreground fg)
   (icicle-key-complete-menu :foreground fg)
   (icicle-input-completion-fail-lax :foreground pink)
   (icicle-input-completion-fail :foreground pink)
   (icicle-historical-candidate-other :foreground fg)
   (icicle-historical-candidate :foreground fg)
   (icicle-current-candidate-highlight :foreground orange :background bg)
   (icicle-Completions-instruction-2 :foreground fg)
   (icicle-Completions-instruction-1 :foreground fg)
   (icicle-completion :foreground fg)
   (icicle-complete-input :foreground orange)
   (icicle-common-match-highlight-Completions :foreground purple)
   (icicle-candidate-part :foreground fg)
   (icicle-annotation :foreground fg)
   ;; icomplete
   (icompletep-determined :foreground orange)
   ;; ido
   (ido-first-match :weight 'bold :foreground pink)
   (ido-only-match :foreground orange)
   (ido-subdir :foreground yellow)
   (ido-virtual :foreground cyan)
   (ido-incomplete-regexp :inherit 'font-lock-warning-face)
   (ido-indicator :foreground fg :background pink)

   ;; ivy
   (ivy-current-match :background (doom-darken current 0.4) :extend t)
   (ivy-subdir :inherit 'ido-subdir)
   (ivy-virtual :inherit 'ido-virtual)
   ;;
   ;; isearch
   (isearch :background pink :weight 'bold)
   (isearch-fail :foreground bg :background red)

   ;; nav-flash
   (nav-flash-face :background comment :extend t)

   ;; jde-java
   (jde-java-font-lock-constant-face :foreground cyan)
   (jde-java-font-lock-modifier-face :foreground pink)
   (jde-java-font-lock-number-face :foreground fg)
   (jde-java-font-lock-package-face :foreground fg)
   (jde-java-font-lock-private-face :foreground pink)
   (jde-java-font-lock-public-face :foreground pink)
   ;; js2-mode
   (js2-external-variable :foreground purple)
   (js2-function-param :foreground cyan)
   (js2-jsdoc-html-tag-delimiter :foreground yellow)
   (js2-jsdoc-html-tag-name :foreground blue)
   (js2-jsdoc-value :foreground yellow)
   (js2-private-function-call :foreground cyan)
   (js2-private-member :foreground fg)
   ;; js3-mode
   (js3-error-face :underline orange)
   (js3-external-variable-face :foreground fg)
   (js3-function-param-face :foreground pink)
   (js3-instance-member-face :foreground cyan)
   (js3-jsdoc-tag-face :foreground pink)
   (js3-warning-face :underline pink)
   ;; magit
   (magit-branch-local :foreground cyan)
   (magit-branch-remote :foreground green)
   (magit-tag :foreground orange)
   (magit-section-heading :foreground pink :weight 'bold)
   (magit-section-highlight :background bg :extend t)
   (magit-diff-context-highlight :background bg :foreground fg :extend t)
   (magit-diff-revision-summary :foreground orange :background bg :weight 'bold)
   (magit-diff-revision-summary-highlight :foreground orange :background bg :weight 'bold :extend t)
   ;; the four following lines are just a patch of the
   ;; upstream color to add the extend keyword.
   (magit-diff-added :background (doom-darken green 0.8) :foreground green  :extend t)
   (magit-diff-added-highlight :background (doom-darken green 0.6) :foreground (doom-lighten green 0.2) :extend t)
   (magit-diff-removed :background (doom-darken red 0.8) :foreground (doom-darken pink 0.2) :extend t)
   (magit-diff-removed-highlight :background (doom-darken red 0.6) :foreground pink :extend t)

   (magit-diff-file-heading :foreground fg)
   (magit-diff-file-heading-highlight :inherit 'magit-section-highlight)
   (magit-diffstat-added :foreground green)
   (magit-diffstat-removed :foreground red)
   (magit-hash :foreground fg)
   (magit-hunk-heading :background bg)
   (magit-hunk-heading-highlight :background bg)
   (magit-item-highlight :background bg)
   (magit-log-author :foreground fg)
   (magit-process-ng :foreground orange :weight 'bold)
   (magit-process-ok :foreground green :weight 'bold)
   ;; markdown
   (markdown-blockquote-face :foreground orange)
   (markdown-code-face :foreground orange)
   (markdown-footnote-face :foreground comment)
   (markdown-header-face :weight 'normal)
   (markdown-header-face-1 :inherit 'bold :foreground pink)
   (markdown-header-face-2 :inherit 'bold :foreground purple)
   (markdown-header-face-3 :foreground green)
   (markdown-header-face-4 :foreground yellow)
   (markdown-header-face-5 :foreground cyan)
   (markdown-header-face-6 :foreground orange)
   (markdown-header-face-7 :foreground comment)
   (markdown-header-face-8 :foreground fg)
   (markdown-inline-code-face :foreground yellow)
   (markdown-plain-url-face :inherit 'link)
   (markdown-pre-face :foreground orange)
   (markdown-table-face :foreground purple)
   ;; message
   (message-mml :foreground green :weight 'normal)
   (message-header-xheader :foreground cyan :weight 'normal)
   ;;
   ;; org
   (org-agenda-date :foreground cyan :underline nil)
   (org-agenda-dimmed-todo-face :foreground comment)
   (org-agenda-done :foreground green)
   (org-agenda-structure :foreground purple)
   (org-block :foreground orange)
   (org-code :foreground yellow)
   (org-column :background comment)
   (org-column-title :inherit 'org-column :weight 'bold :underline t)
   (org-date :foreground cyan :underline t)
   (org-document-info :foreground comment)
   (org-document-info-keyword :foreground comment)
   (org-document-title :weight 'bold :foreground orange)
   (org-done :foreground green)
   (org-ellipsis :foreground comment)
   (org-footnote :foreground comment)
   (org-formula :foreground pink)
   (org-headline-done :foreground comment :weight 'normal :strike-through t)
   (org-hide :foreground bg :background bg)
   (org-level-1 :inherit 'bold :foreground pink)
   (org-level-2 :inherit 'bold :foreground purple)
   (org-level-3 :weight 'normal :foreground green)
   (org-level-4 :weight 'normal :foreground yellow)
   (org-level-5 :weight 'normal :foreground cyan)
   (org-level-6 :weight 'normal :foreground orange)
   (org-level-7 :weight 'normal :foreground comment)
   (org-level-8 :weight 'normal :foreground fg)
   (org-link :foreground cyan :underline t)
   (org-priority :foreground cyan)
   (org-scheduled :foreground green)
   (org-scheduled-previously :foreground yellow)
   (org-scheduled-today :foreground green)
   (org-sexp-date :foreground fg)
   (org-special-keyword :foreground yellow)
   (org-table :foreground purple)
   (org-tag :foreground pink :weight 'bold :background bg-alt)
   (org-todo :foreground orange :weight 'bold :background bg-alt)
   (org-upcoming-deadline :foreground yellow)
   (org-warning :weight 'bold :foreground pink)
   ;; outline
   (outline-1 :foreground pink)
   (outline-2 :foreground purple)
   (outline-3 :foreground green)
   (outline-4 :foreground yellow)
   (outline-5 :foreground cyan)
   (outline-6 :foreground orange)
   ;; powerline
   (powerline-evil-base-face :foreground bg-alt)
   (powerline-evil-emacs-face :inherit 'powerline-evil-base-face :background yellow)
   (powerline-evil-insert-face :inherit 'powerline-evil-base-face :background cyan)
   (powerline-evil-motion-face :inherit 'powerline-evil-base-face :background purple)
   (powerline-evil-normal-face :inherit 'powerline-evil-base-face :background green)
   (powerline-evil-operator-face :inherit 'powerline-evil-base-face :background pink)
   (powerline-evil-replace-face :inherit 'powerline-evil-base-face :background red)
   (powerline-evil-visual-face :inherit 'powerline-evil-base-face :background orange)
   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground fg)
   (rainbow-delimiters-depth-2-face :foreground cyan)
   (rainbow-delimiters-depth-3-face :foreground purple)
   (rainbow-delimiters-depth-4-face :foreground pink)
   (rainbow-delimiters-depth-5-face :foreground orange)
   (rainbow-delimiters-depth-6-face :foreground green)
   (rainbow-delimiters-depth-7-face :foreground yellow)
   (rainbow-delimiters-depth-8-face :foreground blue)
   (rainbow-delimiters-unmatched-face :foreground orange)
   ;; rpm-spec
   (rpm-spec-dir-face :foreground green)
   (rpm-spec-doc-face :foreground pink)
   (rpm-spec-ghost-face :foreground purple)
   (rpm-spec-macro-face :foreground yellow)
   (rpm-spec-obsolete-tag-face :inherit 'font-lock-warning-face)
   (rpm-spec-package-face :foreground purple)
   (rpm-spec-section-face :foreground yellow)
   (rpm-spec-tag-face :foreground cyan)
   (rpm-spec-var-face :foreground orange)
   ;; show-paren
   (show-paren-match-face :background 'unspecified :foreground cyan :weight 'bold)
   (show-paren-match :background 'unspecified :foreground cyan :weight 'bold)
   (show-paren-match-expression :inherit 'match)
   (show-paren-mismatch :inherit 'font-lock-warning-face)
   ;; slime
   (slime-repl-inputed-output-face :foreground purple)
   ;; spam
   (spam :inherit 'gnus-summary-normal-read :foreground orange :strike-through t :slant 'oblique)
   ;; speedbar (and sr-speedbar)
   (speedbar-button-face :foreground green)
   (speedbar-file-face :foreground cyan)
   (speedbar-directory-face :foreground purple)
   (speedbar-tag-face :foreground yellow)
   (speedbar-selected-face :foreground pink)
   (speedbar-highlight-face :inherit 'match)
   (speedbar-separator-face :background bg :foreground fg :weight 'bold)
   ;; tab-bar & tab-line (since Emacs 27.1)
   (tab-bar :foreground purple :background current :inherit 'variable-pitch)
   (tab-bar-tab :foreground pink :background bg :box `(:line-width 2 :color ,bg :style nil))
   (tab-bar-tab-inactive :foreground purple :background bg-alt :box `(:line-width 2 :color ,bg-alt :style nil))
   (tab-line :foreground purple :background current :height 0.9 :inherit 'variable-pitch)
   (tab-line-tab :foreground pink :background bg :box `(:line-width 2 :color ,bg :style nil))
   (tab-line-tab-inactive :foreground purple :background bg-alt :box `(:line-width 2 :color ,bg-alt :style nil))
   (tab-line-tab-current :inherit 'tab-line-tab)
   (tab-line-close-highlight :foreground red)
   ;; term
   (term :foreground fg :background bg)
   (term-color-black :foreground bg :background bg)
   (term-color-blue :foreground purple :background purple)
   (term-color-cyan :foreground cyan :background cyan)
   (term-color-green :foreground green :background green)
   (term-color-magenta :foreground pink :background pink)
   (term-color-red :foreground red :background red)
   (term-color-white :foreground fg :background fg)
   (term-color-yellow :foreground yellow :background yellow)
   ;; undo-tree
   (undo-tree-visualizer-current-face :foreground orange)
   (undo-tree-visualizer-default-face :foreground fg)
   (undo-tree-visualizer-register-face :foreground purple)
   (undo-tree-visualizer-unmodified-face :foreground fg)
   ;; web-mode
   (web-mode-builtin-face :inherit 'font-lock-builtin-face)
   (web-mode-comment-face :inherit 'font-lock-comment-face)
   (web-mode-constant-face :inherit 'font-lock-constant-face)
   (web-mode-doctype-face :inherit 'font-lock-comment-face)
   (web-mode-function-name-face :inherit 'font-lock-function-name-face)
   (web-mode-html-attr-name-face :foreground purple)
   (web-mode-html-attr-value-face :foreground green)
   (web-mode-html-tag-face :foreground pink :weight bold)
   (web-mode-keyword-face :foreground pink)
   (web-mode-string-face :foreground yellow)
   (web-mode-type-face :inherit 'font-lock-type-face)
   (web-mode-warning-face :inherit 'font-lock-warning-face)
   ;; which-func
   (which-func :inherit 'font-lock-function-name-face)
   ;; whitespace
   (whitespace-big-indent :background red :foreground red)
   (whitespace-empty :background orange :foreground red)
   (whitespace-hspace :background bg :foreground comment)
   (whitespace-indentation :background orange :foreground red)
   (whitespace-line :background bg :foreground pink)
   (whitespace-newline :foreground comment)
   (whitespace-space :background bg :foreground comment)
   (whitespace-space-after-tab :background orange :foreground red)
   (whitespace-space-before-tab :background orange :foreground red)
   (whitespace-tab :background bg-alt :foreground comment)
   (whitespace-trailing :inherit 'trailing-whitespace)

   (indent-guide-face :background nil :foreground (doom-darken comment 0.5))

   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comment
    :background (if doom-draculapro-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   (solaire-hl-line-face :background base2)
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground pink)
   (css-property             :foreground pink)
   (css-selector             :foreground green)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)

   ;; rjsx-mode
   (rjsx-tag :foreground purple)
   (rjsx-attr :foreground green :slant 'italic :weight 'medium)

   ;; highlight-quoted-mode
   (highlight-quoted-symbol :foreground cyan)
   (highlight-quoted-quote  :foreground purple)
   )

  ;; --- extra variables ---------------------
  ()
  )

;;; doom-draculapro-theme.el ends here
