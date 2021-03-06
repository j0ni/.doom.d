;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

(package! lsp-ui :disable t)
(package! flycheck)
(package! flycheck-posframe :disable t)
(package! diminish :disable t)
(package! solaire-mode :disable t)
;;(package! git-gutter :disable t)
;;(package! git-gutter-fringe :disable t)
(package! diff-hl :disable t)
(package! dracula-theme :disable t)
(package! indent-guide :disable t)
(package! nyan-mode :disable t)
(package! modus-themes)
(package! cyberpunk-theme)
(package! leuven-theme :disable t)
(package! color-theme-sanityinc-tomorrow :disable t)
(package! tide :disable t)
(package! fennel-mode :recipe (:host gitlab :repo "technomancy/fennel-mode"))
(package! auto-highlight-symbol :disable t)
(package! highlight-symbol :disable t)
(package! org-msg :disable t)
(package! inf-clojure :disable t)
(package! olivetti)
(package! focus)
;; (package! doom-modeline :disable t)
(package! telega)
(package! 2048-game)
(package! anzu :disable t)
(package! evil-anzu :disable t)
(package! highlight-sexp :disable t :recipe (:host github :repo "daimrod/highlight-sexp"))
(package! highlight-parentheses :disable t)
(package! ivy-file-preview :disable t)
(package! browse-kill-ring)
(package! org-super-agenda)
(package! paredit)
(package! evil-paredit)
(package! ctrlf :disable t)
(package! ibuffer-vc)
(package! moody :disable t)
(package! almost-mono-themes)
(package! flycheck-popup-tip :disable t)
(package! tao-theme :disable t)
(package! wolfram)
(package! cyberpunk-theme)
;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
(unpin! doom-themes)
(unpin! sly)
