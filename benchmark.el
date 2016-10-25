  (eval-and-compile
    (prefer-coding-system 'utf-8-unix)
    (setq buffer-file-coding-system 'utf-8-unix
          default-file-name-coding-system 'utf-8-unix
          utf-translate-cjk-mode nil ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
          locale-coding-system 'utf-8-unix)
    (set-terminal-coding-system 'utf-8-unix)
    (set-keyboard-coding-system 'utf-8-unix)
    (set-language-environment 'utf-8)
    (set-default-coding-systems 'utf-8-unix)
    (unless (eq system-type 'windows-nt)
      (set-selection-coding-system 'utf-8-unix)))

  (eval-and-compile
    (require 'package)
    (setq package-enable-at-startup nil
          package-archives
          '(("gnu" . "http://elpa.gnu.org/packages/")
            ("melpa" . "http://melpa.milkbox.net/packages/")
            ("elpy" . "https://jorgenschaefer.github.io/packages/")
            ("org" . "http://orgmode.org/elpa/"))
          load-prefer-newer t))

  (eval-and-compile
    (unless (package-installed-p 'req-package)
      (package-refresh-contents)
      (package-install 'req-package))
    (require 'cl-lib)
    (require 'req-package)
    (require 'bind-key))

  (eval-and-compile
    (req-package
     my-util
     :force t
     :functions my-util-ensureEmacsDir my-util-cb? my-util-win? my-util-linux?
     :load-path "els/my-utils/"))

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)
(diminish 'my-keys-minor-mode)

(bind-keys :map my-keys-minor-mode-map
	   ("C-ö" . windmove-left)
	   ("C-#" . windmove-right)
           ("C-ü" . windmove-up)
           ("C-ä" . windmove-down)
           ("C-Ü" . clone-indirect-buffer))

(defun delete-other-window ()
  (interactive)
  (delete-window (other-window 1)))

(bind-key "C-q" 'delete-other-window my-keys-minor-mode-map)

(bind-key "C-x C-M-f" 'ido-find-file-other-window my-keys-minor-mode-map)

(define-key global-map [(insert)] nil)

(bind-keys :map my-keys-minor-mode-map
	   ("C-M-k" . kill-this-buffer)
	   ("C-l" . goto-line))
;;in global keymap because conflict with org-mode new heading
(bind-keys ("C-<return>" . new-line-below)
	   ("M-<return>" . new-line-above))

(setq default-directory "~/")
(defvar gdrive-dir (concat default-directory "cloud/"))
(defvar meinAll-dir (concat gdrive-dir "dokumente/meinAll/"))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(my-util-ensureEmacsDir "config/")
(my-util-ensureEmacsDir "save/autosave")
(my-util-ensureEmacsDir "save/backup")

(setq delete-old-versions t
  kept-new-versions 6
  create-lockfiles nil
  kept-old-versions 2
  version-control t
  backup-directory-alist '((".*" . "~/.emacs.d/save/backup/")))

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook (lambda () (kill-buffer "*scratch*")))

(tooltip-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)
(delete-selection-mode 1)
(setq backup-inhibited 1)
;; http://www.wisdomandwonder.com/wordpress/wp-content/uploads/2014/03/C3F.html#sec-10-2-3

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

(setq display-buffer-alist
      '(("*Async Shell Command*" . (display-buffer-no-window))))

(menu-bar-mode -1) 
(tool-bar-mode -1)
(if (file-exists-p "/etc/crouton/name")
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(width . 190))
  (add-to-list 'default-frame-alist '(height . 60)))
  (if (daemonp)
    (add-hook 'after-make-frame-functions
        (lambda (frame)
            (select-frame frame)
            (set-frame-parameter nil 'internal-border-width 4)
	    (set-face-attribute 'fringe nil :background "#bisque4")
	    (fringe-mode '(1 . 0))
	    (set-face-attribute 'vertical-border nil :foreground "bisque4")))
    (progn
      (set-frame-parameter nil 'internal-border-width 4)
      (set-face-attribute 'fringe nil :background "#242424")
      (fringe-mode '(1 . 0))
      (set-face-attribute 'vertical-border nil :foreground "bisque4"))
    )
(scroll-bar-mode -1)

(if (my-util-cb?)
    (add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono 15"))
    ;; (setq initial-frame-alist (font . "Fantasque Sans Mono"))
    (add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono")))

;; (set-face-attribute 'mode-line nil :height 135 :foreground "#28a428" :background "#2a2a28")
;; (set-face-attribute 'mode-line-inactive nil :height 135 :foreground "#995400" :background "#2a2a28")

(set-face-attribute 'region nil :background "darkblue")
(set-cursor-color "black")
(setq-default cursor-type 'bar)

(global-visual-line-mode t)
(diminish 'visual-line-mode)

(req-package adaptive-wrap
  :init
  (define-globalized-minor-mode adaptive-wrap-global-mode
  adaptive-wrap-prefix-mode
  adaptive-wrap-prefix-mode)
  :bind
  :config
  (adaptive-wrap-global-mode)
  )

(req-package soft-stone-theme
  :init
  (load-theme 'soft-stone t)
  :bind
  :config
  )

(req-package window-purpose
    :config
  (add-to-list 'purpose-user-mode-purposes '(rust-mode . rust))
  (add-to-list 'purpose-user-mode-purposes '(cargo-process-mode . cargo-process))
  (purpose-compile-user-configuration)
  )

(setq sentence-end-double-space nil)

(req-package drag-stuff
  :init
  :config
  (when (my-util-cb?)
    (bind-keys :map drag-stuff-mode-map
	       ("M-S-<prior>" . drag-stuff-up)
	       ("M-S-<next>" . drag-stuff-down)
	       ;; ("M-right" . drag-stuff-right)
	       ;; ("M-right" . drag-stuff-left))
	       ))
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-global-mode)
  :diminish drag-stuff-mode
  )

(req-package buffer-move
  :bind (:map my-keys-minor-mode-map
	      ("C-M-#" . buf-move-right)
	      ("C-M-ö" . buf-move-left)
              ("C-M-ü" . buf-move-up)
              ("C-M-ä" . buf-move-down))
  )

(req-package ag
    :require wgrep-ag
    :config
    (push "~/.agignore" ag-arguments)
    (push "--path-to-agignore" ag-arguments))

(req-package evil-nerd-commenter
  :init
  :config
  (evilnc-default-hotkeys)
)

(req-package multiple-cursors
  :init
  :bind (:map my-keys-minor-mode-map ("C-<down-mouse-1>" . mc/add-cursor-on-click))
  :config
  (setq mc/list-file (concat user-emacs-directory "config/.mc-lists.el"))
  ;;'(mc/cursor-face ((nil (:background "orange"))))
)

(req-package google-translate
  :init
  (require 'google-translate-smooth-ui)
  :bind (("C-c t" . google-translate-smooth-translate))
  :config
  (setq google-translate-translation-directions-alist
	'(("de" . "en") ("en" . "de") ("de" . "fr") ("de" . "es")))
  (setq google-translate-pop-up-buffer-set-focus t)
)

(req-package outshine
    :init
  ;; because somehow it stopped being activated
  (require 'outshine)
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  ;; (add-hook 'python-mode-hook 'outline-minor-mode)
  :bind
  :config
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  )
(req-package navi-mode
  :init
  :bind
  :config
  )

(req-package company
:config 
(add-hook 'after-init-hook 'global-company-mode)
(define-key company-active-map (kbd "C-ä") 'company-select-next)
(define-key company-active-map (kbd "C-ü") 'company-select-previous)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 1
      company-tooltip-align-annotations t
      company-dabbrev-downcase nil))

(req-package centered-window-mode
  :init
  ;; Makes left fringe 10px or so smaller than right one in cwm 
  (defun cwm/center ()
    (set-fringe-mode
     (let ((right_fringe
	    (/ (- (frame-pixel-width)
		  (* 110 (frame-char-width)))
	       2)))
       (cons (- right_fringe 50) right_fringe))
     ))
  :config
  (centered-window-mode t)
  :diminish centered-window-mode
  )

(req-package smartparens
  :bind (:map smartparens-mode-map
	      ("C-M-<left>" . sp-backward-sexp)
	      ("C-M-<right>" . sp-forward-sexp)
	      ("C-S-<backspace>" . sp-backward-kill-sexp)
	      ("C-M-<down>" . sp-select-next-thing))
  :init
  (setq blink-matching-paren nil)
  (require 'smartparens-config)
  (set-face-attribute 'sp-show-pair-match-face nil :foreground "green" :background nil)
  (set-face-attribute 'sp-show-pair-mismatch-face nil :foreground "red" :background nil)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  :diminish smartparens-mode
  )

(req-package undo-tree
  :bind (("C-p" . undo-tree-undo)
         ("M-p" . undo-tree-redo)
         ("C-M-p" . undo-tree-visualize))
  :init
  :config
  (global-undo-tree-mode t)
  :diminish undo-tree-mode
)

;; (setq diff-command "ediff")
;; (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
(custom-set-variables
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(ediff-split-window-function 'split-window-horizontally)
 )

;; diff hooks for org mode
(add-hook 'ediff-select-hook 'f-ediff-org-unfold-tree-element)
(add-hook 'ediff-unselect-hook 'f-ediff-org-fold-tree)
;; Check for org mode and existence of buffer
(defun f-ediff-org-showhide(buf command &rest cmdargs)
  "If buffer exists and is orgmode then execute command"
  (if buf
      (if (eq (buffer-local-value 'major-mode (get-buffer buf)) 'org-mode)
	  (with-current-buffer (apply command cmdargs)))
    )
  )

(defun f-ediff-org-unfold-tree-element ()
  "Unfold tree at diff location"
  (f-ediff-org-showhide ediff-buffer-A 'org-reveal)  
  (f-ediff-org-showhide ediff-buffer-B 'org-reveal)  
  (f-ediff-org-showhide ediff-buffer-C 'org-reveal)  
  )
;;
(defun f-ediff-org-fold-tree ()
  "Fold tree back to top level"
  (f-ediff-org-showhide ediff-buffer-A 'hide-sublevels 1)  
  (f-ediff-org-showhide ediff-buffer-B 'hide-sublevels 1)  
  (f-ediff-org-showhide ediff-buffer-C 'hide-sublevels 1)  
  )

(req-package expand-region
  :bind (:map my-keys-minor-mode-map
	      ("C-M-w" . er/expand-region)
	      ("C-M-q" . er/contract-region))
  :init
  :config
  (er/enable-mode-expansions 'web-mode 'er/add-js-mode-expansions)
)

(add-hook 'org-src-mode-hook
	  (lambda () (setq-local
		      flycheck-disabled-checkers
		      '(emacs-lisp-checkdoc))))

(req-package ido
:init
:bind
:config
(ido-mode t)
(ido-everywhere t)
(bind-keys ("M-#" . ido-switch-buffer)
		 ("M-ö" . my/switch-to-previous-buffer))
(bind-keys :map ido-common-completion-map
            ("M-#" . ido-next-match)
            ("M-ö" . ido-prev-match)))

(setq ido-case-fold t
      ido-enable-flex-matching t
      ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
			   "*Messages*" "Async Shell Command"))

;; ;; If a buffer name that doesn't exist is chosen, just make a new one without prompting
;; (setq ido-create-new-buffer 'always)

;; Ignore the .aux extensions that TeX programs create 
(setq completion-ignored-extensions 
      (cons "*.aux" completion-ignored-extensions))


;;; Ignore files defined in variable completion-ignored-extensions 
(setq ido-ignore-extensions t) 

;;; Order extensions by how I use them 
(setq ido-file-extensions-order '(".tex"  ".txt" ".py" ".sh" ".el" ".xml" ".htm"))

;;; Keep annoying buffers out of my face 
(setq ido-ignore-buffers (list (rx (or (and bos  " ") 
                                       (and bos 
                                            (or "*Completions*" 
                                                "*Shell Command Output*" 
                                                "*vc-diff*") 
                                            eos))))) 

;;(add-to-list 'ido-ignore-files "\\`media/")

(req-package flx-ido
:init
(flx-ido-mode 1)
:bind
:config
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
:ensure t)

(req-package smex
:bind ("M-x" . smex)
:init
:config
:ensure t
)

(req-package spaceline
  :init
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  :bind
  :config
  (spaceline-toggle-buffer-size-off)
  )

;;asdsd
(req-package projectile
  :init
  (add-hook 'python-mode-hook 'projectile-mode)
  :bind
  :config
  (setq projectile-indexing-method 'alien)
  )

(req-package floobits
:init
:config
:ensure t
)

(req-package aggressive-indent
  :init
  (setq-default indent-tabs-mode nil)
  (global-aggressive-indent-mode 1)
  :bind
  :config)

(req-package which-key :init (which-key-mode))

(req-package dumb-jump)

(setenv "SSH_ASKPASS" "git-gui--askpass")
(req-package ssh-agency
  :if (my-util-win?)
  )
(req-package magit
:ensure t
)

(req-package git-timemachine)

(req-package org-plus-contrib
  :init
  (require 'org)
  ;; (require 'org-drill)
  (require 'org-checklist)
  (add-to-list 'org-modules 'org-checklist)
  :bind
  :config
  (setq org-default-notes-file (concat meinAll-dir "milkyway.org"))
  :diminish org-indent-mode)
  
  (req-package my-org
  :force t
  :bind (:map org-mode-map
	      ("C-c C-M-e" . my-org-export-all))
  :load-path "els/my-org/")

(setq org-startup-indented t
      org-blank-before-new-entry '((heading . nil)
				  (plain-list-item . nil))
      org-return-follows-link nil
      org-completion-use-ido t
      org-image-actual-width '(500)
      org-list-allow-alphabetical t
      org-use-property-inheritance t
      org-use-sub-superscripts nil
      org-checkbox-hierarchical-statistics t
      magit-diff-arguments (quote ("--ignore-space-change"
      "--ignore-all-space" "--no-ext-diff" "--stat")))
(bind-keys ("C-c l" 'org-store-link)
	   ("C-c a" 'org-agenda)
	   ("C-c b" 'org-iswitchb))

(setq org-export-with-toc nil
      org-export-with-section-numbers nil)

(setq org-refile-use-outline-path t
      org-datetree-add-timestamp 1
      org-extend-today-until 6
      org-outline-path-complete-in-steps nil
      org-hide-emphasis-markers t
      org-time-stamp-custom-formats '("<%e. %B '%y>" . "<%b %e, %Y %H:%M>")
      org-refile-targets '((nil :level . 2)))
(setq-default org-display-custom-times t)
(bind-key "C-c c" 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (
	      (" " "Atrium" entry (file+headline (concat org-directory "Milkyway.org") "Atrium") "* %^{Headline} %^ü %?")
	      
              ("d" "Diary")
              ("d " "Thought" entry (file+datetree (concat org-directory "monument/Tagebuch.gpg")) "* %(format-time-string \"[%H:%M]\") Je pense\n %?" :kill-buffer t)
              ("dt" "Tag" entry (file+datetree (concat org-directory "monument/Tagebuch.gpg")) "* M'aujourd'hui\n** Quelque gut :)\n %?\n** Quelque done\n" :kill-buffer t)
              
              ("k" "Knowledge")
	      ("k " "Atrium" entry (file+headline (concat org-directory "lookingGlass/knowledge.org") "Atrium") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t)
	      ("km" "Math" entry (file+headline (concat org-directory "lookingGlass/knowledge.org") "Math") "*** %?")
	      ("kp" "Physics" entry (file+headline (concat org-directory "lookingGlass/knowledge.org") "Physics") "* %?")
	      ("kl" "Language" entry (file+headline (concat org-directory "lookingGlass/knowledge.org") "Language") "* %?")
	      
	      ("p" "Programming")
	      ("p " "Atrium" entry (file+headline (concat org-directory "lookingGlass/programming.org") "Atrium") "* %?")

	      ("pp" "Practice" entry (file+headline (concat org-directory "lookingGlass/programming.org") "Practice") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t)
	      
	      ("pw" "Webdev")
	      ("pw " "Atrium" entry (file+olp (concat org-directory "lookingGlass/programming.org") "Webdev" "Atrium") "* %?")

	      ("pwf" "Frontend")
	      ("pwfd" "Design" entry (file+headline (concat org-directory "lookingGlass/programming.org") "Design") "* %?")
	      ("pwff" "Function" entry (file+headline (concat org-directory "lookingGlass/programming.org") "Function") "* %?")
	      ("pwfi" "Inspiration" entry (file+headline (concat org-directory "lookingGlass/programming.org") "Inspiration") "* %?")
	      ("pwb" "Backend" entry (file+headline (concat org-directory "lookingGlass/programming.org") "Backend") "* %?")
	      ("pl" "Linux" entry (file+olp (concat org-directory "lookingGlass/programming.org") "Linux" "Atrium") "* %?")

	      ("pe" "Emacs")
	      ("pe " "Atrium" entry (file+olp (concat org-directory "lookingGlass/programming.org") "Emacs" "Atrium") "* %?")
	      ("peo" "Org-mode" entry (file+headline (concat org-directory "lookingGlass/programming.org") "Org-mode") "* %?")

	      
	      ("m" "Media")
	      ("m " "Atrium" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Atrium") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t :kill-buffer t)
	      
	      ("mw" "Watch")

	      ("mwm" "Movies")
	      ("mwm " "To watch" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Watch" "Movies" "To Watch") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t :kill-buffer t)
	      ("mwmw" "Watched" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Watch" "Movies" "Watched") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t :kill-buffer t)
	      ("mwms" "Sources" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Watch" "Movies" "Sources") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t :kill-buffer t)
	      ("mws" "Serien" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Watch" "Serien") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t :kill-buffer t)

	      ("mwv" "Video")
	      ("mwve" "Entertainment" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Watch" "Videos" "Entertainment") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t :kill-buffer t)
	      ("mwvl" "Learn" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Watch" "Videos" "Learn") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t :kill-buffer t)
	      
	      ("mm" "Music")
	      ("mm " "Song" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Music" "Songs") "* [[%^{Url}][%^{Interpret} - %^{Titel}]]" :immediate-finish t :kill-buffer t)
	      ("mma" "Audiobook" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Music" "Audiobooks") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t :kill-buffer t)
	      ("mmp" "Playlist" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Music" "Playlists") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t :kill-buffer t)

	      ("mr" "Reading")
	      ("mr " "Book" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Reading" "Books" "To Read") "* %? %^g")
	      ("mrp" "Poem" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Reading" "Poems") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t :kill-buffer t)
	      ("mrs" "Story" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Reading" "Stories") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t :kill-buffer t)
	      ("mrw" "Wikipedia" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Reading" "Wikipedia") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t :kill-buffer t)
	      ("mre" "Et Aliae" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Reading" "Et Aliae") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t :kill-buffer t)
	      ("mrr" "Reddit" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Reading" "Reddit") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t :kill-buffer t)
	      ("mrb" "Blog/Subreddit" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Media" "Reading" "Blogs/Subreddits") "* [[%^{Url}][%^{Titel}]]" :immediate-finish t :kill-buffer t)

	      
	      ("mt" "Toutesuit" entry (file+olp (concat org-directory "lookingGlass/toutesuit.org") "Videos") "* [[%^{Url}][%^{Titel}]] %^g" :kill-buffer t :immediate-finish t)

	      
	      ("r" "Resources")

	      ("rc" "Cuisine")
	      ("rc " "Atrium" entry (file+olp (concat org-directory "lookingGlass/cuisine.org") "Atrium") "* [[%^{Url}][%^{Titel}]]" :kill-buffer t :immediate-finish t)
	      ("rci" "Ingredients" entry (file+olp (concat org-directory "lookingGlass/cuisine.org") "Ingredients") "* [[%^{Url}][%^{Titel}]] %^g" :kill-buffer t :immediate-finish t)
	      
	      ("rcr" "Recipes")
	      ("rcra" "Appetizer" entry (file+olp (concat org-directory "lookingGlass/cuisine.org") "Appetizer") "* [[%^{Url}][%^{Titel}]] %^g" :kill-buffer t :immediate-finish t)
	      ("rcrb" "Breakfast" entry (file+olp (concat org-directory "lookingGlass/cuisine.org") "Breakfast") "* [[%^{Url}][%^{Titel}]] %^g" :kill-buffer t :immediate-finish t)
	      ("rcrd" "Dinner" entry (file+olp (concat org-directory "lookingGlass/cuisine.org") "Dinner") "* [[%^{Url}][%^{Titel}]] %^g" :kill-buffer t :immediate-finish t)
	      ("rcrs" "Salat" entry (file+olp (concat org-directory "lookingGlass/cuisine.org") "Salat") "* [[%^{Url}][%^{Titel}]] %^g" :kill-buffer t :immediate-finish t)

	      
	      ("rs" "Sport")
	      ("rs " "Atrium" entry (file+olp (concat org-directory "lookingGlass/sport.org") "Atrium") "* [[%^{Url}][%^{Titel}]]" :kill-buffer t :immediate-finish t)

	      
	      ("t" "Todo")
	      ("te" "Easy" entry (file+headline (concat org-directory "Milkyway.org") "Easy")
	       "* TODO %?\n :PROPERTIES:\n :CURRENCY_DELTAS: ((xp +5) (light +1)(credit +10))\n :END:")
	      ("tm" "Medium" entry (file+headline (concat org-directory "Milkyway.org") "Medium")
	       "* TODO %?\n :PROPERTIES:\n :CURRENCY_DELTAS: ((xp +12) (light +2)(credit +25))\n :END:")
	      ("th" "Hard" entry (file+headline (concat org-directory "Milkyway.org") "Hard")
	       "* TODO %?\n :PROPERTIES:\n :CURRENCY_DELTAS: ((xp +20) (light +5)(credit +50))\n :END:")

	      ("tl" "Learn" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Toskana Durota" "Learn")
	       "* %? %^g" :kill-buffer t)
	      ("ts" "Someday" entry (file+olp (concat org-directory "lookingGlass/moise.org") "Toskana Durota" "Someday")
	       "* %? %^g" :kill-buffer t)
	      )))

(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("note" . ?n)("exercise" . nil)
		      (:startgroup . nil)
		      ("sport" . nil)
		      (:grouptags . nil)
		      ("sport_yoga" . nil)
		      ("sport_weight" . nil)
		      (:endgroup . nil)
		      (:startgroup . nil)
		      ("read" . nil)
		      (:grouptags . nil)
		      ("read_book" . nil)
		      ("read_ebook" . nil)
		      (:endgroup . nil)))

;; TODO change minibuffer prompt while read-from-minibuffer to display Url: or File: in minibuffer prompt depending on what is inserted
;; TODO maybe change stevinho.justnetwork.eu from @justnetwork.eu to @stevinho.eu
;; replace www. and use first and last (idea)
(defun my/insert-link ()
  (interactive)
  (let* ((keymap (copy-keymap minibuffer-local-map))
	 (get-stored-link
	  '(lambda ()
	     (setq url (caar org-stored-links))
	    (if url
		(concat "::" (car (last (split-string (nth 1 (split-string url "[\\:]")) "[\\/]"))))
	      nil)))
	 (get-url-link
	  '(lambda ()
	     (setq url (org-get-x-clipboard 'CLIPBOARD))
	     (if (string= (substring url 0 4) "http")
		 (let* ((urlParts
			 (last (split-string (nth 2 (split-string url "[\\/]")) "[\\.]") 2)))
		   (concat "@" (nth 0 urlParts) "." (nth 1 urlParts)))
	       nil
	       )))
	 url urlDescription)
    
    (define-key keymap (kbd "<tab>")
      (lambda () (interactive)
	(let (link message)
	  (if (string= "@" (substring (minibuffer-contents) 0 1))
	      (setq link (funcall get-stored-link)
		    message "No link stored")
	    (setq link (funcall get-url-link)
		  message "No Url in Clipboard"))
	  (if link (progn
		     (delete-minibuffer-contents)
		     (insert link))
	    (minibuffer-message message))
	  )))
    
    (define-key keymap (kbd "C-g")
      (lambda () (interactive)
	(delete-minibuffer-contents)
	(exit-minibuffer)
	))
    (setq urlDescription
	  (or (funcall get-url-link) (funcall get-stored-link)))
    
    (if urlDescription
	(progn
	  (setq urlDescription (read-from-minibuffer "Link" urlDescription keymap))
	  (if (string= "" urlDescription)
	      (minibuffer-message "Aborted")
	    (insert (format "[[%s][%s]]" url urlDescription))))
      (minibuffer-message "No Link to insert. Aborted"))
    ))

(when (my-util-win?)
  (setq org-babel-sh-command "C:/cygwin64/bin/bash.exe"))

(setq org-src-fontify-natively t
      org-pretty-entities t
      org-src-preserve-indentation t
      org-src-window-setup 'current-window
      org-edit-src-auto-save-idle-delay 60)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((latex . t)
   (python . t)
   (gnuplot . t)
   (sh . t)
   (sql . t)))

(req-package org-passwords
    :load-path "els/org-passwords/"
    :config (setq org-passwords-file (expand-file-name meinAll-dir
                                                       "monument/lesMysteres.gpg")))
;; http://barrenfrozenwasteland.com/2015/06/configuring-pass-on-windows/
(req-package pass
    :config )

(setenv "GPG_AGENT_INFO" nil)
(require 'epa-file)
(setq epa-file-select-keys nil)

;;(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(setq org-latex-preview-ltxpng-directory (concat temporary-file-directory "ltxpng/"))

(custom-set-faces
 `(org-level-4 ((t (:foreground "darkorange"))))
 `(org-level-2 ((t (:foreground "cadet blue"))))
 `(org-level-3 ((t (:foreground "#b75761"))))
 `(org-property-value ((t (:foreground "purple"))))
 `(org-special-keyword ((t (:foreground "#990099"))))
 `(org-link ((t (:foreground "bisque4"))))
 ;; weird issue with line-wrap, wrapped lines (the indent) don't get this face
 ;; `(org-block-background ((t (:background "#133436"))))
 ;; Underline/overline is weird
 ;; `(org-block-begin-line ((t (:foreground ,"#446a5d" :underline ,"#b3e"))))
 ;; `(org-block-end-line ((t (:foreground ,"#446a5d" :overline  ,"#b3e"))))
 `(org-block-begin-line ((t (:foreground ,"#446a5d"))))
 `(org-block-end-line ((t (:foreground ,"#446a5d"))))
 )

(defun org-sentence-newline()
  (interactive)
  (org-backward-sentence)
  (org-delete-backward-char 1)
  (org-return-indent))
(defun my/org-delete-heading-or-line ()
  (interactive)
  (if (org-at-heading-p)
      (org-cut-subtree)
    (kill-line)))

(bind-keys :map org-mode-map
("<return>" . org-return-indent)
("M-S-<delete>" . my/org-delete-heading-or-line)
("C-M-<left>" . org-backward-sentence)
("C-M-<right>" . org-forward-sentence)
("C-M-<end>" . org-sentence-newline)
("C-c l" . my/insert-link))

(when (my-util-cb?)
    (bind-keys :map org-mode-map
	       ("M-S-<prior>" . org-shiftmetaup)
	       ("M-S-<next>" . org-shiftmetadown)
	       ;; ("M-right" . drag-stuff-right)
	       ;; ("M-right" . drag-stuff-left)
	       ))

(defun new-movie(title)
  "Add a new movie to "
  (interactive "sTitel: ")
  (let ((headline (if (y-or-n-p "Have you seen it already?")
		      "Watched"
		    "To Watch")))
    headline
    ))

(req-package tex
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  :config
  :ensure auctex
)

(req-package neotree
  :init
  :config
)

(req-package dired+
  :init
  (toggle-diredp-find-file-reuse-dir 1)
  :bind
  :config
  (bind-key "?" 'my/dired-get-size dired-mode-map)
  (setq dired-listing-switches "-aDhl  --group-directories-first")
)

(defun my/dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn 
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
		 (match-string 1))))))

(setq ispell-program-name "C:\\cygwin64\\bin\\aspell.exe"
      ispell-really-aspell t
      ispell-extra-args '("--sug-mode=fast")
      ;; TODO name deutsch+english
      ispell-dictionary "deutsch"
      flyspell-issue-message-flag nil)

(req-package elixir-mode
    :init
  :config
  )

(req-package alchemist
    :require elixir-mode
    :init
    :config
    :bind (:map alchemist-mode-map
                ("C-c C-c" . alchemist-eval-buffer)) 
    )

(req-package markdown-mode
  :init
  :config
)

(req-package elpy
    :init
  (elpy-enable)
  :bind
  :config
  ;; https://github.com/jorgenschaefer/elpy/issues/887
  ;; probvably enable again, its new with emacs 25
  (setq python-shell-completion-native-enable nil)
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (if (executable-find "ipython")
      ;; (elpy-use-ipython)
      (message "'ipython' not found found; please install"))
  ;; Currently no debugging in elpy afaik
  ;; (setq elpy-test-pytest-runner-command '("py.test --pdb")) ;
  ;; (elpy-set-test-runner 'elpy-test-pytest-runner)
  (setq elpy-rpc-backend "rope"
        elpy-rpc-python-command "python")
  )

(req-package cl-generic
  :init
  :bind
  :config
  )
(req-package ein
  :init
  :bind
  :config
  )

(defun my/jump-to-test ()
  (interactive)
  
  (let* ((file-name
	  (nth 0 (last (split-string buffer-file-name "[\\/]"))))
	 (test-file
	  (s-join "/" (append (butlast (split-string buffer-file-name "[\\/]"))
			      (list (concat "test_" file-name)))))
	 (func-name "")
	 (func-args (progn
		      (unless (looking-at "def")
			(python-nav-beginning-of-defun))
		      (right-word)
		      (right-char)
		      (set-mark (point))
                      (while (not (looking-at "("))
			(sp-forward-sexp))
		      (setq func-name (buffer-substring-no-properties (mark) (point)))
		      (set-mark (point))
		      (sp-forward-sexp)
		      (buffer-substring-no-properties (mark) (point)))))
    (with-current-buffer (find-file test-file)
      (goto-char (point-min))
      (unless (search-forward-regexp "from .+ import \\*" nil t)
	(insert (concat "from "
			(replace-regexp-in-string "\\.py" "" "calc.py")
			" import *\n")))
      (let ((test-func (concat "test_" func-name)))
	(unless (search-forward test-func nil t)
	  (goto-char (point-max))
	  (insert "\n"
		  (format "def %s():\n" test-func)
		  (format "\tassert %s%s == " func-name func-args))))
      )))

(req-package htmlize
  :init
  :config
)

(req-package scss-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  :config
)

(req-package js2-mode
  :init
  (add-hook 'js-mode-hook 'js2-minor-mode)
  
  :config
  (setq js2-basic-offset 2)
  (setq js2-strict-inconsistent-return-warning nil)
)

(req-package web-mode
    :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (setq web-mode-content-types-alist
	'(("css" . "\\.\\(s?css\\|css\\.erb\\)\\'")
	  ("jsx" . "\\.\\([jt]s\\|[jt]s\\.erb\\)\\'")
	  ("json" . "\\.\\(api\\|json\\|jsonld\\)\\'")
	  ("jsx" . "\\.[jt]sx\\'")
	  ("xml" . "\\.xml\\'")
	  ("html" . ".")))
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-attr-value-indent-offset 2
        web-mode-css-indent-offset 2 
        web-mode-style-padding 1
        web-mode-script-padding 0 
        web-mode-block-padding 0
        web-mode-enable-control-block-indentation nil)
  )
  (req-package company-web )

(req-package coffee-mode
  :init
  :config
  (custom-set-variables '(coffee-tab-width 2))
)

(setq js-indent-level 2)
(req-package json-mode
:config (setq json-reformat:indent-width 2))

(setq lisp-indent-function 'common-lisp-indent-function)
(bind-key "C-h C-f" 'find-function-at-point emacs-lisp-mode-map)
(bind-key "C-h C-v" 'find-variable-at-point emacs-lisp-mode-map)

(req-package php-mode)

(req-package rust-mode)
(req-package company-racer
    :require company
    :config 
    (add-to-list 'company-backends 'company-racer)
    (add-hook 'racer-mode-hook #'company-mode))
(req-package racer
    :config
  (setq racer-cmd "~/.cargo/bin/racer.exe"
        racer-rust-src-path "C:/Program Files/Rust/source/src/")
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))
(req-package cargo
    :require rust-mode
    :init (add-hook 'rust-mode-hook 'cargo-minor-mode)
    :config
    (defun cargo-process--cleanup (buffer)
      (when (get-buffer-process (get-buffer buffer))
        (delete-process buffer)))
    (defvar cargo-process-history '())
    (defun cargo-process-run ()
      "Run the Cargo run command.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute src/main.rs."
      (interactive) 
      (cargo-process--start
       "Run"
       (read-string "Command: " "cargo run" '(cargo-process-history . 0))))

    (defun cargo-process-test (enable_print)
      "Run the Cargo test command.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests."
      (interactive "P")
      (let* ((command "cargo test"))
        (when enable_print
          (concat command " -- --nocapture"))
        (cargo-process--start "Test" "cargo test"))
      )
    )
(req-package flycheck-rust
    :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq same-window-buffer-names '("*Help*"))

(req-package hungry-delete
    :init (global-hungry-delete-mode)
    :config
    )

(req-package spell-number
             :load-path "els/spell-number/")

(req-package exercism
    :load-path "els/exercism/"
    :if (my-util-installed? "exercism")
    :config (when (my-util-win?)
	      (setq *exercism-cmd*
		    (shell-quote-argument "C:\\\\Program Files\\\\Exercism\\\\exercism.exe"))))

(req-package nameless
  :init
  :config
  (setq nameless-private-prefix t)
)

(req-package xtest
  :init
  :config
)

(req-package sql-indent
  :init
  :config
)

(req-package bonjournal
             :load-path "els/bonjournal/"
             :config (setq bonjournal-dir (expand-file-name meinAll-dir
                                                            "monument/bonjournal/"))
             )

(req-package toutesuit
             :load-path "els/toutesuit/"
             :config (setq toutesuit-file (expand-file-name
                                           meinAll-dir "lookingGlass/toutesuit.org")))

(when (my-util-win?)
  (setenv "PATH" (concat "c:/cygwin64/bin;" (getenv "PATH")))
  (setq exec-path (cons "c:/cygwin64/bin/" exec-path))
  (req-package cygwin-mount
      :load-path "els/cygwin/"
      :config (cygwin-mount-activate)
      ))

(req-package-finish)

(defun my/html-to-react ()
  (interactive)
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (while (search-forward "class" nil t) (replace-match "className" nil t))
    (goto-char (point-min))
    (while (search-forward "\"" nil t) (replace-match "'" nil t))
    (web-mode)
    (goto-char (point-min))
    (let* ((html (buffer-string))
	   (toc (my/html-to-react--get-toc html))
	   (content (my/html-to-react--get-content html)))
      (message content)
      (delete-region (point-min) (point-max))
      (insert (format "import React from 'react'

export class Toc extends React.Component {
  render () {
    return (
      %s
    )
  }
}

export class Content extends React.Component {
  render () {
    return (
      <div>%s</div>
    )
  }
}"
		   toc content))))
  )

(defun my/html-to-react--get-toc (html)
  (buffer-substring-no-properties (point-min)
		    (+ (web-mode-element-end-position) 1)))

(defun my/html-to-react--get-content (html)
  (buffer-substring-no-properties (+ (web-mode-element-end-position) 1)
				  (point-max)))

(defun new-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun new-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When `universal-argument' is called first, copy whole buffer (but respect `narrow-to-region')."
  (interactive)
  (let (p1 p2)
    (if (null current-prefix-arg)
        (progn (if (use-region-p)
                   (progn (setq p1 (region-beginning))
                          (setq p2 (region-end)))
                 (progn (setq p1 (line-beginning-position))
                        (setq p2 (line-end-position)))))
      (progn (setq p1 (point-min))
             (setq p2 (point-max))))
    (kill-ring-save p1 p2)))

(bind-key "M-w" 'xah-copy-line-or-region)

(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (but respect `narrow-to-region')."
  (interactive)
  (let (p1 p2)
    (if (null current-prefix-arg)
        (progn (if (use-region-p)
                   (progn (setq p1 (region-beginning))
                          (setq p2 (region-end)))
                 (progn (setq p1 (line-beginning-position))
                        (setq p2 (line-beginning-position 2)))))
      (progn (setq p1 (point-min))
             (setq p2 (point-max))))
    (kill-region p1 p2)))
    
(bind-key "C-w" 'xah-cut-line-or-region)

(defun my/delete-whitespace-or-word ()
  (interactive)
  (if (looking-at "\\(\t\\|  \\)")
      (delete-horizontal-space)
    (delete-word)))
    
(defun my/backward-delete-whitespace-or-word ()
  (interactive)
  (if (looking-back "\\(\t\\|  \\)")
      (delete-horizontal-space)
    (backward-delete-word)))

(bind-key "C-<backspace>" 'my/backward-delete-whitespace-or-word)
(bind-key "C-M-<backspace>" 'my/delete-whitespace-or-word)

;; because back-to-indentation doesn't take me back to visual line
(defun my/back-to-indentation ()
  (interactive)
  (beginning-of-visual-line)
  (indent-for-tab-command))

(bind-key "C-a" 'my/back-to-indentation)

;; maybe kill is actually okay
(defun delete-line-no-kill ()
  (interactive)
  (delete-region
   (line-end-position 0)
   (line-end-position))
  (indent-for-tab-command))

(defun backward-delete-word()
  (interactive)
  (delete-region (point) (progn (backward-word) (point))))

(defun delete-word()
  (interactive)
  (delete-region (point) (progn (forward-word) (point))))

(defun my/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun find-file-right()
  (interactive)
  (split-window-right)
  (ido-find-file-other-window))

(defun space-right()
  (interactive)
  (insert-char 32)
  (left-char))

(defvar xah-switch-buffer-ignore-dired t)
(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
 `user buffer' is a buffer whose name does not start with `*'.
If `xah-switch-buffer-ignore-dired' is true, also skip directory buffer.
2015-01-05 URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (or
           (string-equal "*" (substring (buffer-name) 0 1))
           (if (string-equal major-mode "dired-mode")
               xah-switch-buffer-ignore-dired
             nil
             ))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))


(defun xah-next-user-buffer ()
 "Switch to the next user buffer.
 `user buffer' is a buffer whose name does not start with `*'.
If `xah-switch-buffer-ignore-dired' is true, also skip directory buffer.
2015-01-05 URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (or
           (string-equal "*" (substring (buffer-name) 0 1))
           (if (string-equal major-mode "dired-mode")
               xah-switch-buffer-ignore-dired
             nil
             ))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun goto-code()
(interactive)
  (dired "~/Documents/code")
  )

(if (my-util-linux?)
    (defun sudo-save ()
      (interactive)
      (if (not buffer-file-name)
	  (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
	(write-file (concat "/sudo:root@localhost:" buffer-file-name)))))

(defun my/capitalize-previous-word()
  (interactive)
  (capitalize-word -1))
  (bind-key "M-c" 'my/capitalize-previous-word org-mode-map)

(when (my-util-win?)
  (server-start))
