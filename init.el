(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode t)
(menu-bar-mode -1)
(setq visible-bell t)

(column-number-mode)

(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)

(dolist (mode '(term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Fira Code Retina" :height 110)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 110)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 110 :weight 'regular)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(when (file-exists-p "~/.emacs.d/secrets.el")
  (load-file (expand-file-name "~/.emacs.d/secrets.el")))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package which-key
  :diminish
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :custom
  (counsel-describe-function-function *'helpful-callable)
  (counsel-describe-variable-function *'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package diminish)

(use-package general
  :config
  (general-create-definer nb/leader-keys
    :keymaps '(normal visual emacs)
    :prefix "SPC"))

(nb/leader-keys
  "t" '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package undo-tree
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package ivy
  :diminish
  :init
  (ivy-mode 1)
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes)

(use-package all-the-icons)
(use-package doom-modeline
  :after all-the-icons
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(load-theme 'doom-dark+ t)

(use-package hydra)

(defhydra hydra-text-scale (:timeout 1)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(nb/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package org)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom ((projectile-completion-system 'ivy))
  :init
  (when (file-directory-p "~/../../projects")
    (setq projectile-project-search-path '("~/.emacs.d" ("~/../../projects" . 2))))
  (setq projectile-switch-project-action *'projectile-dired))

; TODO: Take a look at this. The `counsel-projectile-mode` command fails for some reason...
(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch))
  ;:custom
  ;(magit-display-buffer-function *'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

(use-package git-gutter+
  :config
  (global-git-gutter+-mode +1)
  (setq git-gutter+-hide-gutter t))

(use-package groovy-mode)
(use-package jenkinsfile-mode
    :mode "Jenkinsfile.*")
