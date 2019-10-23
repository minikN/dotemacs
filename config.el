(defvar TERM_SHELL "/bin/bash")

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package monokai-pro-theme
  :ensure t
  :config
  (load-theme 'monokai-pro t))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))

(global-set-key (kbd "<s-return>") 'ansi-term)

(global-set-key (kbd "C-c e") 'config-edit)
(global-set-key (kbd "C-c r") 'config-reload)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x 2") 'split-and-focus-h)
(global-set-key (kbd "C-x 3") 'split-and-focus-v)

(defun config-edit ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))

(defun config-reload ()
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(defun split-and-focus-h ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-focus-v ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)

(setq-default left-margin-width 1 right-margin-width 1)
(set-window-buffer nil (current-buffer))

(set-face-background 'fringe "transparent")

(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :font "Inconsolata-11")
)

(setq scroll-conservatively 100)

(setq ring-bell-function 'ignore)

(global-hl-line-mode t)

(setq-default tab-always-indent nil)

(global-prettify-symbols-mode t)

(setq electric-pair-pairs '(
			    (?\( . ?\))
			    (?\[ . ?\])
			    (?\{ . ?\})
			    (?\" . ?\")
			    (?\' . ?\')
			    ))
(electric-pair-mode 1)

(defadvice ansi-term (before force-bash)
  (interactive (list TERM_SHELL)))
(ad-activate 'ansi-term)

(setq make-backup-file nil)
(setq auto-save-default nil)

(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode 1))
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex))

(setq ibuffer-expert t)

(setq org-src-window-setup 'current-window)
