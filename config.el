(defvar TERM_SHELL "/bin/bash")

(defalias 'yes-or-no-p 'y-or-n-p)

(unless (package-installed-p 'spacemacs-theme)
  (package-refresh-contents)
  (package-install 'spacemacs-theme))

(custom-set-variables '(spacemacs-theme-custom-colors
			'((base . "#00ff00"))))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode))

(if (eq system-type 'gnu/linux)
(use-package sudo-edit
  :ensure t
  :bind ("C-c s" . sudo-edit))
)

(use-package rainbow-delimiters
  :ensure t
  :init
  (rainbow-delimiters-mode 1))

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

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))
(use-package all-the-icons
  :ensure t)

(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . 'popup-kill-ring))

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

(if (eq system-type 'windows-nt)
    (defun toggle-full-screen () (interactive) (shell-command "emacs_fullscreen.exe"))
    (global-set-key [f11] 'toggle-full-screen)
)

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-c k a") 'kill-all-buffers)

(defun copy-whole-line ()
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))
(global-set-key (kbd "C-c y y") 'copy-whole-line)

(defun kill-whole-word ()
  (interactive)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c i w") 'kill-whole-word)

(defun kill-current-line (&optional n)
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (let ((kill-whole-line t))
      (kill-line n))))
(global-set-key (kbd "C-c d d") 'kill-current-line)

(defun new-line-above ()
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "C-c o") 'new-line-above)

(defun new-line-below ()
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))
(global-set-key (kbd "C-c O") 'new-line-below)

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

(line-number-mode 1)
(column-number-mode 1)

(setq make-backup-file nil)
(setq auto-save-default nil)

;; Currently disabled
;; Open temporary buffer in same window'C'
; (setq org-src-window-setup 'current-window)

(add-to-list 'org-structure-template-alist
	     '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
