(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;; remove SC if you are not using sunrise commander and org if you like outdated packages
;(setq package-archives '(("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents))

(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

;; Font
(add-to-list 'default-frame-alist '(font . "Inconsolata-12" ))
(set-face-attribute 'default t :font "Inconsolata-12")

(setenv "PS1" "\\[\\e[33m\\]\\w\\[\\e[0m\\] \\> ")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#ff6188" "#fcfcfa"])
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#ff6188" "#fcfcfa"])
 '(custom-safe-themes
   (quote
    ("6b64e8f0c59cf8a281ae520e3ca78372dc12002185d1b666fc28202284642728" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (org-pdfview pdf-tools auctex php-mode quelpa-use-package company-box expand-region mark-multiple swiper spacemacs-theme company popup-kill-ring doom-modeline monokai-pro-classic-theme rainbow-delimiters sudo-edit hungry-delete smex ido-vertical-mode avy org-bullets which-key monokai-pro-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
