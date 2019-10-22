(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)

;;; remove SC if you are not using sunrise commander and org if you like outdated packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(setq explicit-shell-file-name "C:/msys64/usr/bin/bash.exe")
(setq shell-file-name explicit-shell-file-name)
(add-to-list 'exec-path "C:/msys64/usr/bin")

(setenv "PS1" "\\[\\e[32m\\]\\u@\\h \\[\\e[33m\\]\\w\\[\\e[0m\\]\\n\\$")
