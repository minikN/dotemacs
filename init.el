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

(defun cygwin-shell ()
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "~/bin/bash"))
    (call-interactively 'shell)))
