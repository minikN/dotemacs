(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'monokai-pro-theme)
  (package-refresh-contents)
  (package-install 'monokai-pro-theme))

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
