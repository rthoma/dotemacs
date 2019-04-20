;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-01-packages.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Package setup
;;
(require 'package)
(setq package-enable-at-startup nil)  ;; prevents initializing twice

(if (eq system-type 'windows-nt)
    (setq
      package-archives
        '(("gnu"   . "http://elpa.gnu.org/packages/")
          ("melpa" . "http://melpa.org/packages/")
          ("melpa-stable" . "http://stable.melpa.org/packages/")))
  (setq
    package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))))

(setq
  package-selected-packages
    '(async                  ;;
      auctex                 ;;
      bind-key               ;;
      company                ;;
      company-quickhelp      ;;
      counsel                ;;
      dash                   ;;
      diminish               ;;
      elpy                   ;;
      exec-path-from-shell   ;;
      find-file-in-project   ;;
      ghub                   ;;
      git-commit             ;;
      graphql                ;;
      highlight-indentation  ;;
      iedit                  ;;
      ivy                    ;;
      magit                  ;;
      magit-popup            ;;
      org                    ;;
      pos-tip                ;;
      pyvenv                 ;;
      s                      ;;
      swiper                 ;;
      tablist                ;;
      transient              ;;
      treepy                 ;;
      use-package            ;;
      with-editor            ;;
      yasnippet              ;;
      zenburn-theme))        ;;

(when (version< emacs-version "27.0")
      (package-initialize))

;; Bootstrap ``use-package''
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (require 'use-package))

;; Fetch the list of packages available
(unless package-archive-contents
  (or (file-exists-p package-user-dir)
      (package-refresh-contents)))

;; Install the missing packages
(dolist (package package-selected-packages)
 (unless (package-installed-p package)
   (package-install package)))

;; eof
