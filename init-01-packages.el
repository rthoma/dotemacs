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
        '(("gnu"          . "http://elpa.gnu.org/packages/")
          ("melpa-stable" . "http://stable.melpa.org/packages/")))
  (setq
    package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))))

(setq
  package-selected-packages
    '(async                  ;; required by magit
      auctex                 ;;
      bind-key               ;; required by use-package
      company                ;; required by elpy
      company-quickhelp      ;;
      color-theme            ;;
      counsel                ;;
      dash                   ;; required by magit
      diminish               ;; required by use-package
      elpy                   ;;
      exec-path-from-shell   ;;
      find-file-in-project   ;; required by elpy
      git-commit             ;; required by magit
      highlight-indentation  ;; required by elpy
      iedit                  ;;
      ivy                    ;; required by swiper and find-file
      magit                  ;;
      magit-popup            ;; required by magit
      org                    ;;
      pos-tip                ;; required by company-quickhelp
      pyvenv                 ;; required by elpy
      swiper                 ;; required by counsel
      tablist                ;;
      use-package            ;;
      with-editor            ;; required by magit
      yasnippet              ;; required by elpy
      zenburn-theme))        ;;

(when (version< emacs-version "27.0")
      (package-initialize))

;; Bootstrap ``use-package''
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (require 'use-package))

;; eof
