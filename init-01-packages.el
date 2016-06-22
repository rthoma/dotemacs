;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-01-packages.el
;; Emacs, Version 24.5
;; Windows 10 Pro, Version 1511
;; Last edited: June 21, 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Package setup
;;
(require 'package)
(setq package-enable-at-startup nil)

(setq
  package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                     ("melpa-stable" . "http://stable.melpa.org/packages/")))

(setq
  package-selected-packages
    '(async                 ;; dependency
      auctex                ;; installed
      bind-key              ;; dependency
      company               ;; dependency for elpy
      company-quickhelp     ;; installed
      color-theme           ;; dependency
      dash                  ;; dependency
      diminish              ;; dependency
      elpy                  ;; installed
      exec-path-from-shell  ;; installed
      find-file-in-project  ;; dependency for elpy
      git-commit            ;; dependency for magit
      highlight-indentation ;; dependency for elpy
      iedit                 ;; installed
      ivy                   ;; installed
      magit                 ;; installed
      magit-popup           ;; dependency for magit
      org                   ;; installed
      pos-tip               ;; dependency for company-quickhelp
      pyvenv                ;; dependency for elpy
      s                     ;; installed
      tablist               ;; dependency
      use-package           ;; installed
      with-editor           ;; dependency
      yasnippet             ;; dependency for elpy
      zenburn-theme))       ;; installed

(package-initialize)

;; Bootstrap ``use-package''
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; eof
