;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init.el
;; Emacs, Version 25.1.50 (9.0)
;; OS X Yosemite, Version 10.10.5
;; Last edited: June 17, 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initialization driver
;;
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))     ;; Turn off menu bar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))     ;; Turn off tool bar
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; Turn off scroll bar

(setq inhibit-startup-screen t ;; Suppress startup screen
      inhibit-splash-screen t)

(defun rthoma/init-emacs ()
  "Load various emacs init files"
  (interactive)
  (let ((file-name-handler-alist nil)
        (gc-cons-threshold 40000000)) ;; 40 MB
        (load "~/.emacs.d/config/init-01-packages.el")
        (load "~/.emacs.d/config/init-02-emacs.el")
        (load "~/.emacs.d/config/init-03-indent.el")
        (load "~/.emacs.d/config/init-04-latex.el")
        (load "~/.emacs.d/config/init-05-org.el")
        (load "~/.emacs.d/config/init-06-python.el")
        (load "~/.emacs.d/config/init-07-magit.el")
        (load "~/.emacs.d/config/init-08-ivy.el")))
(rthoma/init-emacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom setup
;;
;; (package-initialize) ;; This line needs to be here to keep package.el happy
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; eof
