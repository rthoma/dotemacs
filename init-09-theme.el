;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-09-theme.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; zenburn setup
;;
(use-package zenburn-theme
  :ensure t
  :pin melpa
  :init
  (progn
    (setq custom-safe-themes
      (quote
        "afbb40954f67924d3153f27b6d3399df221b2050f2a72eb2cfa8d29ca783c5a8"))
    (load-theme 'zenburn t)

    (when (eq system-type 'darwin)
          (delete-file "~/Library/Colors/Emacs.clr"))))

;; eof
