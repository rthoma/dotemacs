;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-09-theme.el
;; Emacs, Version 25.1.50 (9.0)
;; OS X Yosemite, Version 10.10.5
;; Windows 10 Pro, Version 1511, Build 10586.420
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; zenburn setup
;;
(use-package zenburn-theme
  :ensure t
  :pin melpa-stable
  :init
  (progn
    (setq custom-enabled-themes 'zenburn)
    (setq custom-safe-themes
      (quote
    ("afbb40954f67924d3153f27b6d3399df221b2050f2a72eb2cfa8d29ca783c5a8" default)
  ))))

;; eof
