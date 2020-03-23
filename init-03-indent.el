;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-03-indent.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Manage indentation
;;
;; Convert tabs to spaces and turn off ``electric'' indentation
(setq-default indent-tabs-mode nil)
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(setq-default octave-block-offset 4)

;; eof
