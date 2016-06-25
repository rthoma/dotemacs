;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-07-magit.el
;; Emacs, Version 25.1.50 (9.0)
;; OS X Yosemite, Version 10.10.5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; magit setup
;;
(use-package magit
  ;;
  ;; Package preferences
  ;;
  :ensure t
  :pin melpa-stable
  :defer t
  ;;
  ;; Preload initialization
  ;;
  ;;:bind
  ;;
  ;;:init
  ;;   Commented out for Mac
  ;;   (progn
  ;;   ;; Add git directory to path and exec-path
  ;;     (setenv "PATH" (concat (getenv "PATH") ";C:\\Program Files\\Git\\bin"))
  ;;     (setq exec-path (append exec-path '("C:\\Program Files\\Git\\bin"))))
  ;;
  ;; After load configuration
  ;;
  ;; :config
  ;; (progn ;; config stuff)
  )

;; eof
