;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-07-magit.el
;; Emacs, Version 24.5
;; Windows 10 Pro, Version 1511
;; Last edited: June 21, 2016
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
  :init
  (progn
    ;; Add git directory to path and exec-path
    (setenv "PATH" (concat (getenv "PATH") ";C:\Program Files\Git\bin"))
    (setq exec-path (append exec-path '("C:\Program Files\Git\bin"))))
  ;;
  ;; After load configuration
  ;;
  ;; :config
  ;; (progn ;; config stuff)
  )

;; eof
