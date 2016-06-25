;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-03-indent.el
;; Emacs, Version 25.1.50 (9.0)
;; OS X Yosemite, Version 10.10.5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Manage indentation
;;
;; Convert tabs to spaces and turn off ``electric'' indentation
(setq-default indent-tabs-mode nil)
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; Defining tab for indentation
(defun rthoma/indent-by-inserting-spaces (n)
  "insert spaces at the beginning of a line"
  (interactive)
  (let
    ((prevline-indent (save-excursion (forward-line -1) (current-indentation)))
     (diffline-indent
       (if (< (current-column) (current-indentation))
           (- (current-indentation) (current-column))
         0)))
       (if (< (current-column) prevline-indent)
           (if (< (current-indentation) prevline-indent)
               (indent-to (- prevline-indent diffline-indent))
             (insert (if (< n 3) "  " (if (< n 5) "    " "        "))))
         (insert (if (< n 3) "  " (if (< n 5) "    " "        "))))))

;; Defining backtab for reverse identation
(defun rthoma/unindent-by-removing-spaces (n)
  "remove spaces from the beginning of a line"
  (interactive)
  (if (< (current-column) n)
      (indent-to-left-margin))
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at (if (< n 3) "^  " (if (< n 5) "^    " "^        ")))
        (replace-match "")))))

;; elisp indentation setup
(defun rthoma/elisp-indent-config ()
  "For use in `emacs-lisp-mode-hook'."
  (local-set-key (kbd "<tab>")
    (lambda () (interactive) (rthoma/indent-by-inserting-spaces 2)))
  (local-set-key (kbd "<backtab>")
    (lambda () (interactive) (rthoma/unindent-by-removing-spaces 2))))

;; Configure indentation
(add-hook 'emacs-lisp-mode-hook #'rthoma/elisp-indent-config)

;; eof
