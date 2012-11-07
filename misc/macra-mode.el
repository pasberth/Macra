(eval-when-compile (require 'cl))

(defvar macra-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in Macra mode.")

(defconst macra-font-lock-keywords
  (list
    (concat
       (regexp-opt
         '( "#macro"
	    "#context"
	    "#end"
	    "!if"
            "!lambda"
            "!define"
            "!return"
            "!funcall"
            "!print")
         t))))
;;;###autoload
(defun macra-mode ()
  "test macra mode"
  (interactive)
  (kill-all-local-variables)
  (use-local-map macra-mode-map)
  (set (make-local-variable 'font-lock-defaults)
          '((macra-font-lock-keywords) nil nil))
  (set (make-local-variable 'font-lock-keywords)
        macra-font-lock-keywords)
  (setq mode-name "Macra"))
;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.macra\\'") 'macra-mode))

(provide 'macra-mode)

;;; macra-mode.el ends here
