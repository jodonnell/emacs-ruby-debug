(defun ruby-debug--close-window (buffer-name is-open-var)
  "Close window named BUFFER-NAME and set the IS-OPEN-VAR to false."
  (set is-open-var nil)
  (when (get-buffer-window buffer-name)
    (delete-window (get-buffer-window buffer-name))
    (kill-buffer buffer-name)))

(defun ruby-debug--print-and-reset-eval (output)
  "Prints the eval OUTPUT and then turns off eval mode."
  (message (ruby-debug--clean-output output)))

(defun ruby-debug--print-and-reset-vars (output var-window is-window-showing)
  "OUTPUT VAR-WINDOW IS-WINDOW-SHOWING."
  (let ((vars (ruby-debug--clean-output output)))
    (ruby-debug--create-debug-window-if-none-existant var-window)
    (ruby-debug--show-debug-window-if-not-showing vars var-window is-window-showing)
    (ruby-debug--insert-output-into-debug-window vars var-window)))

(defun ruby-debug--insert-output-into-debug-window (vars var-window)
  (with-current-buffer (get-buffer var-window)
    (erase-buffer)
    (insert vars)
    (fit-window-to-buffer (get-buffer-window var-window))))

(defun ruby-debug--show-debug-window-if-not-showing (vars var-window is-window-showing)
  (if (not (funcall is-window-showing))
      (set-window-buffer
       (split-window-below (window-min-size))
       (get-buffer var-window))))

(defun ruby-debug--create-debug-window-if-none-existant (debug-window)
  (when (not (get-buffer debug-window))
    (with-current-buffer (get-buffer-create debug-window)
      (toggle-truncate-lines 1))))

(defun ruby-debug--is-window-locals-showing ()
  "TODO."
  (get-buffer-window ruby-debug--local-variable-window))

(defun ruby-debug--is-window-instance-showing ()
  "TODO."
  (get-buffer-window ruby-debug--instance-variable-window))

(defun ruby-debug--remove-string-from-string (remove-string output)
  (replace-regexp-in-string remove-string "" output))

(defun ruby-debug--clean-output (output)
  (ruby-debug--remove-string-from-string "var \\(local\\|instance\\)\n" (ruby-debug--remove-string-from-string "eval .*\n" (ruby-debug--remove-string-from-string "\n(byebug)" (ruby-debug--remove-string-from-string "" output)))))

(provide 'ruby-debug-window)
