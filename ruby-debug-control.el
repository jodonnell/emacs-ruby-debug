(define-minor-mode ruby-debug-control-mode
  "Get your foos in the right places."
  :lighter " ruby-debug-control"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "n") 'ruby-debug--next-line)
            (define-key map (kbd "c") 'ruby-debug--continue)
            (define-key map (kbd "j") 'ruby-debug--jump)
            (define-key map (kbd "e") 'ruby-debug--eval)
            (define-key map (kbd "s") 'ruby-debug--step)
            (define-key map (kbd "u") 'ruby-debug--up)
            (define-key map (kbd "d") 'ruby-debug--down)
            (define-key map (kbd "l") 'ruby-debug--show-local-variables-activate)
            (define-key map (kbd "i") 'ruby-debug--show-instance-variables-activate)
            (define-key map (kbd "b") 'ruby-debug--breakpoint)
            (define-key map (kbd "B") 'ruby-debug--remove-all-breakpoints)
            map)
  (ruby-debug--clear-overlay-arrows))

(defun ruby-debug--next-line ()
  "Step over."
  (interactive)
  (ruby-debug--run-command-and-record "next"))

(defun ruby-debug--step ()
  "Step into."
  (interactive)
  (ruby-debug--run-command-and-record "step"))

(defun ruby-debug--up ()
  "Up a frame."
  (interactive)
  (ruby-debug--run-command-and-record "up"))

(defun ruby-debug--down ()
  "Down a frame."
  (interactive)
  (ruby-debug--run-command-and-record "down"))

(defun ruby-debug--eval ()
  "Eval some code."
  (interactive)
  (ruby-debug--run-command-and-record (concat "eval " (ruby-debug--eval-prompt))))

(defun ruby-debug--eval-prompt ()
  "Get the string to eval."
  (let ((eval ""))
    (if (use-region-p)
        (setq eval (buffer-substring (region-beginning) (region-end))))
    (read-string "Eval: " eval)))

(defun ruby-debug--remove-all-breakpoints ()
  "Remove all the breakpoints."
  (interactive)
  (ruby-debug--run-command-and-record "delete")
  (ruby-debug--run-command "y"))

(defun ruby-debug--continue ()
  "Continue."
  (interactive)
  (ruby-debug--run-command-and-record "continue"))

(defun ruby-debug--jump ()
  "Jump to current line."
  (interactive)
  (ruby-debug--add-breakpoint-at-current-line)
  (ruby-debug--run-command-and-record "continue"))

(defun ruby-debug--breakpoint ()
  "Add breakpoint on the current line."
  (interactive)
  (ruby-debug--add-breakpoint-at-current-line)
  (let ((x 0)
        (symbol 'ruby-debug--breakpoint-mark0))
    (while (boundp symbol)
      (progn
        (setq x (+ 1 x))
        (setq symbol (make-symbol (concat "ruby-debug--breakpoint-mark" (number-to-string x))))))
    (ruby-debug--add-fringe-breakpoint symbol)))

(defun ruby-debug--run-command-and-record (cmd)
  "Run a byebug CMD in the server and record it."
  (setq ruby-debug--command-queue (cons cmd ruby-debug--command-queue))
  (ruby-debug--run-command cmd))

(defun ruby-debug--run-command (cmd)
  "Run a byebug CMD in the server."
  (comint-simple-send (get-buffer-process ruby-debug--process-name) cmd))

(defun ruby-debug--show-local-variables ()
  "Show local variables."
  (ruby-debug--run-command-and-record "var local"))

(defun ruby-debug--show-instance-variables ()
  "Show instance variables."
  (ruby-debug--run-command-and-record "var instance"))

(defun ruby-debug--add-breakpoint-at-current-line ()
  "Add breakpoint on the current line."
  (ruby-debug--run-command-and-record (concat "b " (number-to-string (line-number-at-pos)))))

(provide 'ruby-debug-control)
