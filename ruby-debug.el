(defvar ruby-debug--opened-buffers nil)
(defvar ruby-debug--is-in-debug-session nil)
(defvar ruby-debug--is-evalling nil)

(define-minor-mode ruby-debug-mode
  "Get your foos in the right places."
  :lighter " ruby-debug"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "n") 'ruby-debug-next-line)
            (define-key map (kbd "c") 'ruby-debug-continue)
            (define-key map (kbd "j") 'ruby-debug-jump)
            (define-key map (kbd "e") 'ruby-debug-eval)
            (define-key map (kbd "s") 'ruby-debug-step)
            (define-key map (kbd "b") 'ruby-debug-add-fringe-breakpoint-to-list)
            map)
  (if (bound-and-true-p ruby-debug-mode)
      (message "hi"))
                                        ;(set-process-filter (get-buffer-process "server") 'ruby-debug--process-filter))
  (ruby-debug-clear-overlay-arrows))

(defun ruby-debug-move-line (line)
  (ruby-debug-add-fringe-at-line 'ruby-debug-current-line line))

(defun ruby-debug-run-command(cmd)
  (set-buffer "server")
  (process-send-string (get-buffer-process "server") (concat cmd "\n")))

(defun ruby-debug-next-line()
  (interactive)
  (ruby-debug-run-command "next"))

(defun ruby-debug-step()
  (interactive)
  (ruby-debug-run-command "step"))

(defun ruby-debug-eval(eval)
  (interactive "sEval: ")
  (setq ruby-debug--is-evalling t)
  (ruby-debug-run-command (concat "eval " eval)))

(defun ruby-debug--delete()
  (interactive)
  (ruby-debug-run-command "delete")
  (ruby-debug-run-command "y"))

(defun ruby-debug-continue()
  (interactive)
  (ruby-debug-run-command "continue"))

(defun ruby-debug-jump()
  (interactive)
  (ruby-debug-add-breakpoint-at-current-line)
  (ruby-debug-run-command "continue"))

(defun ruby-debug-add-breakpoint()
  (interactive)
  (ruby-debug-add-breakpoint-fringe-to-current-line)
  (set-buffer "server")
  (insert "b")
  (comint-send-input))

(defun get-marker-at-beginning-of-line (line)
  (if (not line)
      (setq line (line-number-at-pos)))
  (let (m)
    (save-excursion
      (goto-line line)
      (beginning-of-line)
      (setq m (make-marker))
      (set-marker m (point) (current-buffer)))))

(defun ruby-debug-clear-overlay-arrows ()
  (setq overlay-arrow-variable-list '()))

(defun ruby-debug-clear-current-line-fringe ()
  (setq overlay-arrow-variable-list (delete 'ruby-debug-current-line overlay-arrow-variable-list)))

(defun ruby-debug-add-overlay-arrow (temp-var)
  (make-variable-buffer-local temp-var)
  (add-to-list 'overlay-arrow-variable-list temp-var))

(defun ruby-debug-add-fringe-breakpoint (temp-var)
  (ruby-debug-add-overlay-arrow temp-var)
  (put temp-var 'overlay-arrow-bitmap 'exclamation-mark)
  (set temp-var (get-marker-at-beginning-of-line (line-number-at-pos))))

(defun ruby-debug-add-fringe-at-line (temp-var line)
  (ruby-debug-add-overlay-arrow temp-var)
  (put temp-var 'overlay-arrow-bitmap 'right-arrow)
  (set temp-var (get-marker-at-beginning-of-line line)))


(defun ruby-debug-add-breakpoint-at-current-line ()
  (ruby-debug-run-command (concat "b " (number-to-string (line-number-at-pos)))))

(defun ruby-debug-add-fringe-breakpoint-to-list ()
  (interactive)
  (ruby-debug-add-breakpoint-at-current-line)
  (let ((x 0)
        (symbol 'ruby-debug-breakpoint-mark0))
    (while (boundp symbol)
      (progn
        (setq x (+ 1 x))
        (setq symbol (make-symbol (concat "ruby-debug-breakpoint-mark" (number-to-string x))))))
    (ruby-debug-add-fringe-breakpoint symbol)))


(defun ruby-debug--get-current-line-from-output (output)
  (if (string-match "\n=> +\\([0-9]+\\):" output)
      (string-to-number (match-string 1 output))))

(defun ruby-debug--get-current-file-from-output (output)
  (if (string-match "\n\[[0-9]+, [0-9]+\] in \\(.*\\)" output)
      (match-string 1 output)))

(defun ruby-debug--is-debug-over (output)
  (string-match "Completed [0-9]+" output))

(defun ruby-debug--process-filter(output)
  (if (string= "server" (buffer-name))
      (progn
        (if ruby-debug--is-evalling
            (progn
              (setq ruby-debug--is-evalling nil)
              (message output)))
        (ruby-debug-clear-current-line-fringe)
        (ruby-debug--goto-debugged-line output)
        (if (ruby-debug--is-debug-over output)
            (ruby-debug--finish-debug))
        (set-buffer "server"))))

(defun ruby-debug--goto-debugged-line (output)
  (let ((current-line (ruby-debug--get-current-line-from-output output))
        (filename (ruby-debug--get-current-file-from-output output)))
    (if (and current-line filename)
        (progn
          (if (not ruby-debug--is-in-debug-session)
              (ruby-debug--begin-debug-session))
          (ruby-debug--open-and-mark-file filename)
          (ruby-debug-move-line current-line)
          (goto-line current-line)))))

(defun ruby-debug--begin-debug-session ()
  (setq ruby-debug--is-in-debug-session t)
  (ruby-debug--delete))

(defun ruby-debug--open-and-mark-file (filename)
  (find-file filename)
  (add-to-list 'ruby-debug--opened-buffers (current-buffer))
  (if (not (bound-and-true-p ruby-debug-mode))
      (ruby-debug-mode)))


(defun ruby-debug--remove-debug-mode-from-all-buffers (opened-buffers)
  (if opened-buffers
      (progn
        (with-current-buffer (car opened-buffers)
          (ruby-debug-mode 0)
          (ruby-debug--remove-debug-mode-from-all-buffers (cdr opened-buffers))))))


(defun ruby-debug--finish-debug ()
  (ruby-debug-clear-overlay-arrows)
  (ruby-debug--remove-debug-mode-from-all-buffers ruby-debug--opened-buffers)
  (setq ruby-debug--opened-buffers nil)
  (setq ruby-debug--is-in-debug-session nil))


(add-hook 'comint-output-filter-functions 'ruby-debug--process-filter)
;(remove-hook 'comint-output-filter-functions 'ruby-debug--process-filter)
