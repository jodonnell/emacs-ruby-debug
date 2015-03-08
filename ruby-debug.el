(defvar ruby-debug--opened-buffers nil)
(defvar ruby-debug--is-in-debug-session nil)

(defvar ruby-debug--is-evalling nil)
(defvar ruby-debug--is-showing-locals nil)
(defvar ruby-debug--is-locals-window-open nil)
(defvar ruby-debug--output-since-last-command "")
(defvar ruby-debug--process-name "server")

(define-minor-mode ruby-debug-mode
  "Get your foos in the right places."
  :lighter " ruby-debug"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "n") 'ruby-debug--next-line)
            (define-key map (kbd "c") 'ruby-debug--continue)
            (define-key map (kbd "j") 'ruby-debug--jump)
            (define-key map (kbd "e") 'ruby-debug--eval)
            (define-key map (kbd "s") 'ruby-debug--step)
            (define-key map (kbd "u") 'ruby-debug--up)
            (define-key map (kbd "d") 'ruby-debug--down)
            (define-key map (kbd "l") 'ruby-debug--show-local-variables-activate)
            (define-key map (kbd "i") 'ruby-debug--show-instance-variables)
            (define-key map (kbd "b") 'ruby-debug--breakpoint)
            (define-key map (kbd "B") 'ruby-debug--remove-all-breakpoints)
            map)
  (if (bound-and-true-p ruby-debug-mode)
      (message "hi"))
  ;;(set-process-filter (get-buffer-process ruby-debug--process-name) 'ruby-debug--process-filter))
  (ruby-debug--clear-overlay-arrows))

(defun ruby-debug--move-line (line)
  (ruby-debug--add-fringe-at-line 'ruby-debug--current-line line))

(defun ruby-debug--run-command(cmd)
  (process-send-string (get-buffer-process ruby-debug--process-name) (concat cmd "\n")))

(defun ruby-debug--show-local-variables-activate ()
  (interactive)
  (if ruby-debug--is-locals-window-open
      (setq ruby-debug--is-locals-window-open nil)
    (progn
      (setq ruby-debug--is-locals-window-open t)
      (ruby-debug--show-local-variables))))

(defun ruby-debug--show-local-variables ()
  (setq ruby-debug--is-showing-locals t)
  (ruby-debug--run-command "var local"))

(defun ruby-debug--show-instance-variables ()
  (interactive)
  (ruby-debug--run-command "var instance"))

(defun ruby-debug--next-line()
  (interactive)
  (ruby-debug--run-command "next"))

(defun ruby-debug--step()
  (interactive)
  (ruby-debug--run-command "step"))

(defun ruby-debug--up()
  (interactive)
  (ruby-debug--run-command "up"))

(defun ruby-debug--down()
  (interactive)
  (ruby-debug--run-command "down"))

(defun ruby-debug--eval(eval)
  (interactive "sEval: ")
  (setq ruby-debug--is-evalling t)
  (ruby-debug--run-command (concat "eval " eval)))

(defun ruby-debug--remove-all-breakpoints()
  (interactive)
  (ruby-debug--run-command "delete")
  (ruby-debug--run-command "y"))

(defun ruby-debug--continue()
  (interactive)
  (ruby-debug--run-command "continue"))

(defun ruby-debug--jump()
  (interactive)
  (ruby-debug--add-breakpoint-at-current-line)
  (ruby-debug--run-command "continue"))

(defun ruby-debug--breakpoint ()
  (interactive)
  (ruby-debug--add-breakpoint-at-current-line)
  (let ((x 0)
        (symbol 'ruby-debug--breakpoint-mark0))
    (while (boundp symbol)
      (progn
        (setq x (+ 1 x))
        (setq symbol (make-symbol (concat "ruby-debug--breakpoint-mark" (number-to-string x))))))
    (ruby-debug--add-fringe-breakpoint symbol)))

(defun ruby-debug--add-breakpoint-at-current-line ()
  (ruby-debug--run-command (concat "b " (number-to-string (line-number-at-pos)))))

(defun ruby-debug--get-marker-at-beginning-of-line (line)
  (if (not line)
      (setq line (line-number-at-pos)))
  (let (m)
    (save-excursion
      (goto-line line)
      (beginning-of-line)
      (setq m (make-marker))
      (set-marker m (point) (current-buffer)))))

(defun ruby-debug--clear-overlay-arrows ()
  (setq overlay-arrow-variable-list '()))

(defun ruby-debug--clear-current-line-fringe ()
  (setq overlay-arrow-variable-list (delete 'ruby-debug--current-line overlay-arrow-variable-list)))

(defun ruby-debug--add-overlay-arrow (temp-var)
  (make-variable-buffer-local temp-var)
  (add-to-list 'overlay-arrow-variable-list temp-var))

(defun ruby-debug--add-fringe-breakpoint (temp-var)
  (ruby-debug--add-overlay-arrow temp-var)
  (put temp-var 'overlay-arrow-bitmap 'exclamation-mark)
  (set temp-var (ruby-debug--get-marker-at-beginning-of-line (line-number-at-pos))))

(defun ruby-debug--add-fringe-at-line (temp-var line)
  (ruby-debug--add-overlay-arrow temp-var)
  (put temp-var 'overlay-arrow-bitmap 'right-arrow)
  (set temp-var (ruby-debug--get-marker-at-beginning-of-line line)))

(defun ruby-debug--get-current-line-from-output (output)
  (if (string-match "\n=> +\\([0-9]+\\):" output)
      (string-to-number (match-string 1 output))))

(defun ruby-debug--get-current-file-from-output (output)
  (if (string-match "\n\[[0-9]+, [0-9]+\] in \\(.*\\)" output)
      (match-string 1 output)))

(defun ruby-debug--is-debug-over (output)
  (string-match "Completed [0-9]+" output))

(defun ruby-debug--is-complete-output-chunk (output)
  (or
   (ruby-debug--is-debug-over output)
   (string-match "(byebug)" output)))

(defun ruby-debug--process-filter(output)
  (if (string= ruby-debug--process-name (buffer-name))
      (progn
        (setq ruby-debug--output-since-last-command (concat ruby-debug--output-since-last-command output))
        (if (ruby-debug--is-complete-output-chunk ruby-debug--output-since-last-command)
            (progn
              (ruby-debug--process-output ruby-debug--output-since-last-command)
              (set-buffer ruby-debug--process-name)

              (setq ruby-debug--output-since-last-command ""))))))

(defun ruby-debug--process-output (output)
  (if ruby-debug--is-evalling
      (ruby-debug--print-and-reset-eval output))

  (if ruby-debug--is-showing-locals
      (ruby-debug--print-and-reset-locals output))
  
  (ruby-debug--goto-debugged-line output)
  (if (ruby-debug--is-debug-over output)
      (ruby-debug--finish-debug)))

(defun ruby-debug--print-and-reset-eval (output)
  (setq ruby-debug--is-evalling nil)
  (message (replace-regexp-in-string "\n(byebug)" "" output)))

(defun ruby-debug--print-and-reset-locals (output)
  (let ((vars (replace-regexp-in-string "\n(byebug)" "" output)))
    (setq ruby-debug--is-showing-locals nil)
    (set-window-buffer
     (split-window-below (ruby-debug--vars-window-size vars))
     (get-buffer-create "*Ruby Debug Locals*"))
    (with-current-buffer (get-buffer-create "*Ruby Debug Locals*")
      (erase-buffer)
      (insert vars))))

(defun ruby-debug--vars-window-size (output)
  (let ((number-of-lines (+ 2 (s-count-matches "\n" output))))
    (* -1
       (if (> number-of-lines (window-min-size))
           number-of-lines
         (window-min-size)))))

(defun ruby-debug--goto-debugged-line (output)
  (ruby-debug--clear-current-line-fringe)

  (let ((current-line (ruby-debug--get-current-line-from-output output))
        (filename (ruby-debug--get-current-file-from-output output)))
    (if (and current-line filename)
        (progn
          (if (not ruby-debug--is-in-debug-session)
              (ruby-debug--begin-debug-session))
          (ruby-debug--open-and-mark-file filename)
          (ruby-debug--move-line current-line)
          (goto-line current-line)
          (if ruby-debug--is-locals-window-open
              (ruby-debug--show-local-variables))))))

(defun ruby-debug--begin-debug-session ()
  (setq ruby-debug--is-in-debug-session t)
  (ruby-debug--remove-all-breakpoints))

(defun ruby-debug--open-and-mark-file (filename)
  (find-file filename)
  (save-excursion
    (if (not (string= "*Ruby Debug Locals*" (buffer-name (window-buffer (other-window 1)))))
        (delete-other-windows)))
  (read-only-mode 1)
  (add-to-list 'ruby-debug--opened-buffers (current-buffer))
  (if (not (bound-and-true-p ruby-debug-mode))
      (ruby-debug-mode)))


(defun ruby-debug--remove-debug-mode-from-all-buffers (opened-buffers)
  (if opened-buffers
      (progn
        (with-current-buffer (car opened-buffers)
          (ruby-debug-mode 0)
          (read-only-mode 0)
          (ruby-debug--remove-debug-mode-from-all-buffers (cdr opened-buffers))))))


(defun ruby-debug--finish-debug ()
  (ruby-debug--clear-overlay-arrows)
  (ruby-debug--remove-debug-mode-from-all-buffers ruby-debug--opened-buffers)
  (setq ruby-debug--opened-buffers nil)
  (setq ruby-debug--is-locals-window-open nil)
  (setq ruby-debug--is-in-debug-session nil))


(add-hook 'comint-output-filter-functions 'ruby-debug--process-filter)
;(remove-hook 'comint-output-filter-functions 'ruby-debug--process-filter)
