(define-minor-mode ruby-debug-mode
  "Get your foos in the right places."
  :lighter "ruby-debug"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "n") 'ruby-debug-next-line)
            (define-key map (kbd "b") 'ruby-debug-add-fringe-breakpoint-to-list)
            map)
  (if (bound-and-true-p ruby-debug-mode)
      (ruby-debug-move-current-line))
  (ruby-debug-clear-overlay-arrows))

(defun ruby-debug-move-line (line)
  (ruby-debug-add-fringe-at-line 'ruby-debug-current-line line))

(defun ruby-debug-next-line()
  (interactive)
  (set-buffer "server")
  (erase-buffer)
  (process-send-string (get-buffer-process "server") "next\n"))


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


(defun ruby-debug-add-overlay-arrow (temp-var)
  (make-variable-buffer-local temp-var)
  (add-to-list 'overlay-arrow-variable-list temp-var))

(defun ruby-debug-add-fringe-breakpoint (temp-var)
  (ruby-debug-add-overlay-arrow temp-var)
  (define-fringe-bitmap 'fringemark-hollow-right-arrow [128 192 96 48 24 48 96 192 128] 9 8 'center)
  (put temp-var 'overlay-arrow-bitmap 'fringemark-hollow-right-arrow)
  (set temp-var (get-marker-at-beginning-of-line)))

(defun ruby-debug-add-fringe-at-line (temp-var line)
  (ruby-debug-add-overlay-arrow temp-var)
  (put temp-var 'overlay-arrow-bitmap 'right-arrow)
  (set temp-var (get-marker-at-beginning-of-line line)))


(defun ruby-debug-add-fringe-breakpoint-to-list ()
  (interactive)
  (let ((x 0)
        (symbol 'ruby-debug-breakpoint-mark0))
    (while (boundp symbol)
      (progn
        (setq x (+ 1 x))
        (setq symbol (make-symbol (concat "ruby-debug-breakpoint-mark" (number-to-string x))))))
    (ruby-debug-add-fringe-breakpoint symbol)))



(defun ruby-debug--get-current-line-from-output (output)
  (if (string-match "=> +\\([0-9]+\\):" output)
      (string-to-number (match-string 1 output))))

(defun ruby-debug--get-current-file-from-output (output)
  (if (string-match "\n\[[0-9]+, [0-9]+\] in \\(.*\\)" output)
      (match-string 1 output)))

(defun ruby-debug--process-filter(process output)
  (set-buffer "server")
  (insert output)
  (let ((current-line (ruby-debug--get-current-line-from-output output))
        (filename (ruby-debug--get-current-file-from-output output)))
    (if (and current-line filename)
        (progn
          (find-file filename)
          (ruby-debug-move-line current-line)
          (goto-line current-line)))))

(set-process-filter (get-buffer-process "server") 'ruby-debug--process-filter)


