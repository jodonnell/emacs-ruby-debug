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

(defun ruby-debug-move-current-line ()
  (ruby-debug-add-fringe-current-line 'ruby-debug-current-line))

(defun ruby-debug-next-line()
  (interactive)
  (next-line)
  (ruby-debug-move-current-line)
  (set-buffer "server")
  (insert "next")
  (comint-send-input))

(defun ruby-debug-add-breakpoint()
  (interactive)
  (ruby-debug-add-breakpoint-fringe-to-current-line)
  (set-buffer "server")
  (insert "b")
  (comint-send-input))

(defun get-marker-at-beginning-of-line()
  (let (m)
    (save-excursion
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


(defun ruby-debug-add-fringe-current-line (temp-var)
  (ruby-debug-add-overlay-arrow temp-var)
  (put temp-var 'overlay-arrow-bitmap 'right-arrow)
  (set temp-var (get-marker-at-beginning-of-line)))

(defun ruby-debug-add-fringe-breakpoint-to-list ()
  (interactive)
  (let ((x 0)
        (symbol 'ruby-debug-breakpoint-mark0))
    (while (boundp symbol)
      (progn
        (setq x (+ 1 x))
        (setq symbol (make-symbol (concat "ruby-debug-breakpoint-mark" (number-to-string x))))))
    (ruby-debug-add-fringe-breakpoint symbol)))
