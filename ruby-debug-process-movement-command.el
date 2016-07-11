(defun ruby-debug--goto-debugged-line (output)
  "Go to the debugged line found in OUTPUT."
  (ruby-debug--clear-current-line-fringe)
  (if (not ruby-debug--is-in-debug-session)
      (ruby-debug--begin-debug-session))

  (let ((current-line (ruby-debug--get-current-line-from-output output))
        (filename (ruby-debug--get-current-file-from-output output)))
    (ruby-debug--open-and-mark-file filename)
    (ruby-debug--move-fringe current-line)
    (ruby-debug--goto-line current-line)

    (if ruby-debug--is-instance-window-open
        (ruby-debug--show-instance-variables))

    (if ruby-debug--is-locals-window-open
        (ruby-debug--show-local-variables))))


(defun ruby-debug--get-current-line-from-output (output)
  "Get the line number of current line from OUTPUT."
  (if (string-match "\n=> +\\([0-9]+\\):" output)
      (string-to-number (match-string 1 output))))

(defun ruby-debug--get-current-file-from-output (output)
  "Get the current file from OUTPUT."
  (if (string-match "\n\[[0-9]+, [0-9]+\] in \\(.*\\)" output)
      (s-trim-right (match-string 1 output))))

(defun ruby-debug--open-and-mark-file (filename)
  "TODO FILENAME."
  (find-file filename)
  (if (not (or (ruby-debug--is-window-locals-showing) (ruby-debug--is-window-instance-showing)))
      (delete-other-windows))
  (read-only-mode 1)
  (add-to-list 'ruby-debug--opened-buffers (current-buffer))
  (if (not (bound-and-true-p ruby-debug-control-mode))
      (ruby-debug-control-mode)))

(defun ruby-debug--goto-line (num)
  "Go to a line number NUM."
  (goto-char (point-min))
  (forward-line (1- num)))

(provide 'ruby-debug-process-movement-command)
