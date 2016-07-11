(defun ruby-debug--process-chunk (chunk)
  (when (and (ruby-debug--is-debug-over chunk) ruby-debug--is-in-debug-session)
    (ruby-debug--finish-debug))

  (when (ruby-debug--is-complete-output-chunk chunk)
    (ruby-debug--process-command chunk)
    ;;(ruby-debug--debug-chunks chunk)
    (set-buffer ruby-debug--process-name)))

(defun ruby-debug--process-command (chunk)
  "Process a completed chunk of server CHUNK."
  (let ((command (car (last ruby-debug--command-queue))))
    (setq ruby-debug--command-queue (butlast ruby-debug--command-queue))

    ;;(ruby-debug--debug-chunks (concat command "\n" chunk))

    (if (s-starts-with? "eval " command)
        (ruby-debug--print-and-reset-eval chunk))

    (if (string= command "var local")
        (ruby-debug--print-and-reset-vars chunk ruby-debug--local-variable-window #'ruby-debug--is-window-locals-showing))

    (if (string= command "var instance")
        (ruby-debug--print-and-reset-vars chunk ruby-debug--instance-variable-window #'ruby-debug--is-window-instance-showing))

    (if (ruby-debug--was-movement-command chunk)
        (ruby-debug--goto-debugged-line chunk))))

(defun ruby-debug--was-movement-command (output)
  (and (ruby-debug--get-current-line-from-output output) (ruby-debug--get-current-file-from-output output)))

(provide 'ruby-debug-process-chunk)
