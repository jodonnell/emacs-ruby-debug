(defvar ruby-debug--output-since-last-command "")

(defun ruby-debug--process-filter (output)
  "The process filter on the servers buffer, gives OUTPUT."
  (when (string= ruby-debug--process-name (buffer-name))
    (setq ruby-debug--output-since-last-command (concat ruby-debug--output-since-last-command output))
    (ruby-debug--process-output-while-full-chunk-exists)))

(defun ruby-debug--has-full-chunk ()
  (string-match (concat "^[\0-\377[:nonascii:]]*?\\((byebug)\\|Completed [0-9]+\\|" ruby-debug--command-prompt-regex "\\)") ruby-debug--output-since-last-command))

(defun ruby-debug--get-and-remove-full-chunk ()
  (if (ruby-debug--has-full-chunk)
      (progn
        (let ((matched-chunk (match-string 0 ruby-debug--output-since-last-command)))
          (setq ruby-debug--output-since-last-command (s-trim-left (substring ruby-debug--output-since-last-command (length matched-chunk))))
          matched-chunk))
    nil))

(defun ruby-debug--process-output-while-full-chunk-exists ()
  (while (ruby-debug--has-full-chunk)
    (ruby-debug--process-chunk (ruby-debug--get-and-remove-full-chunk))))

(defun ruby-debug--is-debug-over (output)
  "Look for a server finished request message from OUTPUT."
  (or (string-match "Completed [0-9]+" output)
      (string-match ruby-debug--command-prompt-regex output)))

(defun ruby-debug--is-complete-output-chunk (output)
  "Check to see if the output check is done from OUTPUT."
  (string-match "(byebug)" output))

(provide 'ruby-debug-process-filter)
