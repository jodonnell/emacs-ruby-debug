(defun ruby-debug--move-fringe (line)
  "Change the fringe to reflect the currently debugged LINE."
  (ruby-debug--add-fringe-at-line 'ruby-debug--current-line line))

(defun ruby-debug--get-marker-at-beginning-of-line (line)
  "Add a marker at LINE."
  (if (not line)
      (setq line (line-number-at-pos)))
  (let (m)
    (save-excursion
      (ruby-debug--goto-line line)
      (beginning-of-line)
      (setq m (make-marker))
      (set-marker m (point) (current-buffer)))))

(defun ruby-debug--clear-overlay-arrows ()
  "Clear the list of overlay arrows."
  (setq overlay-arrow-variable-list '()))

(defun ruby-debug--clear-current-line-fringe ()
  "Delete the fringe at the current line."
  (setq overlay-arrow-variable-list (delete 'ruby-debug--current-line overlay-arrow-variable-list)))

(defun ruby-debug--add-overlay-arrow (temp-var)
  "Add overlay arrow to a list stored in variable TEMP-VAR."
  (make-variable-buffer-local temp-var)
  (add-to-list 'overlay-arrow-variable-list temp-var))

(defun ruby-debug--add-fringe-breakpoint (temp-var)
  "Add overlay exclamation to a list stored in variable TEMP-VAR."
  (ruby-debug--add-overlay-arrow temp-var)
  (put temp-var 'overlay-arrow-bitmap 'exclamation-mark)
  (set temp-var (ruby-debug--get-marker-at-beginning-of-line (line-number-at-pos))))

(defun ruby-debug--add-fringe-at-line (temp-var line)
  "Add overlay fringe to a list stored in variable TEMP-VAR to the LINE."
  (ruby-debug--add-overlay-arrow temp-var)
  (put temp-var 'overlay-arrow-bitmap 'right-arrow)
  (set temp-var (ruby-debug--get-marker-at-beginning-of-line line)))


(provide 'ruby-debug-fringes)
