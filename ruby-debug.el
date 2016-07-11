;;; ruby-debug.el --- A modular front-end for interacting with external debuggers

;; Author: Jacob O'Donnell
;; Version: 1.0
;; URL: http://github.com/jodonnell/emacs-ruby-debug
;; Package-Requires: ((s "1.4"))
;; Compatibility: GNU Emacs 24.x

;; Copyright (C) 2016 Free Software Foundation, Inc

;; Author: Jacob O'Donnell <jacobodonnell@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A debugger used with byebug to debug rails apps

;;; Code:

(require 's)
(require 'ruby-debug-control)
(require 'ruby-debug-process-filter)
(require 'ruby-debug-process-chunk)
(require 'ruby-debug-window)
(require 'ruby-debug-fringes)

(defvar ruby-debug--opened-buffers nil)
(defvar ruby-debug--is-in-debug-session nil)

(defvar ruby-debug--local-variable-window "*Ruby Debug Local*")
(defvar ruby-debug--is-locals-window-open nil)

(defvar ruby-debug--instance-variable-window "*Ruby Debug Instance*")
(defvar ruby-debug--is-instance-window-open nil)

(defvar ruby-debug--process-name "server")
(defvar ruby-debug--command-queue '("next"))
(defvar ruby-debug--command-prompt-regex "hopefully this never matches a prompt")

(setq ruby-debug--command-prompt-regex "Jacobs-MBP")

;;;###autoload
(define-minor-mode ruby-debug-mode
  "Get your foos in the right places."
  :lighter " ruby-debug"
  (if (bound-and-true-p ruby-debug-mode)
      (progn
        (add-hook 'comint-output-filter-functions 'ruby-debug--process-filter)
        (setq ruby-debug--process-name (buffer-name)))
    (remove-hook 'comint-output-filter-functions 'ruby-debug--process-filter))
  (ruby-debug--clear-overlay-arrows))

(defun ruby-debug--debug-chunks (chunk)
  (set-buffer "chunk-output")
  (insert "Chunk:\n")
  (insert chunk)
  (insert "\n\n"))

(defun ruby-debug--get-current-line-from-output (output)
  "Get the line number of current line from OUTPUT."
  (if (string-match "\n=> +\\([0-9]+\\):" output)
      (string-to-number (match-string 1 output))))

(defun ruby-debug--get-current-file-from-output (output)
  "Get the current file from OUTPUT."
  (if (string-match "\n\[[0-9]+, [0-9]+\] in \\(.*\\)" output)
      (s-trim-right (match-string 1 output))))

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

(defun ruby-debug--begin-debug-session ()
  "TODO."
  (setq comint-scroll-to-bottom-on-output t)
  (ruby-debug--remove-all-breakpoints)
  (setq ruby-debug--is-in-debug-session t))

(defun ruby-debug--open-and-mark-file (filename)
  "TODO FILENAME."
  (find-file filename)
  (if (not (or (ruby-debug--is-window-locals-showing) (ruby-debug--is-window-instance-showing)))
      (delete-other-windows))
  (read-only-mode 1)
  (add-to-list 'ruby-debug--opened-buffers (current-buffer))
  (if (not (bound-and-true-p ruby-debug-control-mode))
      (ruby-debug-control-mode)))

(defun ruby-debug--remove-debug-mode-from-all-buffers (opened-buffers)
  "Turn off debug mode for OPENED-BUFFERS buffers."
  (when opened-buffers
    (with-current-buffer (car opened-buffers)
      (ruby-debug-control-mode 0)
      (read-only-mode 0)
      (ruby-debug--remove-debug-mode-from-all-buffers (cdr opened-buffers)))))

(defun ruby-debug--finish-debug ()
  "Tear down the debugger code."
  (ruby-debug--clear-overlay-arrows)
  (ruby-debug--remove-debug-mode-from-all-buffers ruby-debug--opened-buffers)
  (setq ruby-debug--opened-buffers nil)
  (ruby-debug--close-window ruby-debug--instance-variable-window 'ruby-debug--is-instance-window-open)
  (ruby-debug--close-window ruby-debug--local-variable-window 'ruby-debug--is-locals-window-open)
  (setq ruby-debug--command-queue '("next"))
  (setq ruby-debug--is-in-debug-session nil))

(defun ruby-debug--goto-line (num)
  "Go to a line number NUM."
  (goto-char (point-min))
  (forward-line (1- num)))

(defun rails-server-start ()
  (interactive)
  (let ((process-connection-type nil))  ; use a pipe
    (shell "server")
    (comint-simple-send (get-buffer-process "server") "cd .")
    (comint-simple-send (get-buffer-process "server") "script -q /dev/null rails s")))

(defun rails-server-restart ()
  (interactive)
  (shell "server")
  (comint-kill-subjob)
  (sit-for 0.5)
  (rails-server-start))

(provide 'ruby-debug)

;;; ruby-debug.el ends here
