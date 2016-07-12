(require 'ruby-debug)

(setq test-doc
"
[12, 21] in /Users/jacob/programming/movie_matchmaker/app/controllers/welcome_controller.rb
   12: 
   13:   def find_critics
   14:     byebug
   15:     user = User.find(params[:user_id])
   16:     @critics = Matcher.find_sorted_critics_and_score(user)
=> 17:     a = [1, 2, 3]
   18:     a.push(4)
   19:   end
   20: 
   21:   def delete_like_movie")

(setq test-end-doc
"
  Rendered welcome/find_critics.html.erb within layouts/application (2.3ms)
Completed 200 OK in 4822ms (Views: 439.7ms | ActiveRecord: 64.1ms)")


(ert-deftest ruby-debug-test--get-current-line-from-output ()
  (should (= (ruby-debug--get-current-line-from-output test-doc) 17)))

(ert-deftest ruby-debug-test--get-current-file-from-output ()
  (should (string= (ruby-debug--get-current-file-from-output test-doc)
                   "/Users/jacob/programming/movie_matchmaker/app/controllers/welcome_controller.rb")))

(ert-deftest ruby-debug-test--get-current-file-from-output-with-artifact ()
  (should (string= (ruby-debug--get-current-file-from-output
                    "
[12, 21] in /test/welcome_controller.rb")
                   "/test/welcome_controller.rb")))

(ert-deftest ruby-debug-test--clean-output-local ()
  (should (string= (ruby-debug--clean-output
                    "var local
bang
(byebug)")
                   "bang")))

(ert-deftest ruby-debug-test--clean-output-instance ()
  (should (string= (ruby-debug--clean-output
                    "var instance
bang
(byebug)")
                   "bang")))

(ert-deftest ruby-debug-test--clean-output-instance-with-artifact ()
  (should (string= (ruby-debug--clean-output
                    "var instance
bang
(byebug)")
                   "bang")))

(ert-deftest ruby-debug-test--clean-output-eval ()
  (should (string= (ruby-debug--clean-output
                    "eval person
bang
(byebug)")
                   "bang")))


(ert-deftest ruby-debug-test--get-and-remove-full-chunk ()
  (setq ruby-debug--output-since-last-command "bob")
  (should (not (ruby-debug--get-and-remove-full-chunk)))

  (setq ruby-debug--output-since-last-command "bob(byebug) sam Completed 200 Cracker")
  (should (string= (ruby-debug--get-and-remove-full-chunk) "bob(byebug)"))
  (should (string= ruby-debug--output-since-last-command "sam Completed 200 Cracker"))
  (should (string= (ruby-debug--get-and-remove-full-chunk) "sam Completed 200"))
  (should (string= ruby-debug--output-since-last-command "Cracker")))

(ert-deftest ruby-debug-test--is-debug-over ()
  (should (not (ruby-debug--is-debug-over test-doc)))
  (should (ruby-debug--is-debug-over test-end-doc)))

(ert-deftest ruby-debug-test--is-debug-over-by-command-prompt ()
  (let ((ruby-debug--command-prompt-regex "\\[Jacobs-MacBook-Pro "))
    (should (ruby-debug--is-debug-over "\n[Jacobs-MacBook-Pro ~/programming/emacs-ruby-debug/fixtures] "))))


(ert-deftest ruby-debug-test--is-complete-output-chunk ()
  (should (not (ruby-debug--is-complete-output-chunk test-doc)))
  (should (ruby-debug--is-complete-output-chunk (concat test-doc "\n(byebug)"))))

(ert-deftest ruby-debug-test--file-open ()
  (fixture "test.rb"
   (lambda ()
     (should (string= (what-line) "Line 8")))))

(ert-deftest ruby-debug-test--next-line ()
  (fixture "test.rb"
   (lambda ()
     (ruby-debug--next-line)
     (wait-for (string= (what-line) "Line 8")))))

(ert-deftest ruby-debug-test--step-into ()
  (fixture "test.rb"
   (lambda ()
     (ruby-debug-test--step-into-first-file)
     (should (string= (what-line) "Line 3")))))

(ert-deftest ruby-debug-test--locals-window ()
  (fixture "test.rb"
   (lambda ()
     (ruby-debug--show-local-variables-activate)
     (ruby-debug-test--wait-for-buffer-to-exist-and-set ruby-debug--local-variable-window)
     (should (string= (ruby-debug-test--buffer-contents-no-properties) "apple = 3\nobj = nil")))))

(ert-deftest ruby-debug-test--instance-window ()
  (fixture "test.rb"
   (lambda ()
     (ruby-debug-test--step-into-first-file)
     (ruby-debug--show-instance-variables-activate)
     (ruby-debug--next-line)
     (ruby-debug-test--wait-for-buffer-to-exist-and-set ruby-debug--instance-variable-window)
     (wait-for (string= (ruby-debug-test--buffer-contents-no-properties) "@integer = 3")))))

(ert-deftest ruby-debug-test--instance-window-and-locals ()
  (fixture "test.rb"
   (lambda ()
     (ruby-debug-test--step-into-first-file)
     (ruby-debug--show-local-variables-activate)
     (ruby-debug--show-instance-variables-activate)
     (ruby-debug--next-line)
     (ruby-debug-test--wait-for-buffer-to-exist-and-set ruby-debug--local-variable-window)
     (wait-for (s-starts-with? "integer = 3\nself = #<TestClass:" (ruby-debug-test--buffer-contents-no-properties)))
     (ruby-debug-test--wait-for-buffer-to-exist-and-set ruby-debug--instance-variable-window)
     (wait-for (string= (ruby-debug-test--buffer-contents-no-properties) "@integer = 3")))))

 (ert-deftest ruby-debug-test--ends-at-end-of-output ()
   (fixture "test.rb"
    (lambda ()
      (ruby-debug-test--step-into-first-file)
      (ruby-debug--continue)
      (ruby-debug-test--wait-for-debug-to-end)
      (ruby-debug-test--buffer-should-be-reset "test.rb")
      (ruby-debug-test--buffer-should-be-reset "test_class.rb")
      ;; test closes windows
      (should (eq nil overlay-arrow-variable-list)))))

 (ert-deftest ruby-debug-test--breakpoints-work ()
   (fixture "test.rb"
    (lambda ()
      (forward-line 2)
      (ruby-debug--breakpoint)
      (forward-line 2)
      (ruby-debug--continue)
      (ruby-debug--continue)
      (ruby-debug-test--wait-for-debug-to-end)
      (should (string= (what-line) "Line 10")))))

 (ert-deftest ruby-debug-test--debug-window-grows-as-needed ()
   (fixture "test_growing_debug_window.rb"
    (lambda ()
      (ruby-debug--show-instance-variables-activate)
      (ruby-debug-test--wait-for-buffer-to-exist-and-set ruby-debug--instance-variable-window)
      (should (eq 4 (window-size (get-buffer-window ruby-debug--instance-variable-window))))
      (ruby-debug--next-line)
      (sit-for 1.0)
      (should (eq 5 (window-size (get-buffer-window ruby-debug--instance-variable-window)))))))

(defun ruby-debug-test--wait-for-buffer-to-exist-and-set (buffer-name)
  (wait-for (get-buffer buffer-name))
  (set-buffer buffer-name))

(defun ruby-debug-test--buffer-contents-no-properties ()
    (buffer-substring-no-properties (point-min) (point-max)))

(defun ruby-debug-test--buffer-should-be-reset (filename)
      (set-buffer filename)
      (should (not buffer-read-only))
      (should (not (bound-and-true-p ruby-debug-control-mode))))

(defun ruby-debug-test--step-into-first-file ()
      (ruby-debug--next-line)
      (ruby-debug--step)
      (ruby-debug-test--wait-for-file-to-open "test_class.rb"))

(defun fixture (fixture-name body)
  (unwind-protect
      (progn
        (ruby-debug-test--init fixture-name)
        (ruby-debug-test--wait-for-file-to-open fixture-name)
        (sleep-for 0.05) ; needed to clear the deleting breakpoints
        (funcall body))
    (ruby-debug-test--cleanup)))

(defun ruby-debug-test--wait-for-debug-to-end ()
  (wait-for (eq nil ruby-debug--is-in-debug-session)))

(defun ruby-debug-test--init (fixture-name)
  (shell "test-ruby-debug-mode")
  (ruby-debug-mode)
  (comint-simple-send (get-buffer-process "test-ruby-debug-mode") "cd fixtures")
  (ruby-debug--run-command (concat "./" fixture-name)))

(defun ruby-debug-test--cleanup ()
  (set-buffer "test-ruby-debug-mode")
  (if ruby-debug--is-in-debug-session
      (progn
        (ruby-debug--continue)
        (ruby-debug-test--wait-for-debug-to-end)
        (ruby-debug-mode)))
  (kill-process (get-buffer-process ruby-debug--process-name))
  (set-process-query-on-exit-flag (get-buffer-process ruby-debug--process-name) nil)
  (kill-buffer "test-ruby-debug-mode"))

(defun ruby-debug-test--wait-for-file-to-open (filename)
  (wait-for (string= (buffer-name (window-buffer)) filename))
  (set-buffer filename))

(defmacro wait-for (func)
  `(ert-wait-for 0.8 (lambda () ,func)))

(defmacro ert-wait-for (timeout predicate &rest body)
  "Wait for maximum TIMEOUT second for PREDICATE to verify, than execute forms in BODY."
  `(with-timeout
       (,timeout (ert-fail ,predicate))
     (while (not (funcall ,predicate))
       (accept-process-output nil 0.05))
     ,@body))

;; Local Variables:
;; nameless-current-name: "ruby-debug-test"
;; End:
