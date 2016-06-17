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



(ert-deftest ruby-debug--get-current-line-from-output-test ()
  (should (= (ruby-debug--get-current-line-from-output test-doc) 17)))

(ert-deftest ruby-debug--get-current-file-from-output-test ()
  (should (string= (ruby-debug--get-current-file-from-output test-doc)
                   "/Users/jacob/programming/movie_matchmaker/app/controllers/welcome_controller.rb")))

(ert-deftest ruby-debug--is-debug-over-test ()
  (should (not (ruby-debug--is-debug-over test-doc)))
  (should (ruby-debug--is-debug-over test-end-doc)))


(ert-deftest ruby-debug--is-complete-output-chunk-test ()
  (should (not (ruby-debug--is-complete-output-chunk test-doc)))
  (should (ruby-debug--is-complete-output-chunk (concat test-doc "\n(byebug)")))
  (should (ruby-debug--is-complete-output-chunk test-end-doc)))

(ert-deftest ruby-debug--test-file-open ()
  (my-fixture
   (lambda ()
     (should (string= (what-line) "Line 8")))))

(ert-deftest ruby-debug--test-next-line ()
  (my-fixture
   (lambda ()
     (ruby-debug--next-line)
     (wait-for (string= (what-line) "Line 8")))))

(ert-deftest ruby-debug--test-step-into ()
  (my-fixture
   (lambda ()
     (ruby-debug-test--step-into-first-file)
     (should (string= (what-line) "Line 3")))))

 (ert-deftest ruby-debug--ends-at-end-of-output ()
   (my-fixture
    (lambda ()
      (ruby-debug-test--step-into-first-file)
      (ruby-debug--continue)
      (ruby-debug-test--wait-for-debug-to-end)
      (should (eq nil overlay-arrow-variable-list)))))

(defun ruby-debug-test--step-into-first-file ()
      (ruby-debug--next-line)
      (ruby-debug--step)
      (ruby-debug-test--wait-for-file-to-open "test_class.rb"))

(defun my-fixture (body)
  (unwind-protect
      (progn
        (ruby-debug-test--init)
        (ruby-debug-test--wait-for-file-to-open "test.rb")
        (funcall body))
    (ruby-debug-test--cleanup)))

(defun ruby-debug-test--wait-for-debug-to-end ()
  (wait-for (eq nil ruby-debug--is-in-debug-session)))


(defun ruby-debug-test--init ()
  (shell "test-ruby-debug-mode")
  (ruby-debug-mode)
  (ruby-debug--run-command "./test.rb"))

(defun ruby-debug-test--cleanup ()
  (set-buffer "test-ruby-debug-mode")
  (ruby-debug--continue)
  (ruby-debug-test--wait-for-debug-to-end)
  (ruby-debug-mode)
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

