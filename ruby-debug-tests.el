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
"(byebug) c
  Rendered welcome/find_critics.html.erb within layouts/application (2.3ms)
Completed 200 OK in 4822ms (Views: 439.7ms | ActiveRecord: 64.1ms)")



(ert-deftest ruby-debug--get-current-line-from-output-test ()
  (should (= (ruby-debug--get-current-line-from-output test-doc) 17)))

(ert-deftest ruby-debug--get-current-file-from-output-test ()
  (should (string= (ruby-debug--get-current-file-from-output test-doc)
                   "/Users/jacob/programming/movie_matchmaker/app/controllers/welcome_controller.rb")))

(ert-deftest ruby-debug--is-debug-over-test ()
  (should (ruby-debug--is-debug-over test-end-doc)))

