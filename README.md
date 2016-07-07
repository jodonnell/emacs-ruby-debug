I use rails-server-start and then turn on ruby-debug-mode.

I use nameless to make the package prefix not as bad.


to run the tests you can use rvm to install byebug in this directory or make sure the byebug gem is locally installed.

You can set ruby-debug--command-prompt-regex to make this work when running scripts from the command line.   I just use my computers host name which is found in my prompt and thats accurate enough.