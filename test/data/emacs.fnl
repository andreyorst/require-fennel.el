{:call (fn [] (emacs.call.+ 1 2 3))
 :eval (fn [] (emacs.eval "(progn (defvar fennel-require-test-var 42) 1337)"))
 :var (fn [] emacs.var.fennel-require-test-var)}
