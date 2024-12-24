{:call (fn [] (Elisp.call.+ 1 2 3))
 :eval (fn [] (Elisp.eval "(progn (defvar fennel-require-test-var 42) 1337)"))
 :var (fn [] Elisp.var.fennel-require-test-var)}
