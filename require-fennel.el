;;; require-fennel.el --- Run Fennel code from the comfort of Emacs Lisp -*- lexical-binding: t -*-

;; Copyright © 2024 Andrey Listopadov

;; Author: Andrey Listopadov
;; URL: https://gitlab.com/andreyorst/require-fennel.el
;; Version: 0.0.3
;; Created: 2024-12-16
;; package-requires: ((emacs "28.1"))
;; Keywords: languages, tools

;;; Commentary:
;;
;; `require-fennel' allows loading Fennel code as Emacs Lisp.
;; Internally, Emacs starts a background REPL process, and loads the
;; desired Fennel code into it.  After obtaining module definitions,
;; an emacs-lisp function is created for each one by the means of
;; `defun'.  Defined functions feature documentation and arglist metadata
;; if Fennel provides them.  Fennnel variables, sotred in modules are
;; defined with `defconst'.

;;; Examples
;;
;; Loading the Fennel compiler:
;;
;; (require-fennel fennel)
;; (fennel.eval "(+ 1 2 3)")
;;
;; Loading arbitrary Fennel source from `path/to/file.fnl' as `foo':
;;
;; (require-fennel path.to.file :as foo)
;; (foo.bar 42)

;;; Code:

(declare-function fennel-proto-repl "ext:fennel-proto-repl")
(declare-function fennel-proto-repl-send-message "ext:fennel-proto-repl")
(declare-function fennel-proto-repl-send-message-sync "ext:fennel-proto-repl")

(defconst require-fennel--repl-buffer " *fennel-elisp*"
  "A buffer name used to create the internal REPL process.")

(defgroup require-fennel nil
  "Custom settings for `require-fennel' macro."
  :prefix "require-fennel-"
  :group nil)

(defcustom require-fennel-fennel-program "fennel --globals emacs,elisp-exports"
  "Path to the fennel executable."
  :group 'require-fennel
  :type 'string
  :package-version '(require-fennel "0.0.2"))

(defcustom require-fennel-timeout nil
  "Timeout in seconds for each command."
  :group 'require-fennel
  :type '(choice
	  (const :tag "Infinite timeout" nil)
	  (integer :tag "seconds"))
  :package-version '(require-fennel "0.0.2"))

(defcustom require-fennel-use-hash-tables nil
  "Default hash table format for features provided by `require-fennel'.

The default behavior is to use alists, which means that any Fennel hash
table will be converted to association list when returned to Emacs Lisp
side.  If you prefer using native hash tables, set this variable to t.

This setting can still be overidden with the `:use-hash-tables' keyword
argument of `require-fennel' macro."
  :group 'require-fennel
  :type '(choice
	  (const :tag "use alists" nil)
	  (const :tag "use native hash tables" t))
  :package-version '(require-fennel "0.0.2"))

(defun require-fennel--fennel-to-elisp (value &optional as-hash-table skip)
  "Convert Fennel VALUE into Emacs Lisp value.
Optional argument SKIP is used for recursive parsing of already read
data.  Optional argument AS-HASH-TABLE forces Emacs Lisp hash tables to
be used as return values for Fennel hash tables."
  (pcase value
    ((or "nil" "false" 'nil 'false) nil)
    ("true" 'true 't)
    (_ (let ((value (if skip value (car (read-from-string value)))))
         (pcase value
           ((pred hash-table-p)
            (if as-hash-table
                (let ((hash (make-hash-table :test #'equal)))
                  (maphash
                   (lambda (k v)
                     (puthash (require-fennel--fennel-to-elisp k as-hash-table t)
                              (require-fennel--fennel-to-elisp v as-hash-table t)
                              hash))
                   value)
                  hash)
              (let (results)
                (maphash
                 (lambda (k v)
                   (push (cons
                          (require-fennel--fennel-to-elisp k as-hash-table t)
                          (require-fennel--fennel-to-elisp v as-hash-table t))
                         results))
                 value)
                results)))
           (`(lambda . ,_) (eval value))
           ((or (pred vectorp) (pred listp))
            (thread-last
              value
              (mapcar (lambda (val) (require-fennel--fennel-to-elisp val as-hash-table t)))
              (apply #'vector)))
           (_ value))))))

(defun require-fennel--elisp-to-fennel (value)
  "Convert an elisp VALUE to a fennel variable.
Convert an elisp value, VAR, into a string of fennel source code
specifying a variable of the same value."
  (pcase value
    ((pred hash-table-p)
     (let (results)
       (maphash
        (lambda (key value)
          (push (format "%s %s"
                        (require-fennel--elisp-to-fennel key)
                        (require-fennel--elisp-to-fennel value))
                results))
        value)
       (concat "{" (mapconcat #'identity results " ") "}")))
    ((and `((,_ . ,b) . ,_)
          (guard (not (listp b))))
     (let (results)
       (dolist (kv value)
         (push (format "%s %s"
                       (require-fennel--elisp-to-fennel (car kv))
                       (require-fennel--elisp-to-fennel (cdr kv)))
               results))
       (concat "{" (mapconcat #'identity results " ") "}")))
    ((pred null) "nil")
    ((pred listp)
     (concat "[" (mapconcat #'require-fennel--elisp-to-fennel value " ") "]"))
    ((pred keywordp)
     (format "%S" (substring (symbol-name value) 1)))
    (`t "true")
    (_ (format
        (if (stringp value) "%S" "%s")
        (if (stringp value) (substring-no-properties value) value)))))

(defun require-fennel--transform-arg (arg rest &optional skip)
  "Transform ARG to a from that is understood by `declare'.
REST is a symbol to match against later on.  SKIP is an optional flag to
skip parsing the argument from Fennel to Emacs Lisp."
  (if (and (not skip)
           (stringp arg)
           (string-prefix-p "{" arg))
      (let ((hashlist
             (require-fennel--fennel-to-elisp
              (concat "[" (substring arg 1 -1) "]")))
            arglist)
        (dotimes (i (/ (length hashlist) 2))
          (push (cons (require-fennel--transform-arg (elt hashlist (* i 2)) rest t)
                      (require-fennel--transform-arg (elt hashlist (1+ (* i 2))) rest t))
                arglist))
        arglist)
    (pcase (if skip arg (require-fennel--fennel-to-elisp arg))
      ('... rest)
      ((and `((,_ . ,b) . ,_)
            (guard (not (listp b))))
       (let ((arglist nil))
         (dolist (kv arg)
           (cons (require-fennel--transform-arg (car kv) rest t)
                 (require-fennel--transform-arg (cdr kv) rest t)
                 arglist))
         arglist))
      ((and (or (pred listp) (pred vectorp)) args)
       (mapcar (lambda (arg) (require-fennel--transform-arg arg rest t)) args))
      ((pred stringp) (intern arg))
      (arg arg))))

(defun require-fennel--eval (fmt &rest args)
  "Send code to the Fennel process.
FMT is a format string, used with ARGS."
  (fennel-proto-repl-send-message-sync
   :eval
   (apply #'format fmt args)
   nil
   nil
   (or require-fennel-timeout most-positive-fixnum)))

(defun require-fennel--fn-arglist (var fn)
  "Obtain arglist from Fennel function FN stored in VAR."
  (seq-let (arglist)
      (require-fennel--eval
       "(fennel.metadata:get %s.%s :fnl/arglist)" var fn)
    (let ((rest (gensym "rest")))
      (when (and arglist (not (string= arglist "[]")))
        (let ((arglist (thread-last
                         arglist
                         require-fennel--fennel-to-elisp
                         (mapcar (lambda (arg) (require-fennel--transform-arg arg rest)))))
              result)
          (dolist (arg arglist)
            (if (not (equal arg rest))
                (push arg result)
              (push '&rest result)
              (push 'args result)))
          (reverse result))))))

(defun require-fennel--fn-docstring (var fn)
  "Obtain docstring from Fennel function FN stored in VAR."
  (seq-let (docstring)
      (require-fennel--eval "(fennel.metadata:get %s.%s :fnl/docstring)" var fn)
    (when docstring
      (require-fennel--fennel-to-elisp docstring))))

(defun require-fennel--module-definitions (var)
  "Obtain definitions from module stored in VAR."
  (thread-last
    `(if (= :table (type ,var))
         (icollect [k v (pairs ,var)]
           (if (= :function (type v))
               [":function" k]
             [":val" k]))
       (let [vartype (if (= :function (type ,var)) ":sole-function" ":sole-val")]
         (set ,var { ":value" ,var })
         [[vartype ":value"]]))
    (format "%s")
    (require-fennel--eval)
    car
    read-from-string
    car))

(defun require-fennel--handle-multivalue-return (values)
  "When multiple VALUES are returned from Fe, check if its more than one.
If it is, return all values as a list, otherwise return only the first
value."
  (if (length= values 1)
      (car values)
    values))

(defvar require-fennel--pprint
  "(tset _G.___repl___ :pp
     (fn pp [t keys]
       (let [module \"%s\"]
         (fn cons [t v]
           (doto (collect [k v (pairs (or t []))] k v)
             (table.insert v)))
         (fn length* [t]
           (let [mt (getmetatable t)]
             (if (and (= :table mt) mt.__len)
                 (mt.__len t)
                 (length t))))
         (fn pairs* [t]
           (let [mt (getmetatable t)]
             (if (and (= :table mt) mt.__pairs)
                 (mt.__pairs t)
                 (pairs t))))
         (fn ipairs* [t]
           (let [mt (getmetatable t)]
             (if (and (= :table mt) mt.__ipairs)
                 (mt.__ipairs t)
                 (ipairs t))))
         (match (type t)
           :table
           (let [len (length* t)
                 (nxt t* k) (pairs* t)]
             (if (not= nil (nxt t* (if (= len 0) k len)))
                 (.. \"#s(hash-table test equal data (\"
                     (table.concat
                      (icollect [k v (pairs* t)]
                        (string.format
                         \"%%s %%s\"
                         (pp k (cons keys (tostring k)))
                         (pp v (cons keys (tostring k)))))
                      \" \")
                     \"))\")
                 (> len 0)
                 (.. \"[\"
                     (table.concat (icollect [i v (ipairs* t)] (pp v (cons keys (tostring i)))) \" \")
                     \"]\")
                 \"[]\"))
           :function
           (let [err-sym (.. \"err_\" (math.random 100000) \"_\")
                 keys (table.concat keys \".\")]
             (tset %s keys t)
             (..
              \"(lambda (&rest args)\"
              \"  (with-current-buffer require-fennel--repl-buffer\"
              \"    (let* (\" err-sym \"\"
              \"           (values (fennel-proto-repl-send-message-sync\"
              \"                    :eval (format \\\"((. \" module \" \\\\\\\"\" keys \"\\\\\\\") %%s)\\\"\"
              \"                           (mapconcat #'require-fennel--elisp-to-fennel args \\\" \\\"))\"
              \"                    (lambda (_ msg trace) (setq \" err-sym \" (if trace (format \\\"%%s\n%%s\\\" msg trace) msg)))\"
              \"                    (lambda (data) (message \\\"%%s\\\" data)) (or require-fennel-timeout most-positive-fixnum))))\"
              \"      (if \" err-sym \" (error \" err-sym \")\"
              \"          (thread-last values\"
              \"            (mapcar (lambda (value) (require-fennel--fennel-to-elisp value nil)))\"
              \"            require-fennel--handle-multivalue-return)))))\"))
           :userdata
           (fennel.view (fennel.view t {:one-line? true}) {:one-line? true})
           _ (fennel.view t {:one-line? true})))))"
  "Format Fennel tables as elisp data on Fennel side.")

(defun require-fennel--setup-paths (directory)
  "Setup PATH environment for Fennel based on current DIRECTORY."
  (dolist (path `(("fennel" "path" ,(expand-file-name "?.fnl" directory))
                  ("fennel" "path" ,(expand-file-name "?/init.fnl" directory))
                  ("fennel" "macro-path" ,(expand-file-name "?.fnl" directory))
                  ("fennel" "macro-path" ,(expand-file-name "?/init.fnl" directory))
                  ("package" "path" ,(expand-file-name "?.lua" directory))
                  ("package" "path" ,(expand-file-name "?/init.lua" directory))))
    (seq-let (var field path) path
      (require-fennel--eval
       "(tset %s %S (.. %s.%s \";\" %S))" var field var field path))))

(defun require-fennel--define-const (field-type field as var separator use-hash-tables)
  "Generate `defconst' definition for FIELD.
If FIELD-TYPE is a string val, the nested definition is created using
SEPARATOR.  Otherwise, if the string is sole-val, AS is used as
definition.

See `require-fennel--define' for descriptions of AS, VAR, SEPARATOR,
USE-HASH-TABLES."
  (let ((name (pcase field-type
                ("val" (intern (format "%s%s%s" as separator field)))
                ("sole-val" (intern (format "%s" as))))))
    `(defconst ,name
       (with-current-buffer require-fennel--repl-buffer
         (require-fennel--fennel-to-elisp
          (thread-last
            ,(format "%s.%s" var field)
            require-fennel--eval
            car)
          ,use-hash-tables)))))

(cl-defun require-fennel--define-fun (fn-type fn as var separator use-hash-tables)
  "Generate `defun' definition for Fennel FN.
If FN-TYPE is a string function, the nested definition is created using
SEPARATOR.  Otherwise, if the string is sole-function, AS is used as
definition.

See `require-fennel--define' for descriptions of AS, VAR, SEPARATOR,
USE-HASH-TABLES."
  (let ((docstring (require-fennel--fn-docstring var fn))
        (arglist (require-fennel--fn-arglist var fn))
        (error-var (gensym "err"))
        (name (pcase fn-type
                ("function" (intern (format "%s%s%s" as separator fn)))
                ("sole-function" (intern (format "%s" as))))))
    `(defun ,name (&rest unknown-arguments)
       ,(or docstring "undocumented")
       ,(when arglist
          `(declare (advertised-calling-convention ,arglist "")))
       (with-current-buffer ,require-fennel--repl-buffer
         (let* (,error-var
                (values
                 (fennel-proto-repl-send-message-sync
                  :eval
                  (format ,(format "(%s.%s %%s)" var fn)
                          (mapconcat #'require-fennel--elisp-to-fennel unknown-arguments " "))
                  (lambda (err-type msg trace)
                    (setq ,error-var (if trace (format "%s\n%s" msg trace) msg)))
                  (lambda (data)
                    (message "%s" data))
                  (or require-fennel-timeout most-positive-fixnum))))
           (if ,error-var (error ,error-var)
             (thread-last
               values
               (mapcar (lambda (value) (require-fennel--fennel-to-elisp value ,use-hash-tables)))
               require-fennel--handle-multivalue-return)))))))

(defun require-fennel--define (value as var separator use-hash-tables)
  "Generate an ELisp definition for VALUE.
AS is the ELisp alias to give to the definition.  VAR is the Fennel-side
variable used to store the definition.  SEPARATOR is the separation
character used to concatenate the AS alias and the inner name of a
Fennel value, if any.  USE-HASH-TABLES determines if Fennel hash tables
should be converted to ELisp-native hash tables."
  (pcase-let ((`[,type ,field] value))
    (pcase type
      ((or "val" "sole-val")
       (require-fennel--define-const
        type field
        as var separator use-hash-tables))
      ((or "function" "sole-function")
       (require-fennel--define-fun
        type field
        as var separator use-hash-tables)))))

(defvar require-fennel--emacs-integration
  (thread-last
    "(let [format-elisp _G.___repl___.pp]
       (protocol.env-set!
        :emacs {:call (->> {:__index
                            (fn [_ k]
                              (fn [...]
                                (protocol.message
                                 [[:id {:sym protocol.id}]
                                  [:op {:string :require-fennel/call}]
                                  [:fun {:sym k}]
                                  [:arguments {:list (fcollect [n 1 (select \"#\" ...)]
                                                  (format-elisp (pick-values 1 (select n ...))))}]])
                                (. (protocol.receive) :data)))}
                           (setmetatable {}))
                :var (->> {:__index
                           (fn [_ k]
                             (protocol.message [[:id {:sym protocol.id}]
                                                [:op {:string :require-fennel/var}]
                                                [:var {:sym k}]])
                             (. (protocol.receive) :data))}
                          (setmetatable {}))
                :eval (fn [expr]
                        (protocol.message [[:id {:sym protocol.id}]
                                           [:op {:string :require-fennel/eval}]
                                           [:expr {:sym expr}]])
                        (. (protocol.receive) :data))})
       {:id 1000 :nop \"\"})"
    (replace-regexp-in-string "^ *" "")
    (replace-regexp-in-string "\n" " ")
    (format "%s\n")))

(defun require-fennel--replace-literal-newlines (data)
  "Replace literal newlines in a STRING with escaped ones."
  (if (stringp data)
      (replace-regexp-in-string "\n" "\\\\n" data)
    data))

(cl-defmethod fennel-proto-repl-handle-custom-op ((op (eql :require-fennel/call)) message callbacks)
  "Custom handler for the require-fennel/call OP.
Accepts a MESSAGE and its CALLBACKS.
The MESSAGE contains a function to evaluate and its arguments."
  (fennel-proto-repl-send-message
   nil
   (format "{:id %s :data %s}"
           (plist-get message :id)
           (require-fennel--replace-literal-newlines
            (require-fennel--elisp-to-fennel (apply (plist-get message :fun) (plist-get message :arguments)))))
   nil))

(cl-defmethod fennel-proto-repl-handle-custom-op ((op (eql :require-fennel/var)) message callbacks)
  "Custom handler for the require-fennel/var OP.
Accepts a MESSAGE and its CALLBACKS.
The MESSAGE contains a var which value is then returned to Fennel."
  (fennel-proto-repl-send-message
   nil
   (format "{:id %s :data %s}"
           (plist-get message :id)
           (require-fennel--replace-literal-newlines
            (require-fennel--elisp-to-fennel (eval (plist-get message :var)))))
   nil))

(cl-defmethod fennel-proto-repl-handle-custom-op ((op (eql :require-fennel/eval)) message callbacks)
  "Custom handler for the require-fennel/var OP.
Accepts a MESSAGE and its CALLBACKS.
The MESSAGE contains an expression which is evaluated and its result is returned to Fennel."
  (fennel-proto-repl-send-message
   nil
   (format "{:id %s :data %s}"
           (plist-get message :id)
           (require-fennel--replace-literal-newlines
            (require-fennel--elisp-to-fennel (eval (plist-get message :expr)))))
   nil))

;;;###autoload
(cl-defmacro require-fennel (module &key as (separator ".") (use-hash-tables require-fennel-use-hash-tables))
  "Require a Fennel MODULE as a set of Emacs Lisp definitions.
Keyword argument AS specifies how to prefix each definition.  Keyword
argument SEPARATOR specifies what character to use as a separator
between the AS prefix and Fennel function name.  Keyword argument
USE-HASH-TABLES forces tables to be returned as Emacs Lisp hash tables
instead of alists."
  (let* ((as (if as (format "%s" as)
               (replace-regexp-in-string "[.]" "-" (format "%s" module))))
         (var (gensym "fennel-elisp-"))
         (exports (format "%s-exports" var))
         (directory default-directory))
    `(prog1 ',(intern as)
       ,@(with-current-buffer (get-buffer-create require-fennel--repl-buffer)
           (unless (and (eq major-mode 'fennel-proto-repl-mode)
                        (process-live-p (get-buffer-process (current-buffer))))
             (save-window-excursion
               (fennel-proto-repl require-fennel-fennel-program (current-buffer))
               (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
               (set-process-query-on-exit-flag (get-buffer-process fennel-proto-repl--process-buffer) nil)
               (rename-buffer " *fennel-elisp*")))
           (require-fennel--eval "(local fennel (require :fennel))")
           (require-fennel--setup-paths directory)
           (fennel-proto-repl-send-message
            nil require-fennel--emacs-integration nil)
           (require-fennel--eval "(var %s (require :%s))" var module)
           (require-fennel--eval "(var %s {})" exports)
           (require-fennel--eval require-fennel--pprint exports exports)
           (thread-last
             var
             require-fennel--module-definitions
             (mapcar (lambda (value)
                       (require-fennel--define value as var separator use-hash-tables))))))))

(provide 'require-fennel)
;;; require-fennel.el ends here
