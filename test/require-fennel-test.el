;;; -*- lexical-binding: t -*-

(require 'require-fennel)
(require 'ht)
(require 'ert)

(ert-deftest load-fennel ()
  (should (require-fennel fennel)))

(ert-deftest data-conversion-send-test ()
  (require-fennel data.simple :as simple)
  (should (equal 1 (simple.identity 1)))
  (should (equal "a" (simple.identity "a")))
  (should (equal "a" (simple.identity :a)))
  (should (equal t (simple.identity t)))
  (should (equal nil (simple.identity 'false)))
  (should (equal nil (simple.rest nil)))
  (should (equal '(nil nil) (simple.rest nil nil nil)))
  (should (null (simple.identity '())))
  (should (equal ["a"] (simple.identity '(:a))))
  (should (equal ["a" "b"] (simple.identity '(:a :b))))
  (should (equal ["a" "b" "c"] (simple.identity '(:a :b :c))))

  (should (equal [] (simple.identity [])))
  (should (equal ["a"] (simple.identity [:a])))
  (should (equal ["a" "b"] (simple.identity [:a :b])))
  (should (equal ["a" "b" "c"] (simple.identity [:a :b :c])))

  (should-not (simple.nothing 2))
  (should (equal '(2 3) (simple.rest 1 2 3)))
  (should (equal '(("bar" . 1))
                 (simple.table (ht (:foo 1)))))
  (should (equal '(("bar" . 1))
                 (simple.table '((:foo . 1)))))
  (should (equal ["bar" "foo"] (simple.vector [:foo :bar])))
  (should (equal ["bar" "foo"] (simple.vector '("foo" "bar")))))

(ert-deftest hash-table-conversion-send-test ()
  (require-fennel data.simple :as simple :use-hash-tables t)
  (should (simple.identity '((:a . 1))))
  (should (simple.identity '((:a . 1) (:b . 1))))
  (should (simple.identity '((:a . 1) (:b . 1) (:c . 1))))
  (should (simple.identity '((:a . 1) (:b . 1) (:c . 1) (:d . 4))))

  (should (equal [] (simple.identity (ht))))
  (should (simple.identity (ht (:a 1))))
  (should (simple.identity (ht (:a 1) (:b 2))))
  (should (simple.identity (ht (:a 1) (:b 2) (:c 3))))
  (should (simple.identity (ht (:a 1) (:b 2) (:c 3) (:d 4))))

  (should (simple.identity (ht (:a (ht (:a 1))))))
  (should (simple.identity (ht (:a (ht (:a 1))) (:b (ht (:a (ht (:a 1))) (:b 2))))))
  (should (simple.identity (ht (:a (ht (:a 1))) (:b (ht (:a (ht (:a 1))) (:b 2))) (:c (ht (:a (ht (:a 1))) (:b (ht (:a (ht (:a 1))) (:b 2))))))))
  (should (simple.identity (ht (:a (ht (:a 1))) (:b (ht (:a (ht (:a 1))) (:b 2))) (:c (ht (:a (ht (:a 1))) (:b (ht (:a (ht (:a 1))) (:b 2))))) (:d (ht (:a (ht (:a 1))) (:b (ht (:a (ht (:a 1))) (:b 2))) (:c (ht (:a (ht (:a 1))) (:b (ht (:a (ht (:a 1))) (:b 2))))))))))

  (should (simple.identity '((:a . 1))))
  (should (simple.identity '((:a . 1) (:b . 1))))
  (should (simple.identity '((:a . 1) (:b . 1) (:c . 1))))
  (should (simple.identity '((:a . 1) (:b . 1) (:c . 1) (:d . 4)))))

(ert-deftest alist-conversion-send-test ()
  (require-fennel data.simple :as simple)
  (should (simple.identity '((:a . 1))))
  (should (simple.identity '((:a . 1) (:b . 1))))
  (should (simple.identity '((:a . 1) (:b . 1) (:c . 1))))
  (should (simple.identity '((:a . 1) (:b . 1) (:c . 1) (:d . 4))))

  (should (equal [] (simple.identity (ht))))
  (should (simple.identity (ht (:a 1))))
  (should (simple.identity (ht (:a 1) (:b 2))))
  (should (simple.identity (ht (:a 1) (:b 2) (:c 3))))
  (should (simple.identity (ht (:a 1) (:b 2) (:c 3) (:d 4))))

  (should (simple.identity (ht (:a (ht (:a 1))))))
  (should (simple.identity (ht (:a (ht (:a 1))) (:b (ht (:a (ht (:a 1))) (:b 2))))))
  (should (simple.identity (ht (:a (ht (:a 1))) (:b (ht (:a (ht (:a 1))) (:b 2))) (:c (ht (:a (ht (:a 1))) (:b (ht (:a (ht (:a 1))) (:b 2))))))))
  (should (simple.identity (ht (:a (ht (:a 1))) (:b (ht (:a (ht (:a 1))) (:b 2))) (:c (ht (:a (ht (:a 1))) (:b (ht (:a (ht (:a 1))) (:b 2))))) (:d (ht (:a (ht (:a 1))) (:b (ht (:a (ht (:a 1))) (:b 2))) (:c (ht (:a (ht (:a 1))) (:b (ht (:a (ht (:a 1))) (:b 2))))))))))

  (should (simple.identity '((:a . 1))))
  (should (simple.identity '((:a . 1) (:b . 1))))
  (should (simple.identity '((:a . 1) (:b . 1) (:c . 1))))
  (should (simple.identity '((:a . 1) (:b . 1) (:c . 1) (:d . 4)))))

(ert-deftest data-conversion-receive-test ()
  (defvar ns (require-fennel data.simple :as simple))
  (should (equal 4 simple.four))
  (should simple.true)
  (should-not simple.false)
  (should (equal '(("foo" . 1)) simple.plain-table))
  (should (equal ["foo" 1] simple.plain-vector)))

(ert-deftest loading-single-function-test ()
  (require-fennel data.function)
  (should (string= "anon" (data-function)))
  (require-fennel data.function :as f)
  (should (string= "anon" (f))))

(ert-deftest loading-single-value-test ()
  (require-fennel data.value)
  (should (equal 42 data-value))
  (require-fennel data.value :as v)
  (should (equal 42 v)))
