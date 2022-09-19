(defpackage emacs-update/tests/main
  (:use :cl
        :emacs-update
        :rove))
(in-package :emacs-update/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :emacs-update)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
