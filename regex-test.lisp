(in-package :regex-test)

#+nil
(progn
  (setf fiveam:*run-test-when-defined* t)
  (setf fiveam:*on-error* :debug))

(test regex-tests
  (is (scan "ab*c" "abbbbbbbbbc")))
