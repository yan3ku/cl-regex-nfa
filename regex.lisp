;;;; regex.lisp

(in-package #:regex)

(defstruct (state
            (:constructor make-state (&rest trans)))
  trans)

(defun add-trans (state trans)
  (setf (state-trans state) (append (state-trans state) (list trans))))

(defvar op-keyword
  '((#\*   . :star)
    (#\?   . :option)
    (#\|   . :alt)))

(defun is-op (chr)  (assoc chr op-keyword))
(defun is-sym (chr) (not (is-op chr)))


;;; PARSER
(defvar *regex-stream* nil)

(defun parse-regex-rec (depth out escaped)
  (let ((ch (read-char *regex-stream* nil)))
    (cond
      ((and (eql ch #\)) (zerop depth))
       (error "unbalanced parantheses"))
      ((and (not escaped) (eql ch #\|))
       (list :alt (nreverse out) (parse-regex-rec (1+ depth) '() nil)))
      ((and (not escaped) (is-op ch))
       (parse-regex-rec depth
                        (append (list (car out) (cdr (assoc ch op-keyword))) (cdr out))
                        nil))
      ((eql ch #\()
       (let ((subregex (parse-regex-rec (1+ depth) '() nil)))
         (parse-regex-rec depth (cons subregex out) escaped)))
      ((or (null ch) (eql ch #\)))
       (nreverse out))
      (t (parse-regex-rec depth (cons ch out) nil)))))

(defun parse-regex (regex)
  "Parse regex string to prefix Sexp."
  (with-input-from-string (*regex-stream* regex)
    (parse-regex-rec 0 '() nil)))


;;; COMPILATION
(defvar *dangling* nil)
(defvar *new-dangling* nil)

(defmacro save-danglings (&body body)
  "Use collected danglings until now for next patch."
  `(let ((*dangling* *new-dangling*))
     (setf *new-dangling* nil)
     ,@body))

(defun patch-danglings (state)
  "Set transition of (saved) danglings to state."
  (dolist (out *dangling*)
    (rplacd out state)))

(defgeneric compile-nfa (kind rest)
  (:documentation "Build NFA from sexp representing regex in prefix form:
(:ALT (:STAR #\e #\a) (#\a)) for e*a|a?
Return the start state.")
  (:method :around ((kind character) (rest (eql nil))) ; the last character to compile (dangling arrow)
    (let ((last (call-next-method kind rest)))
      (assert (= 1 (length (state-trans last))))
      (push (car (state-trans last)) *new-dangling*) ;save it
      last))
  (:method ((kind character) rest)
    (make-state
     (cons kind (compile-nfa (car rest) (cdr rest)))))
  (:method ((kind (eql :alt)) rest)     ;e|e
    (let ((start (make-state)))         ;compile as separate nfa
      (dolist (sub rest)
        (add-trans start (cons t (compile-nfa (car sub) (cdr sub)))))
      start))
  (:method ((kind (eql :option)) rest)  ;e?
    (let ((option-nfa (compile-nfa (car rest) nil))) ;optional nfa
      (let ((follow-nfa nil))
        (save-danglings
          (setf follow-nfa (compile-nfa (cadr rest) (cddr rest))) ;following nfa
          (patch-danglings follow-nfa))
        (make-state
         (cons t option-nfa)
         (cons t follow-nfa)))))
  (:method ((kind (eql :star)) rest)    ;e*
    (let ((start (make-state)))
      (add-trans start (cons t (compile-nfa (car rest) nil))) ;repetition nfa
      (save-danglings
        (add-trans start (cons t (compile-nfa (cadr rest) (cddr rest)))) ;following nfa (when skipped)
        (patch-danglings start))
      start))
  (:method ((kind list) rest)           ;(abc)d - new regex group
    ;; this is regex group followed by something that will be compiled and linked (patched) to the group
    (let ((start (compile-nfa (car kind) (cdr kind))))
      (save-danglings
        (let ((tail (compile-nfa (car rest) (cdr rest))))
          (patch-danglings tail)))
      start))
  (:method ((kind list) (rest (eql nil))) ;dont override danglings for :option (#\a)
    (let ((start (compile-nfa (car kind) (cdr kind))))
      ;; (print kind)
      start))
  (:method ((kind (eql nil)) rest)      ;end of regex
    :end))


;;; EXECUTION
(defgeneric skip-t (trans)
  (:method ((trans (eql :end)))
    (list (cons t :end)))
  (:method ((trans state))
    (skip-t (state-trans trans)))
  (:method ((trans list))
    (let ((reachable))
      (dolist (acons trans reachable)
        (if (eql t (car acons))
            (dolist (found (skip-t (cdr acons)))
              (push found reachable))
            (push acons reachable))))))

(defun find-reachable (ch states)
  (let ((reachable))
    (dolist (state states reachable)
      (dolist (found (skip-t state))
        (when (or (eql (car found) ch) (eql (car found) t))
          (push (cdr found) reachable))))))

(defun match (regex str)
  "Execute the NFA."
  (loop
    :for i :from 0
    :for ch :across str
    :for reachable = (find-reachable ch (list regex))
      :then (find-reachable ch reachable)
    :when (member :end (find-reachable t reachable)) :do
      (return i)
    :while reachable))

(defun scan (regex-str str)
  (loop
    :with regex = (compile-regex regex-str)
    :for i :from 0 :to (length str) :do
      (let ((end (match regex (subseq str i))))
        (when end
          (return (cons i (+ 1 i end)))))))

(defun compile-regex (regex)
  (let ((*dangling* nil))
    (compile-nfa (parse-regex regex) nil)))
