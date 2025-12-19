;;;; regex.lisp

(in-package #:regex)

(defstruct (state (:constructor make-state (&rest trans)))
  trans)

(defun add-trans (state trans)
  (setf (state-trans state) (append (state-trans state) (list trans))))

(defvar op-keyword
  '((#\*   . :star)
    (#\?   . :option)
    (#\|   . :alt)))

(defun is-op (chr)  (assoc chr op-keyword))
(defun is-sym (chr) (not (is-op chr)))

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
                        (append (list (list (car out)) (cdr (assoc ch op-keyword))) (cdr out))
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

(defvar *dangling* nil)
(defvar *saved-dangling* nil)

(defmacro save-danglings (&body body)
  "Save current dangling value for next patch-saved-dangling call.
After body finishes overwrite the *dangling* with danglings collected inside body.
The most recent danglings (the ondes collected from inside body) are the ones that should be patched next,
higher in call stack of compile-nfa. e.g: for a(bc)d the dangling for c should be saved when exiting the (bc) group so it will be patched to d."
  `(setf *dangling* (let ((*saved-dangling* *dangling*)
                          (*dangling* nil))
                      ,@body
                      *dangling*)))

(defun patch-saved-danglings (state)
  "Set transition of dangling arrow to state."
  (dolist (out *saved-dangling*)
    (rplacd out state)))

(defgeneric compile-nfa (kind rest)
  (:documentation "Build NFA from sexp representing regex in prefix form:
(:ALT (:STAR (#\e) #\a) (:OPTION (#\a))) for e*a|a?
Return the start state.")
  (:method :around ((kind character) (rest (eql nil))) ; the last character to compile (dangling arrow)
    (let ((last (call-next-method kind rest)))
      ;; save the dangling arrows
      (loop :for trans :in (state-trans last) :do
        (push trans *dangling*))
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
          (patch-saved-danglings follow-nfa))
        (make-state
         (cons t option-nfa)
         (cons t follow-nfa)))))
  (:method ((kind (eql :star)) rest)    ;e*
    (let ((start (make-state)))
      (add-trans start (cons t (compile-nfa (car rest) nil))) ;repetition nfa
      (save-danglings
        (add-trans start (cons t (compile-nfa (cadr rest) (cddr rest)))) ;following nfa (when skipped)
        (patch-saved-danglings start))
      start))
  (:method ((kind list) rest)           ;(abc)d - new regex group
    ;; this is regex group followed by something that will be compiled and linked (patched) to the group
    (let ((start (compile-nfa (car kind) (cdr kind))))
      (save-danglings
        (let ((tail (compile-nfa (car rest) (cdr rest))))
          (patch-saved-danglings tail)))
      start))
  (:method ((kind list) (rest (eql nil))) ;dont override danglings for :option (#\a)
    (let ((start (compile-nfa (car kind) (cdr kind))))
      start))
  (:method ((kind (eql nil)) rest)      ;end of regex
    :end))

(defun find-reachable (ch edges)
  "Return list of reachable edges for (ch)aracter in directed graph."
  (let ((reachable))
    (dolist (trans edges reachable)
      (if (eql t (car trans))
          (if (eql (cdr trans) :end)
              (push :end reachable)
              (let ((follow (find-reachable ch (state-trans (cdr trans)))))
                (setf reachable (append reachable follow))))
          (when (eql ch (car trans))
            (if (eql (cdr trans) :end)
                (push :end reachable)
                (setf reachable (append reachable (state-trans (cdr trans))))))))))

(defun match (regex str)
  "Execute the NFA."
  (if (member :end (find-reachable t (state-trans regex))) ;check if empty input matches
      t
      (loop
        :for ch :across str
        :for reachable = (find-reachable ch (state-trans regex))
          :then (find-reachable ch reachable)
        :when (member :end reachable) :do
          (return t)
        :while reachable)))

(defun compile-regex (regex)
  (let ((*dangling* nil))
    (compile-nfa (parse-regex regex) nil)))
