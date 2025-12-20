;;;; regex.lisp

(in-package #:regex)

(defstruct (state
            (:constructor make-state (&rest trans)))
  "State which list of (cond . state) pairs representing edges in the directed graph of NFA. cond of T is used for epsilon transition."
  trans)

(defun add-trans (state trans)
  (setf (state-trans state) (append (state-trans state) (list trans))))

(defvar operator-char
  '((#\*   . :star)
    (#\?   . :qmark))
  "List of postfix operators.")

(defvar special-char
  '((#\^  . :caret)
    (#\$  . :dolar)
    (#\.  . :dot))
  "List of special characters in regex.")

(defun is-op (chr)  (assoc chr operator-char))
(defun is-spec (chr)(assoc chr special-char))
(defun is-sym (chr) (not (is-op chr)))


;;; PARSER
(defvar *regex-stream* nil)

(defun parse-regex-rec (depth out &optional (escaped nil))
  (let ((ch (read-char *regex-stream* nil)))
    (cond
      ((and (eql ch #\)) (zerop depth))
       (error "unbalanced closing parantheses"))
      ((and (null ch) (not (zerop depth)))
       (error "unbalanced opening parantheses"))
      ((eql ch #\()
       (let ((subregex (parse-regex-rec (1+ depth) '())))
         (parse-regex-rec depth (cons subregex out))))
      ((and (eql ch #\\))
       (parse-regex-rec depth
                        (if escaped (cons #\\ out) out)
                        (not escaped)))
      ((or (null ch) (eql ch #\)))
       (nreverse out))
      ((not escaped)
       (cond
         ((is-spec ch)
          (parse-regex-rec depth (cons (cdr (assoc ch special-char)) out)))
         ((eql ch #\|)
          (list :pipe (nreverse out) (parse-regex-rec depth '())))
         ((is-op ch)
          (parse-regex-rec depth
                           (append (list (car out) (cdr (assoc ch operator-char))) (cdr out))
                           nil))
         (t (parse-regex-rec depth (cons ch out)))))
      (t    (parse-regex-rec depth (cons ch out))))))

(defun parse-regex (regex)
  "Parse regex string to prefix Sexp."
  (with-input-from-string (*regex-stream* regex)
    (parse-regex-rec 0 '())))


;;; COMPILATION
(defvar *dangling* nil)
(defvar *new-dangling* nil)

(defmacro save-danglings (&body body)
  "Use collected danglings until now for next patch."
  `(let ((*dangling* *new-dangling*))
     (setf *new-dangling* nil)
     ,@body))

(defun patch-danglings (state)
  "Set transition of (saved) danglings to state.
When reached ending state eg: for (d*|) we need to save the danglings of the d* expression.
The cdr is already :end so this would only result in lost of the danglings and nfa ending too fast."
  (if (eql state :end)
      (setf *new-dangling* *dangling*)
      (dolist (out *dangling*)
        (rplacd out state))))

(defgeneric make-trans (cond state)
  (:documentation "Make transition and if reached ending state register it in *dangling* for patching.")
  (:method (cond state)
    (cons cond state))
  (:method :around (cond (state (eql :end)))
    (let ((cons (call-next-method cond state)))
      (push cons *new-dangling*) ;save it
      cons)))

(defgeneric compile-nfa (kind rest)
  (:documentation "Build NFA from sexp representing regex in prefix form:
(:PIPE (:STAR #\e #\a) (#\a)) for e*a|a?
Return the start state.")
  (:method ((kind character) rest)
    (make-state
     (make-trans kind (compile-nfa (car rest) (cdr rest)))))
  (:method ((kind symbol) rest)
    (make-state
     (make-trans kind (compile-nfa (car rest) (cdr rest)))))
  (:method ((kind (eql :pipe)) rest)     ;e|e
    (let ((join-dangling))              ; the dangling out is the concatenation of each sub-regex danglings
      (let ((start (make-state)))        ;compile as separate nfa
        (dolist (sub rest)
          (add-trans start (make-trans t (compile-nfa (car sub) (cdr sub))))
          (setf join-dangling (append join-dangling *new-dangling*))
          (setf *new-dangling* nil))
        (setf *new-dangling* join-dangling)
        start)))
  (:method ((kind (eql :qmark)) rest)  ;e?
    (let ((qmark-nfa (compile-nfa (car rest) nil))) ;optional nfa
      (let ((follow-nfa nil))
        (save-danglings
          (setf follow-nfa (compile-nfa (cadr rest) (cddr rest))) ;following nfa
          (patch-danglings follow-nfa))
        (make-state
         (make-trans t qmark-nfa)
         (make-trans t follow-nfa)))))
  (:method ((kind (eql :star)) rest)    ;e*
    (let ((start (make-state)))
      (add-trans start (make-trans t (compile-nfa (car rest) nil))) ;repetition nfa
      (save-danglings
        (add-trans start (make-trans t (compile-nfa (cadr rest) (cddr rest)))) ;following nfa (when skipped)
        (patch-danglings start))
      start))
  (:method ((kind list) rest)           ;(abc)d - new regex group
    ;; this is regex group followed by something that will be compiled and linked (patched) to the group
    ;; if rest is nil this can result in lost of danglings for kind expression, that's why we need guard
    (let ((start (compile-nfa (car kind) (cdr kind))))
      (when rest
        (save-danglings
          (let ((tail (compile-nfa (car rest) (cdr rest))))
            (patch-danglings tail))))
      start))
  (:method ((kind (eql nil)) rest)      ;end of regex
    :end))


;;; EXECUTION
(defvar *visited* nil)

(defgeneric follow-epsilon (states)
  (:documentation "From list of states return list of terminal edges when following the epsilon transition.")
  (:method ((state (eql :end)))
    (list (cons t :end)))
  (:method ((state state))
    (if (gethash state *visited*)
        nil
        (progn
          (setf (gethash state *visited*) t)
          (follow-epsilon (state-trans state)))))
  (:method ((trans list))
    (let ((reachable))
      (dolist (acons trans reachable)
        (if (eql t (car acons))
            (dolist (found (follow-epsilon (cdr acons)))
              (push found reachable))
            (push acons reachable))))))

(defun follow (cond states &key start-p end-p)
  "Return list of states that are reachable following epsilon transitions and then consuming the character."
  (let ((reachable))
    (dolist (state states reachable)
      (let ((*visited* (make-hash-table)))
        (dolist (found (follow-epsilon state))
          (cond
            ((and (eql (car found) :dolar) end-p)
             (setf reachable (append reachable
                                     (follow cond (list (cdr found)) :start-p nil :end-p end-p))))
            ((and (eql (car found) :caret) start-p)
             (setf reachable (append reachable
                                     (follow cond (list (cdr found)) :start-p nil :end-p end-p))))
            ((and (eql (car found) :dot) (characterp cond) (alpha-char-p cond))
             (push (cdr found) reachable))
            ((eql (car found) cond)
             (push (cdr found) reachable))))))))

(defun reached-end-p (states &key start-p end-p)
  (member :end (append (follow t states :start-p start-p :end-p end-p))))

(defun match-regex (regex str &key (start 0) (end (1- (length str))))
  (let ((last-success nil))
    (if (reached-end-p (list regex) :start-p t :end-p nil)
        (setf last-success -1))         ;matches empty string
    (loop
      :for i :from start :upto end
      :for ch = (aref str i)
      :for start-p = (= i start)
      :for end-p   = (= i end)
      :for reachable = (follow ch (list regex) :start-p start-p :end-p end-p)
        :then (follow ch reachable :start-p start-p :end-p end-p)
      :while reachable
      :when (reached-end-p reachable :start-p start-p :end-p end-p) :do
        (setf last-success (1+ i))
      :do (setf reachable (remove :end reachable)))
    last-success))

(defun scan (regex-str str)
  (loop
    :with regex = (compile-regex regex-str)
    :for i :from 0 :to (length str) :do
      (let ((end (match-regex regex str :start i)))
        (when (and end (plusp end))
          (return (cons i end))))))

(defun scanstr (regex-str str)
  (let ((found (scan regex-str str)))
    (when found
      (subseq str (car found) (cdr found)))))

(defun compile-regex (regex)
  (let ((*new-dangling* nil))
    (compile-nfa (parse-regex regex) nil)))
