(defmacro loop-with-neighbours ((loop-var repeat-size 
                                &optional &key (start 0) (increment 1))
                                &body body)
"Loops the variable 'loop-var' from 'start' adding 'increment' each
time, and stopping when the variable is equal to or greater than
'repeat-size' (the body is not executed when the variable meets this
condition).  At the same time, variables appear in scope, with the
name of 'loop-var' and appended with '+' or '-'.  These hold,
respectively, one more than and one less than the 'loop-var', except
at the borders, where it loops around.  Looping 'i' to 10 would create
a variable 'i-' which starts at 9, then goes to 0 through 8."
  (let ((loop+ (gensym)) (loop- (gensym)))
    (setf loop+ (intern (format nil "~A+" loop-var)))
    (setf loop- (intern (format nil "~A-" loop-var)))

    `(do* ((,loop-var ,start (incf ,loop-var ,increment))
           (,loop+ (mod (1+ ,loop-var) ,repeat-size) (mod (incf ,loop+ ,increment) ,repeat-size))
           (,loop- (mod (1- ,loop-var) ,repeat-size) (mod (incf ,loop- ,increment) ,repeat-size)))
      ((>= ,loop-var ,repeat-size))
      ,@body)))
