;; The following file presents the code required to perform a discrimination game

;; ******************************************************************************
;; macrolet taken from https://github.com/sharplispers/split-sequence
;; Public domain license
(macrolet ((check-bounds (sequence start end)
             (let ((length (gensym (string '#:length))))
               `(let ((,length (length ,sequence)))
                  (check-type ,start unsigned-byte "a non-negative integer")
                  (when ,end (check-type ,end unsigned-byte "a non-negative integer or NIL"))
                  (unless ,end
                    (setf ,end ,length))
                  (unless (<= ,start ,end ,length)
                    (error "Wrong sequence bounds. start: ~S end: ~S" ,start ,end))))))
  
  (defun split-sequence (delimiter sequence &key (start 0) (end nil) (from-end nil)
                                              (count nil) (remove-empty-subseqs nil)
                                              (test #'eql) (test-not nil) (key #'identity))
    "Return a list of subsequences in seq delimited by delimiter.
If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
    (check-bounds sequence start end)
  (cond
    ((and (not from-end) (null test-not))
     (split-from-start (lambda (sequence start)
                         (position delimiter sequence :start start :key key :test test))
                       sequence start end count remove-empty-subseqs))
    ((and (not from-end) test-not)
     (split-from-start (lambda (sequence start)
                         (position delimiter sequence :start start :key key :test-not test-not))
                       sequence start end count remove-empty-subseqs))
    ((and from-end (null test-not))
     (split-from-end (lambda (sequence end)
                       (position delimiter sequence :end end :from-end t :key key :test test))
                     sequence start end count remove-empty-subseqs))
    ((and from-end test-not)
     (split-from-end (lambda (sequence end)
                       (position delimiter sequence :end end :from-end t :key key :test-not test-not))
                     sequence start end count remove-empty-subseqs))))
  (defun split-from-start (position-fn sequence start end count remove-empty-subseqs)
  (let ((length (length sequence)))
    (loop
       :for left := start :then (+ right 1)
       :for right := (min (or (funcall position-fn sequence left) length)
                          end)
       :unless (and (= right left)
                    remove-empty-subseqs) ; empty subseq we don't want
       :if (and count (>= nr-elts count))
       ;; We can't take any more. Return now.
       :return (values subseqs left)
       :else
       :collect (subseq sequence left right) :into subseqs
       :and :sum 1 :into nr-elts
       :until (>= right end)
       :finally (return (values subseqs right))))))

;; Function used to replace values on string
(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

;; Function used to convert a string line into a list of numbers
(defun line-to-numbers (input-line)
  "Given a line read from a file as a string, gives back a list with
the real numbers corresponding to the string values"
  (with-input-from-string (in (replace-all input-line "," ""))
    (loop for x = (read in nil nil) while x collect x)))

;; Definition of structure to manage a file input object
(defstruct (input-object)
  (x)
  (y)
  (z)
  (width)
  (height)
  (avg-y)
  (stdv-y)
  (min-y)
  (max-y)
  (avg-u)
  (stdv-u)
  (min-u)
  (max-u)
  (avg-v)
  (stdv-v)
  (min-v)
  (max-v))

;; Gets a list with the contents of the object and returns an input-object based on it.
(defun input-object-from-list(list-args)
  (setq input-obj (make-input-object  :x (nth 0 list-args) :y (nth 1 list-args) :z (nth 2 list-args) :width (nth 3 list-args) :height (nth 4 list-args) :avg-y (nth 5 list-args) :avg-u (nth 6 list-args) :avg-v (nth 7 list-args) :min-y (nth 8 list-args) :min-u (nth 9 list-args) :min-v (nth 10 list-args) :max-y (nth 11 list-args) :max-u (nth 12 list-args) :max-v (nth 13 list-args) :stdv-y (nth 14 list-args) :stdv-u (nth 15 list-args) :stdv-v (nth 16 list-args)))
  input-obj)

;; Definition of function to load situation data
(defun read-file (filename)
  "Function that reads a file line by line returning the 
list of lines as strings."
  (let ((result '()))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil)
         while line
         do(setq result (append result (list (input-object-from-list (line-to-numbers line)))))))
    result))

;; Definition of variable to hold the list of objects that represent the input.
(defparameter *input* (read-file "~/Documents/vub/programming_paradigms/dev/lisp_code/assignment_1/assignment-1-data/qrio-1/object-features.txt"))

;; Get random object from situation data
(defparameter *number-of-situation-objects* 1643)
(defun get-random-situation-object()
  "Gets a random situation object from the ones loaded from the file"
  (nth (+ (random *number-of-situation-objects* (make-random-state t)) 1) *input*))

;; Definition of game parameters
(defparameter *number-of-objects-per-scene* 5)
(defparameter *total-number-of-games* 500)
(defparameter *game-channels* '("x" "y" "width" "height" "v-avg" "u-avg" "y-avg"))

;; Function that allows to get a random set of objects to make a game
(defun get-random-scene-objects()
  "Gets a random set of situational objects to execute a game based on it"
  (let ((result '()))
  (loop for i from 1 to *number-of-objects-per-scene* do
       (setq obj (get-random-situation-object))
       (if(eq (member obj result) nil)
          (setq result (append result (list obj))))
       )
  result))

;; Definition of the structure for a scene
(defstruct scene
  (objects)
  (topic))

;; Definition of the structure for the data contained in the tree nodes
(defstruct node-data
  (score)
  (game-number))

;; Definition of the binary-tree node structure
(defstruct binary-tree-node
  (data)
  (left-child)
  (right-child))

;; Definition of the structure for the sensory channels
(defstruct sensory-channel
  (feature-name)
  (lower-bound)
  (upper-bound)
  (discrimination-tree))

;; Definition of the main agent structure
(defstruct agent
  (feature-detectors))



;; Function to create a scene using the random objects. The topic is removed from the
;; list of objects in the scene to avoid redundant comparisons.
(defun create-scene()
  "creates a scene for the game"
  (let
      ((objs (get-random-scene-objects)))
    (setq tpc (nth (random *number-of-objects-per-scene* (make-random-state t)) objs))
    (setq scene (make-scene :objects (remove tpc objs) :topic tpc))
    scene
    ))

;; Function to run a single discrimination game
(defun run-single-game()
  (let ((scene (create-scene)))
    ))

;; Definition of the game. Main function to run.
(defun exec-game()
  (loop for i from 0 to *total-number-of-games* do
       ()))
