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
       :finally (return (values subseqs right)))))
  (defun split-from-end (position-fn sequence start end count remove-empty-subseqs)
  (loop
     :for right := end :then left
     :for left := (max (or (funcall position-fn sequence right) -1)
                       (1- start))
     :unless (and (= right (1+ left))
                  remove-empty-subseqs) ; empty subseq we don't want
     :if (and count (>= nr-elts count))
     ;; We can't take any more. Return now.
     :return (values (nreverse subseqs) right)
     :else
     :collect (subseq sequence (1+ left) right) into subseqs
     :and :sum 1 :into nr-elts
     :until (< left start)
     :finally (return (values (nreverse subseqs) (1+ left))))))

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

;; Call the accessor function of the inpu-object structure based on a string
(defun get-input-object-value (input-object slot-name)
  "Given a string for the slot name gets the value using the accessor function 
of the structure for the given input object"
  (funcall (symbol-function (find-symbol (string-upcase (concatenate 'string "input-object-" slot-name)))) input-object))

;; Gets a list with the contents of the object and returns an input-object based on it.
(defun input-object-from-list(list-args)
  (let ((input-obj (make-input-object  :x (nth 0 list-args) :y (nth 1 list-args) :z (nth 2 list-args) :width (nth 3 list-args) :height (nth 4 list-args) :avg-y (nth 5 list-args) :avg-u (nth 6 list-args) :avg-v (nth 7 list-args) :min-y (nth 8 list-args) :min-u (nth 9 list-args) :min-v (nth 10 list-args) :max-y (nth 11 list-args) :max-u (nth 12 list-args) :max-v (nth 13 list-args) :stdv-y (nth 14 list-args) :stdv-u (nth 15 list-args) :stdv-v (nth 16 list-args))))
  input-obj))

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
(defparameter *total-number-of-games* 10)
(defparameter *game-channels* '("x" "y" "z" "width" "height" "avg-y" "stdv-y" "min-y" "max-y" "avg-u" "stdv-u" "min-u" "max-u" "avg-v" "stdv-v" "min-v" "max-v"))
(defparameter *current-game* 0)

;; Function that allows to get a random set of objects to make a game
(defun get-random-scene-objects()
  "Gets a random set of situational objects to execute a game based on it"
  (let ((result '()))
    (loop for i from 1 to *number-of-objects-per-scene* do
       (let ((obj (get-random-situation-object)))
         (if(eq (member obj result) nil)
          (setq result (append result (list obj)))
          (setq i (- i 1))) 
       ))
  result))

;; Definition of the structure for a scene
(defstruct scene
  (objects)
  (topic))

;; Definition of the structure for the data contained in the tree nodes
(defstruct node-data
  (score 0)
  (game-number)
  (lower-bound)
  (upper-bound))

;; Definition of the binary-tree node structure
(defstruct tree-node
  (data)
  (parent)
  (left-child)
  (right-child))

;; Definition of the structure for the sensory channels
(defstruct sensory-channel
  (feature-name)
  (discrimination-tree))

;; Definition of the main agent structure
(defstruct agent
  (feature-detectors (init-sensory-channels)))

;; Structure to represent the result of a discrimination
(defstruct discrimination-result
  (successful)
  (feature-name)
  (discrimination-node))

;; Function to get the root of a tree
(defun get-root()
  (make-tree-node :data (make-node-data :score 0.0 :game-number 0 :lower-bound 0.0 :upper-bound 1.0)))


;; Gets the root of the tree for the current node based on its ancestors
(defun find-root(node)
  (if(eq (tree-node-parent node) nil)
     node
  (find-root (tree-node-parent node))))

;; Function to initialize the sensory channels for the agent
(defun init-sensory-channels()
  (let ((channels '()))
    (loop for c in *game-channels* do
         (setq channels (append channels (list (make-sensory-channel :feature-name c :discrimination-tree (get-root))))))
    channels))

;; Function to create a scene using the random objects. The topic is removed from the
;; list of objects in the scene to avoid redundant comparisons.
(defun create-scene()
  "creates a scene for the game"
  (let* ((objs (get-random-scene-objects)) (tpc (nth (random *number-of-objects-per-scene* (make-random-state t)) objs)) (scene (make-scene :objects (remove tpc objs) :topic tpc)))
    scene
    ))

;; Gets the lower bound of a node, basic wrapper for slot accessor
(defun get-lower-bound(node)
  "Given a node gets its lower bound contained in its data"
  (node-data-lower-bound (tree-node-data node)))

;; Gets the upper bound of a node, basic wrapper for slot accessor
(defun get-upper-bound(node)
  "Given a node gets its upper bound contained in its data"
  (node-data-upper-bound (tree-node-data node)))

;; Function in charge of expand (bifurcate) a node creating new children for it
(defun expand-node (node)
  "Expands a node's children dividing its interval in two"
  (setf (tree-node-left-child node) (make-tree-node :data (make-node-data :lower-bound (get-lower-bound node) :upper-bound (/ (+ (get-lower-bound node) (get-upper-bound node)) 2.0))))
  (setf (tree-node-right-child node) (make-tree-node :data (make-node-data :lower-bound (/ (+ (get-lower-bound node) (get-upper-bound node)) 2.0) :upper-bound (get-upper-bound node))))
  node)

;; Function to get a random child 
(defun random-child (node)
  (if(eq (random 2 (make-random-state t)) 0)
     (tree-node-left-child node)
     (tree-node-right-child node)))

;; Helper for random-expand, in charge of expanding a random leaf
(defun random-expand-helper(node)
  "Expands a random leaf in a given tree"
  (if(and (not (eq (tree-node-left-child node) nil)) (not (eq (tree-node-right-child node) nil)))
     (random-expand-helper (random-child node))
     (expand-node node)))

;; Randomly expands a leaf in a tree
(defun random-expand (node)
  "Expands a random leaf in a tree and returns the modified structure"
  (random-expand-helper node)
  node)

;; Function to choose a random channel and call the method in charge of node expansion
(defun expand-channel (channels)
  "Selects a random channel and calls the node expansion function.
If a channel has not been initialized (expanded for the first time) priority is given to it"
  (loop for c in channels do
       (let ((dt (sensory-channel-discrimination-tree c)))
         (print "hello")
         (when(and (eq (tree-node-left-child dt) nil) (eq (tree-node-right-child dt) nil))
           (return-from expand-channel (random-expand dt)))))
  (random-expand (nth (random (length channels) (make-random-state t)) channels)))

;; Function in charge of discriminate a value inside a node (tree representation)
(defun discriminate-values(node topic-value object-value feature-name)
  "Discriminates the topic-value from the object-value using the node/tree provided as parameter"
  (let ((low-b (get-lower-bound node)) (up-b (get-upper-bound node)))
    (format t "lb: ~d ub: ~d tv: ~d ov: ~d ~C" low-b up-b topic-value object-value #\linefeed)
    (when (and (<= low-b topic-value) 
               (>= up-b topic-value) 
               (or (> low-b object-value) (< up-b object-value)))
      (return-from discriminate-values (make-discrimination-result :successful t :discrimination-node node :feature-name feature-name)))
    (setq res nil)
    (when (not (eq (tree-node-left-child node) nil))
      (setq res (discriminate-values (tree-node-left-child node) object-value topic-value)))
    (when (not (eq res nil))
      (return-from discriminate-values res))
    (when (not (eq (tree-node-right-child node) nil))
      (setq res (discriminate-values (tree-node-right-child node) object-value topic-value))))
  res)

;; loops over the feature detectors (channels) of the agent
;; to try to discriminate the two objects in each of them
(defun agent-discriminate (agent topic object)
  "Discrimination attempt on all the feature detectors of the given agent"
  (loop for fd in (agent-feature-detectors agent) do 
       (discriminate-values (sensory-channel-discrimination-tree fd) topic object (sensory-channel-feature-name fd))))

;; Function in charge of making a discrimination between the objects of a scene
;; using the feature detectors of the agent
(defun run-discrimination(agent scene)
  (let ((res '()))
    (loop for obj in (scene-objects scene) do
         (loop for gc in *game-channels* do
              (let ((disc (agent-discriminate agent (get-input-object-value (scene-topic scene) gc) (get-input-object-value obj gc))))
                (when (not (eq disc nil))
                  (setq res (append res (list disc)))))))
    res))

(defun reward-discrimination-trees(trees)
  (let ((tr (nth (random (length trees) (make-random-state t)) trees)))
    (loop for tree in trees do
         (when(> (node-data-score (tree-node-data tree)) (node-data-score (tree-node-data tr)))
           (setq tr tree)))
    (setf (node-data-score (tree-node-data tree)) (+ (node-data-score (tree-node-data tree)) 1))
    (setf (node-data-game-number (tree-node-data tree)) *current-game*)))

;; ****** Add saliency and context-scaling
;; Function to run a single discrimination game
(defun run-single-game(agent)
  (let* ((scene (create-scene)) (discrimination-results (run-discrimination agent scene)))
    ((if (eq discrimination-results nil)
         (expand-channel (agent-feature-detectors agent))
         (reward-discrimination-trees discrimination-results))))

;; Definition of the game. Main function to run.
(defun exec-game()
  (let ((agent (make-agent)))
    (loop for i from 0 to *total-number-of-games* do
         (run-single-game agent))))

(exec-game)
