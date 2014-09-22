(defpackage :evolution)

;; Evolution Game
(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)

(defparameter *plants* (make-hash-table :test #'equal))
(defparameter *animal-pos* (make-hash-table :test #'equal))

(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))


(defclass animal ()
  ((x
    :initarg :x
    :initform (random *width*)
    :accessor animal-x)
   (y
    :initarg :y
    :initform (random *height*)
    :accessor animal-y)
   (char
    :initarg :char
    :initform #\X
    :reader animal-char)
   (energy
    :initarg :energy
    :initform 1000
    :accessor animal-energy)
   (rep-energy
    :initarg :rep-energy
    :reader rep-energy)
   (dir
    :initform 0
    :accessor animal-dir)
   (speed
    :initform 1
    :reader animal-speed)
   (combat
    :initarg :combat
    :initform (random 10)
    :accessor combat)
   (genes
    :initarg :genes
    :initform (loop repeat 8
                 collecting (1+ (random 10)))
    :accessor animal-genes)))


(defclass carnivore (animal)
  ((rep-energy
    :initform 400)
   (char
    :initform #\C)
   (combat
    :initform (+ 5 (random 5)))))

(defclass herbivore (animal)
  ((rep-energy
    :initform 200)
   (char
    :initform #\H)))

(defclass omnivore (animal)
  ((meat-bonus
    :initarg :meat-bonus
    :initform 0.5)
   (rep-energy
    :initform 600)
   (char
    :initform #\O)))




(defparameter *animals* nil)


(defun init-evolution ()
  (set-starting-params)
  (setf *animals*
        (list (make-instance 'herbivore
                             :x (ash *width*  -1)
                             :y (ash *height* -1))
              (make-instance 'carnivore
                             :x (+ (ash *width*  -1) 10)
                             :y (+ (ash *height* -1) 10)))))


(defun set-starting-params ()
  (setf *width* 100)
  (setf *height* 30)
  (setf *jungle* '(45 10 10 10))
  (setf *plant-energy* 80)
  (setf *plants* (make-hash-table :test #'equal))
  (setf *animal-pos* (make-hash-table :test #'equal)))


(defun move (animal)
  (let ((dir (animal-dir animal))
        (x (animal-x animal))
        (y (animal-y animal)))
    (setf (gethash (cons x y) *animal-pos*)
          (remove animal (gethash (cons x y) *animal-pos*)))
    (setf (animal-x animal) (mod (+ x
                                    (cond ((and (>= dir 2) (< dir 5)) 1)
                                          ((or (= dir 1) (= dir 5)) 0)
                                          (t -1))
                                    *width*)
                                 *width*))
    (setf (animal-y animal) (mod (+ y
                                    (cond ((and (>= dir 0) (< dir 3)) -1)
                                          ((and (>= dir 4) (< dir 7)) 1)
                                          (t 0))
                                    *height*)
                                 *height*))
    (decf (animal-energy animal))
    (setf (gethash (cons (animal-x animal) (animal-y animal)) *animal-pos*)
          (cons animal (gethash (cons (animal-x animal) (animal-y animal)) *animal-pos*)))))

(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x)
               (let ((xnu (- x (car genes))))
                 (if (< xnu 0)
                     0
                     (1+ (angle (cdr genes) xnu))))))
      (setf (animal-dir animal)
            (mod (+ (animal-dir animal) (angle (animal-genes animal) x))
                 8)))))

;; Eating:
;; Carnivorous behavior is the common/default form of eating for animals.
;; Only herbivores make an acception in that they will only hunt for plants, ignoring other animals.
;; Eating will be determined based on combat ability, the same way that genes work. A loss in combat
;; will result on the death of the loser if the winner is not an herbivore.

(defmethod eat ((m animal))
  (let* ((pos (cons (animal-x m) (animal-y m)))
         (prey (find-prey m pos *animal-pos*)))
    (when prey
      (incf (animal-energy m) (ash (animal-energy prey) -1))
      (setf (animal-energy prey) 0)
      (setf (gethash pos *animal-pos*) (remove prey (gethash pos *animal-pos*))))))

;; TODO: Change just removing m from prey list to removing all animals "similar" to m.
(defmethod eat ((m animal))
  (let* ((pos (cons (animal-x m) (animal-y m)))
         (prey (remove m (gethash pos *animal-pos*))))
    (when prey
        (let ((target (nth (random (length prey)) prey)))
          (combat-roll m target)))))


;; TODO: Remove animals from hashtable in addition to killing them
(defun combat-roll (m1 m2)
  (let ((roll (random (+ 1 (combat m1) (combat m2)))))
    (cond
      ((= roll 0) (progn
                    (setf (animal-energy m1) 0)
                    (setf (animal-energy m2) 0)))
      ((< roll (combat m1)) (progn
                              (incf (animal-energy m1) (animal-energy m2))
                              (setf (animal-energy m2) 0))))))

(defmethod eat ((m herbivore))
  (let ((pos (cons (animal-x m) (animal-y m))))
    (when (gethash pos *plants*)
      (incf (animal-energy m) *plant-energy*)
      (remhash pos *plants*))))


(defun find-prey (carn pos tab)
  (let ((animals (gethash pos tab)))
    (find-if-not (lambda (animal) (equal (type-of animal) 'carnivore)) animals)))

;; (defparameter *reproduction-energy* 200)

;; (defmethod reproduce ((m animal))
;;   (let ((e (animal-energy m)))
;;     (when (>= e (rep-energy m))
;;       (setf (animal-energy m) (ash e -1))
;;       (let ((animal-nu (copy-structure m))
;;             (genes     (copy-list (animal-genes m)))
;;             (mutation  (random 8)))
;;         (setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
;;         (setf (animal-genes animal-nu) genes)
;;         (push animal-nu *animals*)))))
(defun mutate-genes (genes)
  (let ((mutation (random 8)))
    (setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3)  -1)))
    genes))


(defmethod reproduce ((m animal))
  (let ((e (animal-energy m)))
    (when (>= e (rep-energy m))
      (setf (animal-energy m) (ash e -1))
      (let ((animal-nu
             (make-instance (class-of m)
                            :x     (animal-x m)
                            :y     (animal-y m)
                            :energy (ash e -1)
                            :genes (mutate-genes (copy-list (animal-genes m))))))
        (push animal-nu *animals*)))))

;; (defun update-world ()
;;   (setf *animals* (remove-if (lambda (animal)
;;                                (<= (animal-energy animal) 0))
;;                              *animals*))
;;   (mapc (lambda (animal)
;;           (turn animal)
;;           (move animal)
;;           (eat animal)
;;           (reproduce animal))
;;         *animals*)
;;   (add-plants))

(defun update-world ()
  (labels ((kill-dead (animals)
             (if animals
                 (let* ((animal (car animals))
                        (x (animal-x animal))
                        (y (animal-y animal)))
                   (cond ((<= (animal-energy animal) 0)
                          (progn (setf (gethash (cons x y) *animal-pos*)
                                       (remove animal (gethash (cons x y) *animal-pos*)))
                                 (kill-dead (cdr animals))))
                         (t (cons animal (kill-dead (cdr animals))))
                         )))))
    (setf *animals* (kill-dead *animals*))
    (mapc (lambda (animal)
            (turn animal)
            (move animal)
            (eat animal)
            (reproduce animal))
          *animals*)
    (add-plants)))


(defun draw-world ()
  (loop for y
        below *height*
        do (progn (fresh-line)
                  (princ "|")
                  (loop for x
                        below *width*
                     do (princ (cond ((gethash (cons x y) *animal-pos*)
                                      (animal-char (car (gethash (cons x y) *animal-pos*))))
                                     ((gethash (cons x y) *plants*) #\*)
                                     (t #\space))))
                  (princ "|"))))

(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
          (t (let ((x (parse-integer str :junk-allowed t)))
               (if x
                   (loop for i
                         below x
                         do (update-world)
                         if (zerop (mod i 1000))
                           do (princ #\.))
                   (update-world))
               (evolution))))))

(defmethod pretty-print ((m animal))
  (format t "Animal:~&~10tChief Species: ~a~&~10tGenes: ~a~%~%" (type-of m) (animal-genes m)))
