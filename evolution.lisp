(defpackage :evolution
  (:use :common-lisp))

(in-package :evolution)

;; Evolution Game
(defparameter *width* 100)
(defparameter *height* 60)
(defparameter *jungles* '())
(defparameter *plant-energy* 80)
(defparameter *plants* (make-hash-table :test #'equal))
(defparameter *animals* nil)
(defparameter *animal-pos* (make-hash-table :test #'equal))

(defun set-starting-params ()
  (defparameter *width* 100)
  (defparameter *height* 60)
  (defparameter *jungles* '())
  (defparameter *plant-energy* 80)
  (defparameter *plants* (make-hash-table :test #'equal))
  (defparameter *animals* nil)
  (defparameter *animal-pos* (make-hash-table :test #'equal)))



(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  (progn
    (loop for jungle in *jungles*
         do (apply #'random-plant jungle))
    (random-plant 0 0 *width* *height*)))

(defun spawn-jungles ()
  (labels ((jungle (x y)
             (push (list x y 10 10) *jungles*)))
    (jungle (- (ash *width*  -1) 5)
            (- (ash *height* -1) 5))
    (loop repeat (/ (* *width* *height*) 3000)
         do (jungle (random (- *width*  10))
                    (random (- *height* 10))))))


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
    :accessor animal-genes)
   (age
    :initform 0
    :accessor age)
   (max-age
    :accessor max-age)
   (parent
    :initarg :parent
    :initform '()
    :accessor parent)))


(defclass carnivore (animal)
  ((rep-energy
    :initform 350)
   (char
    :initform #\C)
   (max-age
    :initform 1000)))

(defclass herbivore (animal)
  ((rep-energy
    :initform 200)
   (char
    :initform #\H)
   (max-age
    :initform 1000)))

(defclass omnivore (animal)
  ((food-multiplier
    :initarg :food-multiplier
    :initform 0.5
    :accessor multiplier)
   (rep-energy
    :initform 250)
   (char
    :initform #\O)
   (max-age
    :initform 300)))


(defun init-evolution ()
  (set-starting-params)
  (spawn-jungles)
  (setf *animals*
        (append (loop repeat (+ 5 (random 5))
                   collect (make-instance 'herbivore
                                          :x (1+ (random *width*))
                                          :y (1+ (random *height*))))
                (loop repeat (+ 1 (random 5))
                   collect (make-instance 'carnivore
                                    :x (+ (ash *width*  -1) 10)
                                    :y (+ (ash *height* -1) 10)
                                    :combat 10)
                      ;; (make-instance 'omnivore
                      ;;                :x (- (ash *width*  -1) 10)
                      ;;                :y (- (ash *height* -1) 10)
                      ;;                :combat 5)
                      )))
  (loop for animal in *animals*
     doing (setf (gethash (cons (animal-x animal)
                                (animal-y animal))
                            *animal-pos*)
                   (list animal)))
  *animals*)




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

;; (defmethod eat ((m animal))
;;   (let* ((pos (cons (animal-x m) (animal-y m)))
;;          (prey (car (find-prey m pos *animal-pos*))))
;;     (when prey
;;       (incf (animal-energy m) (ash (animal-energy prey) -1))
;;       (setf (animal-energy prey) 0)
;;       (setf (gethash pos *animal-pos*) (remove prey (gethash pos *animal-pos*))))))

;; TODO: Change just removing m from prey list to removing all animals "similar" to m.
(defgeneric eat (animal)
  (:documentation "Method for allowing animals to eat and gain energy."))

(defmethod eat ((m animal))
  (let* ((pos (cons (animal-x m) (animal-y m)))
         (prey (find-prey m pos *animal-pos*)))
    (when prey
      (let ((target (nth (random (length prey)) prey)))
        (combat-roll m target pos)))))

(defmethod eat ((m omnivore))
  (let ((pos (cons (animal-x m) (animal-y m))))
    (when (gethash pos *plants*)
      (incf (animal-energy m) (* (mod (* (multiplier m) 4) 1) *plant-energy*))
      (remhash pos *plants*))
    (let ((prey (find-prey m pos *animal-pos*)))
      (when prey
        (let ((target (nth (random (length prey)) prey)))
          (combat-roll m target pos 0 (/ (multiplier m) 4)))))))


;; TODO: Remove animals from hashtable in addition to killing them
(defun combat-roll (m1 m2 pos &optional (chance 1) (mult 1))
  (let* ((c1  (ash (combat m1) chance))
         (c2 (truncate (combat m2)))
         (roll (random (+ 1 c1 c2))))
    (cond
      ((= roll 0) (progn
                    (setf (animal-energy m1) 0)
                    (setf (animal-energy m2) 0)
                    (format nil "Tie")))
      ((< roll c1) (progn
                     (incf (animal-energy m1) (* mult (animal-energy m2)))
                     (setf (animal-energy m2) 0)
                     (setf (gethash pos *animal-pos*)
                           (remove m2 (gethash pos *animal-pos*)))
                     (format nil "m1 wins!")))
      (t (format nil "m2 won, no one died")))))

(defmethod eat ((m herbivore))
  (let ((pos (cons (animal-x m) (animal-y m))))
    (when (gethash pos *plants*)
      (incf (animal-energy m) *plant-energy*)
      (remhash pos *plants*))))


(defun find-prey (hunter pos tab)
  (let ((animals (gethash pos tab)))
    (remove-if-not (lambda (animal) (and (or (and (equal (type-of animal) 'herbivore)
                                             (equal (type-of hunter) 'carnivore))
                                        (< 10.0 (genetic-difference animal hunter))
                                        )
                                    (not (equal animal (parent hunter)))))
                   animals)))

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

(defun genetic-difference (s1 s2)
  (let ((g1 (animal-genes s1))
        (g2 (animal-genes s2)))
    (sqrt (loop for i in g1
             for j in g2
             summing (* (- j i) (- j i))))))


;; (defmethod reproduce ((m animal))
;;   (reproduce-helper m 1))

;; (defmethod reproduce ((m carnivore))
;;   (if (= 0 (random 1000))
;;       (reproduce-helper m 2 'omnivore)
;;       (reproduce-helper m 2)))

;; (defmethod reproduce ((m herbivore))
;;   (when (= 0 (random 1000))
;;     (reproduce-helper m 1 'omnivore)
;;     (reproduce-helper m 1)))

(defmethod reproduce ((m carnivore))
  (reproduce-helper m 2))

(defmethod reproduce ((m herbivore))
  (reproduce-helper m 1))

(defmethod reproduce ((m omnivore))
  (let ((e (round (animal-energy m)))
        (combat-change 1))
    (when (>= e (rep-energy m))
      (setf (animal-energy m) (ash e -1))
      (let* ((mult-nu (+ (- (random 0.2) 0.1)
                         (multiplier m)))
             (animal-nu
              (make-instance 'omnivore
                             :x     (animal-x m)
                             :y     (animal-y m)
                             :energy (ash e -1)
                             :genes (mutate-genes (copy-list (animal-genes m)))
                             :combat (max 0 (+ (round (- (random (1+ combat-change))
                                                         (/ combat-change 2)))
                                               (combat m)))
                             :food-multiplier  mult-nu
                             :parent m)))
        (let ((r (random 10)))
          (cond
            ((= r 0) (reproduce-helper m 1 'herbivore))
            ((= r 9) (reproduce-helper m 1 'carnivore))
            (t (push animal-nu *animals*))))))))

(defun reproduce-helper (m combat-change &optional (s (class-of m)))
  (let ((e (round (animal-energy m))))
    (when (>= e (rep-energy m))
      (setf (animal-energy m) (ash e -1))
      (let ((animal-nu
             (make-instance s
                            :x     (animal-x m)
                            :y     (animal-y m)
                            :energy (ash e -1)
                            :genes (mutate-genes (copy-list (animal-genes m)))
                            :combat (max 0 (+ (round (- (random (1+ combat-change))
                                                        (/ combat-change 2)))
                                              (combat m)))
                            :parent m)))
        (push animal-nu *animals*)))))


;; Most of this can be simplified immensely if we create a function to return herbivore/carnivore
;; instead of duplicating this entire let body & declaration
;; (defun mutate-species (s)
;;   (labels ((new-species (sym)
;;              (let* ((x (animal-x s))
;;                                (y (animal-y s))
;;                                (e (animal-energy s))
;;                                (genes (copy-list (animal-genes s)))
;;                                (combat (combat s))
;;                                (s-nu (make-instance sym
;;                                                     :x x
;;                                                     :y y
;;                                                     :energy e
;;                                                     :genes genes
;;                                                     :combat combat)))
;;                           (setf (gethash (cons x y) *animal-pos*)
;;                                 (cons s-nu (remove s (gethash (cons x y) *animal-pos*))))
;;                           (setf *animals* (cons s-nu (remove s *animals*))))))
;;     (cond ((eq (type-of s) 'omnivore)
;;            (let ((m (multiplier s)))
;;              (cond
;;                ((> 0.4 m) (new-species 'herbivore))
;;                ((< 0.6 m) (new-species 'carnivore)))))
;;           ((< (animal-energy s) 5)
;;            (new-species 'omnivore)))))

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
                   (cond ((or ;; (> (age animal) 1000)
                              (<= (animal-energy animal) 0))
                          (progn (setf (gethash (cons x y) *animal-pos*)
                                       (remove animal (gethash (cons x y) *animal-pos*)))
                                 (kill-dead (cdr animals))))
                         (t (cons animal (kill-dead (cdr animals))))
                         )))))
    (setf *animals* (kill-dead *animals*))
    (mapc (lambda (animal)
            (turn animal)
            (move animal)
            (incf (age animal))
            (eat animal)
            (reproduce animal)
            ;; (mutate-species animal)
            )
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

;; Convenvient functions for debugging/testing code

(defmethod pretty-print ((m animal))
  (format t "Animal:~&~10tChief Species: ~a~&~10tGenes: ~a~&~10tEnergy: ~a~%~%" (type-of m) (animal-genes m) (animal-energy m)))

(defun print-combats (animals)
  (loop for m in animals doing (format t "~a  ~a~&" (type-of m) (combat m))))

(defun species (s animals)
  (remove-if-not (lambda (x) (equal (type-of x) s)) animals))

(defun skip (n)
  (dotimes (_ n) (update-world)))

(defun start ()
  (progn
    (init-evolution)
    (evolution)))


;; Functions for collecting/processing statistical data on species
(defun find-differences (m animals)
           (remove 0.0 (loop for s in animals
                          collecting (genetic-difference m s))))

;; (defun find-all-differences (animals)
;;   (loop for n in animals collecting (cons n (find-differences n animals))))

(defun find-all-differences (animals)
  (let ((tab (make-hash-table :test #'equal)))
    (loop for n in animals
         doing (setf (gethash n tab) (find-differences n animals)))
    tab))

(defun median (nums)
  (nth (round (/ (length nums) 2)) (sort nums #'<)))

(defun average (nums)
  (loop for n in nums
     summing n into total
       counting n into len
       finally (return (/ total len))))

;; TODO create function to find most "average" species (one with all lowest genetic diffs)
