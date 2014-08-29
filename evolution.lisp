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

(defstruct animal x y energy rep-energy dir genes)

(defclass animal ()
  ((x
    :init-arg x
    :init-form (random *width*)
    :accessor animal-x)
   (y
    :init-arg y
    :init-form (random *height*)
    :accessor animal-y)
   (energy
    :init-arg energy
    :init-form 1000
    :accessor animal-energy)
   (rep-energy
    :init-arg rep-energy
    :reader rep-energy)
   (dir
    :init-form 0
    :accessor animal-dir)
   (speed
    :init-form 1
    :reader animal-speed)
   (genes
    :init-arg genes
    :init-form (loop repeat 8
                    collecting (1+ random 10)))))

(defstruct (carnivore (:include animal)) (combat 1))

(defclass carnivore (animal)
  (rep-energy
   :init-form 400))

;; (defstruct (omnivore (:include animal) :meat-bonus 0.5))

;; (defparameter *animals*
;;   (list (make-animal :x      (ash *width*  -1)
;;                      :y      (ash *height* -1)
;;                      :energy 1000
;;                      :rep-energy 200
;;                      :dir    0
;;                      :genes  (loop repeat 8
;;                                    collecting (1+ (random 10))))
;;         (make-carnivore :x      (ash *width*  -2)
;;                         :y      (ash *height* -2)
;;                         :energy 1000
;;                         :rep-energy 400
;;                         :dir    0
;;                         :genes (loop repeat 8
;;                                     collecting (1+ (random 10))))))
(defparameter *animals* nil)

(defun init-evolution ()
  (setf *width* 100)
  (setf *height* 30)
  (setf *jungle* '(45 10 10 10))
  (setf *plant-energy* 80)
  (setf *plants* (make-hash-table :test #'equal))
  (setf *animals*
        (list (make-animal    :x      (ash *width*  -1)
                              :y      (ash *height* -1)
                              :energy 1000
                              :rep-energy 200
                              :dir    0
                              :genes  (loop repeat 8
                                      collecting (1+ (random 10))))
              (make-carnivore :x      (+ (ash *width*  -1) 10)
                              :y      (+ (ash *height* -1) 10)
                              :energy 1000
                              :rep-energy 400
                              :dir    0
                              :genes (loop repeat 8
                                        collecting (1+ (random 10))))))
  (setf *animal-pos* (make-hash-table :test #'equal)))


;; (defparameter *animals*
;;   (let ((tab (make-hash-table))
;;         (herb (make-animal :x          (ash *width*  -1)
;;                            :y          (ash *height* -1)
;;                            :energy     1000
;;                            :rep-energy 200
;;                            :dir        0
;;                            :genes      (loop repeat 8
;;                                             collecting (1+ (random 10)))))
;;         (carn (make-carnivore :x          (ash *width*  -2)
;;                               :y          (ash *height* -2)
;;                               :energy     1000
;;                               :rep-energy 400
;;                               :dir        0
;;                               :genes      (loop repeat 8
;;                                                collecting (1+ (random 10))))))
;;     (setf (gethash herb *animals*) 't)
;;     (setf (gethash carn *animals*) 't)))

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

(defmethod eat ((m animal))
  (let ((pos (cons (animal-x m) (animal-y m))))
    (when (gethash pos *plants*)
      (incf (animal-energy m) *plant-energy*)
      (remhash pos *plants*))))

(defmethod eat ((m carnivore))
  (let* ((pos (cons (animal-x m) (animal-y m)))
         (prey (find-prey m pos *animal-pos*)))
    (when prey
      (incf (animal-energy m) (ash (animal-energy prey) -1))
      (setf (animal-energy prey) 0)
      (setf (gethash pos *animal-pos*) (remove prey (gethash pos *animal-pos*))))))

;; (defun find-prey (carn pos list)
;;   (cond
;;     ((null list) '())
;;     (t (let* ((curr (car list))
;;               (apos (cons (animal-x curr) (animal-y curr))))
;;          (cond
;;            ((and (equal pos apos) (not (equal
;;                                         (type-of carn)
;;                                         (type-of curr))))
;;             curr)
;;            (t (find-prey carn pos (cdr list))))))))
(defun find-prey (carn pos tab)
  (let ((animals (gethash pos tab)))
    (find-if-not (lambda (animal) (equal (type-of animal) (type-of carn))) animals)))

;; (defparameter *reproduction-energy* 200)

(defmethod reproduce ((m animal))
  (let ((e (animal-energy m)))
    (when (>= e (animal-rep-energy m))
      (setf (animal-energy m) (ash e -1))
      (let ((animal-nu (copy-structure m))
            (genes     (copy-list (animal-genes m)))
            (mutation  (random 8)))
        (setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
        (setf (animal-genes animal-nu) genes)
        (push animal-nu *animals*)))))

(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
                               (<= (animal-energy animal) 0))
                             *animals*))
  (mapc (lambda (animal)
          (turn animal)
          (move animal)
          (eat animal)
          (reproduce animal))
        *animals*)
  (add-plants))

(defun draw-world ()
  (loop for y
        below *height*
        do (progn (fresh-line)
                  (princ "|")
                  (loop for x
                        below *width*
                        do (princ (cond ((some (lambda (animal)
                                                 (and (= (animal-x animal) x)
                                                      (= (animal-y animal) y)
                                                      (equal (type-of animal) 'animal)))
                                               *animals*)
                                         #\M)
                                        ((some (lambda (animal)
                                                 (and (= (animal-x animal) x)
                                                      (= (animal-y animal) y)
                                                      (equal (type-of animal) 'carnivore)))
                                               *animals*)
                                         #\C)
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
