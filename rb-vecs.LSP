;;;; Simple vector library for VisualLISP/AutoLISP
;;;; Ryan Burnside 2022
;;;; For topical usage only use as directed.


;;; Constants
(setq tau (* pi 2.0))


;; Utility function
;; Reduces a list to a single return value
;; If an empty list is presented identity is returned
;; otherwise identity is appended to the list and fn applied
;; (reduce + 10 '()) => 10
;; (reduce + 10 '(1 2 3 4) => 20
;; (reduce + 0 '(1 2 3 4)) => 10

;; Like Common Lisp's reduce not like Scheme's (SRFI-1)
(defun reduce (fn identity lis / %buffer)
  (setq %buffer identity)
  (if (eq lis '())
    identity
    (progn
      (foreach i lis
        (setq %buffer (fn %buffer i)))
      %buffer)))
	

;;; Essentially an explicit version of (list x y z)
(defun mk-vec (x y z)
  (mapcar '(lambda (i)
             (if (numberp i)
               i
               0))
          (list x y z)))


(defun mk-vec2d-mag-angle (dist angl / x y)
  (setq x (* (cos angl) dist)
        y (* (sin angl) dist))
  (mk-vec x y 0.0))


;;; Element accessors for vectors
(defun vx (vec)
  (car vec))

(defun vy (vec)
  (cadr vec))

(defun vz (vec)
  (caddr vec))


;;; Routines, assume vectors are origin centered lacking position

(defun vec+ (vec vec2)
  (mapcar '+ vec vec2))

(defun vec+all (vec-list)
  (reduce vec+ (mk-vector 0 0 0) vec-list))

(defun vec-mag (vec)
  (distance (mk-vec 0.0 0.0 0.0) vec))

(defun vec-norm (vec / %dist)
  (setq %dist (vec-mag vec))
  (if (= %dist 0.0)
    vec
    (mapcar '(lambda (i)
               (/ i %dist))
            vec)))

(defun vec-scale (vec scaler)
  (mapcar '(lambda (i)
             (* i scaler))
          vec))

(defun vec-angle (vec)
  (angle (mk-vec 0 0 0) vec))
        
(defun vec-dot (vec vec2)
  (apply '+ (mapcar '* vec vec2)))

(defun vec-negate (vec)
  (mapcar '- vec)) 

(defun vec-cross (vec vec2)
  (mk-vec (- (* (vy vec) (vz vec2))
             (* (vz vec) (vy vec2)))
          (- (* (vz vec) (vx vec2))
             (* (vx vec) (vz vec2)))
          (- (* (vx vec) (vy vec2))
             (* (vy vec) (vx vec2)))))

(defun vec-rot (vec angl)
  (mk-vec2d-mag-angle (vec-mag vec)
                      (+ (vec-angle vec)
                         angl)))

(defun vec-scale-to (vec mag)
  (vec-scale (vec-norm vec) mag))


