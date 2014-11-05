(defun boarders (x y nlinhas ncolunas)
(let* (
(i 0)       
(xmin (max (- x 1) 0))
(xmax (min (+ x 1) (- nlinhas 1)))
(ymin (max (- y 1) 0))
(ymax (min (+ y 1) (- ncolunas 1)))
(boarderList (make-list (* (- (+ xmax 1) (+ xmin 0)) (- (+ ymax 1) (+ ymin 0))))))

(loop for a from ymin to ymax
      do(
      loop for b from xmin to xmax      
      
      do(
         setf (nth i boarderList) (format nil "~D ~D" b a))
      do(setf i (+ i 1))    
      )
      )

boarderList
))
