;;;; gllife.lisp

(in-package #:gllife)

(defun init-board (grid &optional (fade-grid nil) &key (probability 0.5))
  "Randomly set cells to t and nil."
  (loop for i from 0 below (array-dimension grid 0)
     do
       (setf cy 0)
       (loop for j from 0 below (array-dimension grid 1)
          do
            (if (> (/ (random 100.0) 100.0) probability )
                (progn
                  (setf (aref grid i j) t)
                  (if fade-grid
                      (setf (aref fade-grid i j) 1)))
                (progn
                  (setf (aref grid i j) nil)
                  (if fade-grid
                      (setf (aref fade-grid i j) 0)))))))

(defun toggle-location (boards win-width win-height x y)
  (let* ((grid (car boards))
         (fade-grid (caddr boards))
         (xdim (array-dimension grid 0))
         (ydim (array-dimension grid 1))
         (xp (truncate (* xdim (/ x win-width))))
         (yp (truncate (* ydim (/ (- win-height y) win-height))))
         (cv (aref grid xp yp)))
    (if cv
        (progn
          (setf (aref grid xp yp) nil)
          (if fade-grid
              (setf (aref fade-grid xp yp) 0)))
        (progn
          (setf (aref grid xp yp) t)
          (if fade-grid
              (setf (aref fade-grid xp yp) 1))))))

(defun count-neighbors (grid i j)
  "Count the neighbors of grid location i,j"
  (let ((ip (if (> 0 (- i 1)) (- (array-dimension grid 0) 1) (- i 1)))
        (jp (if (> 0 (- j 1)) (- (array-dimension grid 1) 1) (- j 1)))
        (in (if (>= i (- (array-dimension grid 0) 1)) 0 (+ i 1)))
        (jn (if (>= j (- (array-dimension grid 1) 1)) 0 (+ j 1)))
        (count 0))
    (if (aref grid ip jp) (incf count))
    (if (aref grid ip j) (incf count))
    (if (aref grid ip jn) (incf count))
    (if (aref grid i jp) (incf count))
    (if (aref grid i jn) (incf count))
    (if (aref grid in jp) (incf count))
    (if (aref grid in j) (incf count))
    (if (aref grid in jn) (incf count))
    count))

(defun update-board (old-grid new-grid &optional (fade-grid nil))
  "Update old-grid based on neighbor counts, placing the results in new-grid."
  (loop for i from 0 below (array-dimension old-grid 0)
     do
       (setf cy 0)
       (loop for j from 0 below (array-dimension old-grid 1)
          do
            (setf (aref new-grid i j) nil)
            (let ((nc (count-neighbors old-grid i j)))
              (if (and (aref old-grid i j) (< nc 2))
                  (setf (aref new-grid i j) nil)
                  (if (and (aref old-grid i j) (or (= nc 2) (= nc 3)))
                      (progn
                        (setf (aref new-grid i j) t)
                        (if fade-grid
                            (let ((cv (aref fade-grid i j)))
                              (incf cv)
                              (if (< cv 255)
                                  (setf (aref fade-grid i j) cv)))))
                      (if (and (aref old-grid i j) (> nc 3))
                          (setf (aref new-grid i j) nil)
                          (if (and (not (aref old-grid i j)) (= 3 nc))
                              (progn
                                (setf (aref new-grid i j) t)
                                (if fade-grid
                                    (let ((cv (aref fade-grid i j)))
                                      (incf cv)
                                      (if (< cv 255)
                                          (setf (aref fade-grid i j) cv)))))))))))))

(defun draw-board (grid win-width win-height &optional (fade-grid nil))
  "Used OpenGL to display the grid."
  (gl:matrix-mode :modelview)
  (gl:push-matrix)
  (let ((dx (/ win-width (array-dimension grid 0)))
        (dy (/ win-height (array-dimension grid 1)))
        (cx 0)
        (cy 0))
    (gl:begin :quads)
    (loop for i from 0 below (array-dimension grid 0)
       do
         (setf cy 0)
         (loop for j from 0 below (array-dimension grid 1)
            do
              (if fade-grid
                  (let ((perc (/ (aref fade-grid i j) 256.0)))
                    (gl:color 0 perc 0)))
                    ;; (if (> (aref fade-grid i j) 0)
                    ;;     (decf (aref fade-grid i j)))))
              (if (aref grid i j)
                  (gl:color 0 1 0))
              (progn 
                (gl:vertex cx cy)
                (gl:vertex (+ dx cx) cy)
                (gl:vertex (+ dx cx) (+ dy cy))
                (gl:vertex cx (+ dy cy)))
              (incf cy dy))
         (incf cx dx))
    (gl:end)
    (gl:pop-matrix)))

(defun start-life (&key
                     (board-width 100) (board-height 100)
                     (win-width 800) (win-height 800)
                     (fps 30)
                     (delay 20)
                     (prob 0.5))
  "Run the game of life in an SDL window."


  (let
      ((boards (list
                (make-array `(,board-width ,board-height))
                (make-array `(,board-width ,board-height))
                (make-array `(,board-width ,board-height) :element-type '(mod 256) :initial-element 0)))
       ;; boards is a cons cell pointing to two 2-d arrays of booleans
       (prev-tick 0) ;; prev-tick is the previous value of sdl-system-ticks when the board was updated
       (paused nil)) ;; paused is t when paused, nil when not
    (init-board (car boards) (caddr boards) :probability prob )
    (sdl:with-init
        ()
      ;; Setup the window and view
      (sdl:window win-width win-height
                  :opengl t
                  :opengl-attributes '((:sdl-gl-depth-size   16)
                                       (:sdl-gl-doublebuffer 1)))
      (setf (sdl:frame-rate) fps)
      
      (format t "version: ~a~%" (gl:get-string :version))
      (gl:viewport 0 0 win-width win-height)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho 0.0 win-width 0.0 win-height -1.0 1.0)

      (gl:matrix-mode :modelview)
      (gl:load-identity)
      
      (gl:clear-color 0 0 0 0)
      (gl:shade-model :flat)
      (gl:cull-face :back)
      (gl:polygon-mode :front :fill)
      (gl:draw-buffer :back)
      (gl:enable :cull-face :depth-test)

      (gl:clear :color-buffer :depth-buffer)
      
      ;; Draw the initial board
      (draw-board (car boards) win-width win-height (caddr boards))
      (sdl:update-display)

      ;; Handle events
      (sdl:with-events ()
        (:quit-event () t)
        (:mouse-button-up-event
         (:BUTTON BUTTON :STATE STATE :X X :Y Y)
         (toggle-location boards win-width win-height x y))
        (:key-down-event
         ()
         ;; quit
         (when (or (sdl:key-down-p :sdl-key-q) (sdl:key-down-p :sdl-key-escape))
           (sdl:push-quit-event))

         ;; Reset to a random state
         (when (sdl:key-down-p :sdl-key-r)
           (init-board (car boards) (caddr boards) :probability prob ))

         (when (sdl:key-down-p :sdl-key-c)
           (init-board (car boards) (caddr boards) :probability 0 ))

         ;; Pause/unpause
         (when (sdl:key-down-p :sdl-key-p)
           (if paused
               (setf paused nil)
               (setf paused t))))

        (:video-expose-event () (sdl:update-display))

        (:idle
         ;; Check if it's time to update
         (if (> (- (sdl:system-ticks) prev-tick) delay)
             (progn
               (setf prev-tick (sdl:system-ticks))

               ;; Only update the board while not paused
               (when (not paused)
                 (update-board (car boards) (cadr boards) (caddr boards))
                 (setf boards (list (cadr boards) (car boards) (caddr boards))))

               ;; Clear the screen and redraw
               (gl:clear :color-buffer :depth-buffer)
               (draw-board (car boards) win-width win-height (caddr boards))
               (sdl:update-display))))))))
