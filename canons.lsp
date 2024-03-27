;AUTHORS: Nahuel Vazquez, Yelyzaveta Denysova

;############################################################
; Utils
;############################################################
;returns a random between 2 values
(defun bounded_random (incMin excMax)
    (+ incMin (random (- excMax incMin))))

;adds an offset to the y component of pos
(defun offset_height (pos offset)
    (mapcar '+ pos (cons 0 (list offset)))
)

(defun radians (degrees)
 (/ (* degrees (* 2 pi)) 360)
 )

;adds an offset to the y component of pos
(defun offset_width (pos offset)
    (mapcar '+ pos (cons offset (list 0)))
)

;adds an offset to the x and y component of pos
(defun offset_width_and_height (pos width height)
    (mapcar '+ pos (cons width (list height)))
)

;Limits a value between a minimum and a maximum
(defun clamp (maxVal minVal n)
    (max (min maxVal n) minVal)
)

;returns true if the projectile collided with the specified block
(defun inside_of_block (projectile block)
    (cond ((and (> (get projectile 'x) (car (get block 'pos))) (< (get projectile 'x) (+ (car (get block 'pos)) (get block 'width))))
            (cond ((and (> (get projectile 'y) (car(cdr (get block 'pos)))) (< (get projectile 'y) (+ (car (cdr (get block 'pos))) (get block 'height))))
                t
            ) (t nil))
    ) (t nil))
    
)

(defun tan(x)
(/ (sin x) (cos x))
)

(defun sleep (seconds)
    "Espera la quantitat indicada de segons"
    ; Això és un bucle iteratiu. NO PODEU FER-LO SERVIR ENLLOC MÉS
    (do ((endtime (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
        ((> (get-internal-real-time) endtime))))

;returns the block the projectile collided with, or nil
(defun block_collided (projectile)
    (cond 
        ((inside_of_block projectile 'left_field) 'left_field )
        ((inside_of_block projectile 'left_cannon) 'left_cannon)
        ((inside_of_block projectile 'wall) 'wall)
        ((inside_of_block projectile 'right_field) 'right_field)
        ((inside_of_block projectile 'right_cannon) 'right_cannon)
        ((not (inside_of_block projectile 'scene)) 'scene)
        (t nil)
    )
)

(defun decrease_health_cannon (cannon)
    (cond ((not (null (get cannon 'hp))) (putprop cannon (- (get cannon 'hp) 10) 'hp)))
)

(defun calculate_initial_velocity ()
    (/ (- (car(get 'left_cannon 'pos)) (car(get 'right_cannon 'pos)) (* 0.5 (get 'flag 'strength) (* (calculate_air_time) (calculate_air_time)))) 
    (* (cos (get 'right_cannon 'angle)) (calculate_air_time)))
)

(defun calculate_air_time ()  
    (max 0. 0000001 (realpart (sqrt (/
        (* 2 (+ (* (tan (get 'right_cannon 'angle)) (- (car (get 'left_cannon 'pos)) (car (get 'right_cannon 'pos)))) (car (cdr (get 'right_cannon 'pos))) (- 0 (car (cdr (get 'left_cannon 'pos))))))
        (+ (* (tan (get 'right_cannon 'angle)) (get 'flag 'strength)) 9.8)))))
)

;############################################################
; Drawing Elements
;############################################################
;Fills a block with color
(defun draw_fill (bloc row)
    ;select color
    (apply 'color (get bloc 'fill))
    ;draw line by line till the height is reached
    (cond ((< row (get bloc 'height)) 
        
            (apply 'move (offset_height (get bloc 'pos) row))
            (apply 'draw (offset_width_and_height (get bloc 'pos) (get bloc 'width) row))
            (draw_fill bloc (+ 1 row))
        ) 
        (t t)
    )
)

;Draws the border of a block
(defun draw_border (bloc)
    ;select color
    (apply 'color (get bloc 'border))
    ;draw the 4 lines
    (apply 'move (get bloc 'pos))

    (apply 'draw (offset_width (get bloc 'pos) (get bloc 'width)))
    (apply 'draw (offset_width_and_height (get bloc 'pos) (get bloc 'width) (get bloc 'height)))
    (apply 'draw (offset_height (get bloc 'pos) (get bloc 'height)))
    (apply 'draw (get bloc 'pos))
)

(defun draw_block (bloc)
    ;Draw the fill
    (draw_fill bloc 0)
    (draw_border bloc)
)

(defun draw_cannon (cannon)
    ;Draw the body of the cannon
    (draw_block cannon)
    ;Draw the line
    (apply 'color (get cannon 'border))
    (apply 'move (offset_width_and_height (get cannon 'pos) (floor (get cannon 'width) 2) (get cannon 'height)))
    (drawrel (round (* (get cannon 'strength) (cos (get cannon 'angle)))) (round (* (get cannon 'strength) (sin (get cannon 'angle)))))
)

;Draws each segment indicating the wind strength
(defun draw_flag_segment (flag n) 
    (cond 
        ((< n (abs (get flag 'strength))) 
            (color 255 (* (mod n 2) 255) (* (mod n 2) 255)) ;Switch between red and white lines
            (apply 'move (offset_width_and_height (get flag 'pos) (* (abs (- 1 (* 2 (+ n 1)))) (/ (abs (get flag 'strength)) (get flag 'strength))) (get flag 'height)))
            (drawrel 0 -8)
            (color 0 0 0) ;Draw the separating black line
            (apply 'move (offset_width_and_height (get flag 'pos) (* (* 2 (+ n 1)) (/ (abs (get flag 'strength)) (get flag 'strength))) (get flag 'height)))
            (drawrel 0 -8)
            (draw_flag_segment flag (+ n 1))
        )
        (t 
            ;Draw the top and bottom lines
            (color 0 0 0)
            (apply 'move (offset_height (get flag 'pos) (get flag 'height)))
            (drawrel (* 2 (get flag 'strength)) 0)
            (apply 'move (offset_height (get flag 'pos) (- (get flag 'height) 8)))
            (drawrel (* 2 (get flag 'strength)) 0)
        )
    )
)

;Draws the flag
(defun draw_flag (flag)
    ;Draw the pole
    (apply 'color (get flag 'border))
    (apply 'move (get flag 'pos))
    (drawrel 0 (get flag 'height))
    ;Draw the segments
    (draw_flag_segment flag 0)
)

;Draws in red the remaining life of the tank
(defun draw_hp (cannon hp_box row)
    ;select color
    (apply 'color (get hp_box 'fill))
    ;draw line by line till the height is reached
    (cond ((< row (get cannon 'hp)) 
            (apply 'move (offset_height (get hp_box 'pos) row))
            (apply 'draw (offset_width_and_height (get hp_box 'pos) (get hp_box 'width) row))
            (draw_hp cannon hp_box (+ 1 row))
        ) 
        (t t)
    )
)

;draws the box and the remaining life of the tank
(defun draw_hp_box (cannon hp_box)
    (draw_hp cannon hp_box 0)
    (draw_border hp_box)
)

;Clears the screeen and draws all elements updated
(defun repaint ()
    (cls)
    (draw_block 'left_field)
    (draw_block 'right_field)
    (draw_block 'wall)
    (draw_cannon 'right_cannon)
    (draw_cannon 'left_cannon)
    (draw_flag 'flag)
    (draw_hp_box 'left_cannon 'left_hp_box)
    (draw_hp_box 'right_cannon 'right_hp_box)
)

;Draws a point where the projectile
(defun draw_point (projectile)
    (color 0 0 0)
    (move (round (get projectile 'x)) (round (get projectile 'y)))
    (drawrel 1 1)
)

;############################################################
; Gameplay functions
;############################################################
;Functions for changing the angles of the cannons
(defun increase_angle_left_cannon ()
    (putprop 'left_cannon  (clamp pi 0 (+ (get 'left_cannon 'angle) 0.1)) 'angle)
)

(defun decrease_angle_left_cannon ()
    (putprop 'left_cannon  (clamp pi 0 (- (get 'left_cannon 'angle) 0.1)) 'angle)
)

(defun increase_angle_right_cannon ()
    (putprop 'right_cannon (clamp pi 0 (- (get 'right_cannon 'angle) 0.1)) 'angle)
)

(defun decrease_angle_right_cannon ()
    (putprop 'right_cannon (clamp pi 0 (+ (get 'right_cannon 'angle) 0.1)) 'angle)
)
;Functions for moving the cannons
(defun move_left_left_cannon ()
    ;change only x component and limit to the sides of the field
    (putprop 'left_cannon  (cons (clamp (- (get 'left_field 'width) (get 'left_cannon 'width)) 0 (- (car (get 'left_cannon 'pos)) 2)) (cdr (get 'left_cannon 'pos))) 'pos)
)

(defun move_right_left_cannon ()
    ;change only x component and limit to the sides of the field
    (putprop 'left_cannon  (cons (clamp (- (get 'left_field 'width) (get 'left_cannon 'width)) 0 (+ (car (get 'left_cannon 'pos)) 2)) (cdr (get 'left_cannon 'pos))) 'pos)
)

(defun move_left_right_cannon ()
    ;change only x component and limit to the sides of the field
    (putprop 'right_cannon  (cons (clamp (- (get 'scene 'width) (get 'right_cannon 'width)) (+ (get 'left_field 'width) (get 'wall 'width)) (- (car (get 'right_cannon 'pos)) 2)) (cdr (get 'right_cannon 'pos))) 'pos)
)

(defun move_right_right_cannon ()
    ;change only x component and limit to the sides of the field
    (putprop 'right_cannon  (cons (clamp (- (get 'scene 'width) (get 'right_cannon 'width)) (+ (get 'left_field 'width) (get 'wall 'width)) (+ (car (get 'right_cannon 'pos)) 2)) (cdr (get 'right_cannon 'pos))) 'pos)
)
;Functions for changing the power of the cannon
(defun increase_power (cannon)
    (putprop cannon (clamp 1000 1 (+ (get cannon 'strength) 2)) 'strength)
)

(defun decrease_power (cannon)
    (putprop cannon (clamp 1000 1 (- (get cannon 'strength) 2)) 'strength)
)
;Function for shooting
(defun shoot (projectile)
    (cond ((null (block_collided projectile))
        ;Update X position
        (putprop projectile (+ (get projectile 'x) (* (get projectile 'vx) (/ (- (get-internal-real-time) (get 'time 'last_time)) internal-time-units-per-second))) 'x)
        ;Update Y Position
        (putprop projectile (+ (get projectile 'y) (* (get projectile 'vy) (/ (- (get-internal-real-time) (get 'time 'last_time)) internal-time-units-per-second))) 'y)

        ;Update X velocity
        (putprop projectile (+ (get projectile 'vx) (* (get 'flag 'strength) (/ (- (get-internal-real-time) (get 'time 'last_time)) internal-time-units-per-second))) 'vx)
        ;Update Y velocity
        (putprop projectile (- (get projectile 'vy) (* 9.8 (/ (- (get-internal-real-time) (get 'time 'last_time)) internal-time-units-per-second))) 'vy)

        (draw_point projectile)

        (putprop 'time (get-internal-real-time) 'last_time)
        (sleep 0.01)
        (shoot projectile)
    ) (t (decrease_health_cannon (block_collided projectile))))
)

;############################################################
; Game functions
;############################################################
(defun inicia ()
    (putprop 'scene 640 'width)
    (putprop 'scene 340 'height)
    (putprop 'scene (cons 0 (list 0)) 'pos)

    ;==============================================================
    ;Obtain the sizes of each block
    ;==============================================================
    ;Blocks
    (putprop 'wall (bounded_random 20 41) 'width)
    (putprop 'wall (bounded_random 100 151) 'height)
        ;grab the floor because we can't have decimals
    (putprop 'right_field (floor (- (get 'scene 'width) (get 'wall 'width) (random 41)) 2) 'width)
    (putprop 'right_field (bounded_random 15 45) 'height)
        ;add 1 because the floor in the right_field
    (putprop 'left_field (+ 1 (- (get 'scene 'width) (get 'wall 'width) (get 'right_field 'width))) 'width)
    (putprop 'left_field (bounded_random 15 45) 'height)
    ;Cannons
    (putprop 'left_cannon 20 'width)
    (putprop 'left_cannon 10 'height)

    (putprop 'right_cannon 20 'width)
    (putprop 'right_cannon 10 'height)
    ;Flag
    (putprop 'flag 20 'height)
    ;HP Boxes
    (putprop 'right_hp_box 100 'height)
    (putprop 'right_hp_box 30 'width)

    (putprop 'left_hp_box 100 'height)
    (putprop 'left_hp_box 30 'width)

    ;==============================================================
    ;Obtain the absolut positions (Bottom left) of each block
    ;==============================================================
    ;Blocks
    (putprop 'left_field (cons 0 (list 0)) 'pos)

    (putprop 'wall (cons (get 'left_field 'width) (list 0)) 'pos)

    (putprop 'right_field (cons (+ (get 'left_field 'width) (get 'wall 'width)) (list 0)) 'pos)
    ;Cannons
    (putprop 'left_cannon (cons (bounded_random (floor (get 'left_field 'width) 3) (- (* 2 (floor (get 'left_field 'width) 3)) (get 'left_cannon 'width))) (list (get 'left_field 'height))) 'pos)

    (putprop 'right_cannon (cons 
                                (+ (get 'left_field 'width) 
                                (get 'wall 'width) 
                                (bounded_random (floor (get 'right_field 'width) 3) (- (* 2 (floor (get 'right_field 'width) 3)) (get 'right_cannon 'width)))) (list (get 'right_field 'height))) 'pos)
    ;Flag
    (putprop 'flag (cons (+ (get 'left_field 'width) (floor (get 'wall 'width) 2)) (list (get 'wall 'height))) 'pos)
    ;HP Boxes
    (putprop 'left_hp_box (cons 10 (list (floor (get 'scene 'height) 2))) 'pos)

    (putprop 'right_hp_box (cons (- (get 'scene 'width) 10 (get 'left_hp_box 'width)) (list (floor (get 'scene 'height) 2))) 'pos)

    ;==============================================================
    ;Set the fill color and the border
    ;==============================================================
    ;Blocks
    (putprop 'left_field '(0 107 30) 'border)
    (putprop 'left_field '(0 212 60) 'fill)

    (putprop 'right_field '(0 107 30) 'border)
    (putprop 'right_field '(0 212 60) 'fill)

    (putprop 'wall '(74 74 74) 'border)
    (putprop 'wall '(130 130 130) 'fill)
    ;Cannons
    (putprop 'left_cannon '(74 74 74) 'border)
    (putprop 'left_cannon '(130 130 130) 'fill)

    (putprop 'right_cannon '(74 74 74) 'border)
    (putprop 'right_cannon '(130 130 130) 'fill)
    ;Flag
    (putprop 'flag '(0 0 0) 'border) ;The flag will have other colors but specified in the function that draws it
    ;HP Boxes
    (putprop 'right_hp_box '(0 0 0) 'border)
    (putprop 'right_hp_box '(255 0 0) 'fill)

    (putprop 'left_hp_box '(0 0 0) 'border)
    (putprop 'left_hp_box '(255 0 0) 'fill)

    ;==============================================================
    ;Extra settings for cannons
    ;==============================================================
    (putprop 'left_cannon 20 'strength)
    (putprop 'left_cannon (radians 45) 'angle)
    (putprop 'left_cannon 100 'hp)

    (putprop 'right_cannon 20 'strength)
    (putprop 'right_cannon (radians (- 180 45)) 'angle)
    (putprop 'right_cannon 100 'hp)

    ;==============================================================
    ;Extra settings for the flag
    ;==============================================================
    (putprop 'flag (bounded_random -5 6) 'strength)

    ;==============================================================
    ;Start the game
    ;==============================================================
    (color 0 0 0 66 135 245)
    (pprint "PRESS SPACE TO PLAY PVE, ANYTHING ELSE TO PLAY PVP")
    (cond ((eq (get-key) 32)
            (repaint)
            (pprint "PLAYER'S TURN")
            (game_loop_ai (get-key) 0)
        ) (t 
            (repaint)
            (game_loop_players (get-key)))
    )
)

(defun game_loop_ai (key n)
    ;Verify if somone died
    (cond ((and (> (get 'left_cannon 'hp) 0) (> (get 'right_cannon 'hp)))

    (cond 
        ((eq (mod n 2) 0) ;If n is even, it is player's turn: Evaluate action
            (cond 
                ( (eq 97 key) ;A Move cannon to the left
                    (move_left_left_cannon)
                    (repaint)
                    (game_loop_ai (get-key) n)
                )
                ( (eq 100 key) ;D Move cannon to the right
                    (move_right_left_cannon)
                    (repaint)
                    (game_loop_ai (get-key) n)
                )
                ( (eq 119 key) ;W Rise cannon
                    (increase_angle_left_cannon)
                    (repaint)
                    (game_loop_ai (get-key) n)
                )
                ( (eq 115 key) ;S Lower cannon
                    (decrease_angle_left_cannon)
                    (repaint)
                    (game_loop_ai (get-key) n)
                )
                ( (eq 113 key) ;Q Decrease power
                    (decrease_power 'left_cannon)
                    (repaint)
                    (game_loop_ai (get-key) n)
                )
                ( (eq 101 key) ;E Increase power
                    (increase_power 'left_cannon)
                    (repaint)
                    (game_loop_ai (get-key) n)
                )
                ( (eq 102 key) ;F Shoot
                    ;obtain delta time
                    (putprop 'time (get-internal-real-time) 'last_time)
                    ;Load the projectile
                    (putprop 'left_projectile (+ (car (get 'left_cannon 'pos)) (floor (get 'left_cannon 'width) 2)) 'x)
                    (putprop 'left_projectile (+ (car (cdr (get 'left_cannon 'pos))) (get 'left_cannon 'height) 1) 'y)
                    ;Set initial velocity
                    (putprop 'left_projectile (* (get 'left_cannon 'strength) (cos (get 'left_cannon 'angle))) 'vx)
                    (putprop 'left_projectile (* (get 'left_cannon 'strength) (sin (get 'left_cannon 'angle))) 'vy)

                    (shoot 'left_projectile)
                    (repaint)
                    (game_loop_ai nil (+ 1 n))
                )
                ( (eq 27 key) ;ESC
                    ;Do nothing: END
                    (cls)
                )
                ( t
                    (game_loop_ai (get-key) n) ;Nothing changed
                )
            )
        )
        (t  ;Turn of AI
                ;Set random position of the AI
                (putprop 'right_cannon (cons (bounded_random (+ (get 'left_field 'width) (get 'wall 'width)) (- (get 'scene 'width) (get 'right_cannon 'width))) (cdr (get 'right_cannon 'pos))) 'pos)
                ;Set random angle of AI
                (putprop 'right_cannon (radians(- 180 (bounded_random 45 75))) 'angle)

                ;Set initial velocity
                (putprop 'right_cannon (calculate_initial_velocity) 'strength)
                (putprop 'right_projectile (* (calculate_initial_velocity) (cos (get 'right_cannon 'angle))) 'vx)
                (putprop 'right_projectile (* (calculate_initial_velocity) (sin (get 'right_cannon 'angle))) 'vy)
                
                ;Load the projectile
                (putprop 'right_projectile (+ (car (get 'right_cannon 'pos)) (floor (get 'right_cannon 'width) 2)) 'x)
                (putprop 'right_projectile (+ (car (cdr (get 'right_cannon 'pos))) (get 'right_cannon 'height) 1) 'y)

                (repaint)
                (pprint "AI'S TURN")

                ;obtain delta time
                (putprop 'time (get-internal-real-time) 'last_time)

                (shoot 'right_projectile)

                ;Change wind direction
                (putprop 'flag (bounded_random -5 6) 'strength)

                (repaint)
                (pprint "PLAYER'S TURN")
                (game_loop_ai (get-key) (+ 1 n))
            
        )
    ) );Victory Messages
    ((eq 0 (get 'left_cannon 'hp)) (pprint "AI WINS"))
    ((eq 0 (get 'right_cannon 'hp)) (pprint "PLAYER WINS"))
    )
)

(defun game_loop_players (key)
    (cond ((and (> (get 'left_cannon 'hp) 0) (> (get 'right_cannon 'hp)))
    (cond 
        ;LEFT PLAYER ------------------------------------------------------------------------
        ( (eq 97 key) ;A Move cannon to the left
            (move_left_left_cannon)
            (repaint)
            (game_loop_players (get-key))
        )
        ( (eq 100 key) ;D Move cannon to the right
            (move_right_left_cannon)
            (repaint)
            (game_loop_players (get-key))
        )
        ( (eq 119 key) ;W Rise left cannon
            (increase_angle_left_cannon)
            (repaint)
            (game_loop_players (get-key))
        )
        ( (eq 115 key) ;S Lower left cannon
            (decrease_angle_left_cannon)
            (repaint)
            (game_loop_players (get-key))
        )
        ( (eq 113 key) ;Q Decrease left power
            (decrease_power 'left_cannon)
            (repaint)
            (game_loop_players (get-key))
        )
        ( (eq 101 key) ;E Increase left power
            (increase_power 'left_cannon)
            (repaint)
            (game_loop_players (get-key))
        )
        ( (eq 102 key) ;F Shoot
            ;obtain delta time
            (putprop 'time (get-internal-real-time) 'last_time)
            ;Load the projectile
            (putprop 'left_projectile (+ (car (get 'left_cannon 'pos)) (floor (get 'left_cannon 'width) 2)) 'x)
            (putprop 'left_projectile (+ (car (cdr (get 'left_cannon 'pos))) (get 'left_cannon 'height) 1) 'y)
            ;Set initial velocity
            (putprop 'left_projectile (* (get 'left_cannon 'strength) (cos (get 'left_cannon 'angle))) 'vx)
            (putprop 'left_projectile (* (get 'left_cannon 'strength) (sin (get 'left_cannon 'angle))) 'vy)

            (shoot 'left_projectile)
            (putprop 'flag (bounded_random -5 6) 'strength)
            (repaint)
            (game_loop_players (get-key) )
        )
        ;RIGHT PLAYER ------------------------------------------------------------------------
        ( (eq 106 key) ;J Move cannon to the left
            (move_left_right_cannon)
            (repaint)
            (game_loop_players (get-key) )
        )
        ( (eq 108 key) ;L Move cannon to the right
            (move_right_right_cannon)
            (repaint)
            (game_loop_players (get-key) )
        )
        ( (eq 105 key) ;I Rise right cannon
            (increase_angle_right_cannon)
            (repaint)
            (game_loop_players (get-key) )
        )
        ( (eq 107 key) ;K Lower right cannon
            (decrease_angle_right_cannon)
            (repaint)
            (game_loop_players (get-key) )
        )
        ( (eq 111 key) ;O Decrease right power
            (decrease_power 'right_cannon)
            (repaint)
            (game_loop_players (get-key) )
        )
        ( (eq 117 key) ;U Increase right power
            (increase_power 'right_cannon)
            (repaint)
            (game_loop_players (get-key) )
        )
        ( (eq 104 key) ;H Shoot
            ;obtain delta time
            (putprop 'time (get-internal-real-time) 'last_time)
            ;Load the projectile
            (putprop 'right_projectile (+ (car (get 'right_cannon 'pos)) (floor (get 'right_cannon 'width) 2)) 'x)
                (putprop 'right_projectile (+ (car (cdr (get 'right_cannon 'pos))) (get 'right_cannon 'height) 1) 'y)
            ;Set initial velocity
            (putprop 'right_projectile (* (get 'right_cannon 'strength) (cos (get 'right_cannon 'angle))) 'vx)
            (putprop 'right_projectile (* (get 'right_cannon 'strength) (sin (get 'right_cannon 'angle))) 'vy)

            (shoot 'right_projectile)
            (putprop 'flag (bounded_random -5 6) 'strength)
            (repaint)
            (game_loop_players (get-key) )
        )
        ( (eq 27 key) ;ESC
            ;Do nothing: END
            (cls)
        )
        ( t
            (game_loop_players (get-key) ) ;Nothing changed
        )
    )) ;Victory Messages
    ((eq 0 (get 'left_cannon 'hp)) (pprint "LEFT PLAYER WINS"))
    ((eq 0 (get 'right_cannon 'hp)) (pprint "RIGHT PLAYER WINS"))
    )
)

(inicia)