(in-package :jumpguy)

(defun game-menu ()
  (setf (clear-color *engine-manager*) *black*)
  (let ((menu (create-menu (menu :active-input-device *all-input-id*)
                (game-name *engine-manager*)
                ("Play" (run-action
                         (change-scene *engine-manager* (launch-jumpguy))))
                ("Quit" (run-action (quit))))))
    menu))


(defclass rectangle (static-sprite)
  ((color :initarg :color :initform (make-color) :accessor color)
   (recurse.vert:path-to-image :initform (resource-path "rectangle.png")
                               :allocation :class
                               :reader path-to-image))
  (:documentation "A basic rectangle with obb collision detection."))

(defmethod initialize-instance :after ((rectangle rectangle) &rest args)
  (declare (ignore args))
  (setf (color rectangle) (color rectangle)))

(defmethod (setf color) :after (value (rectangle rectangle))
  (setf (recurse.vert::color-mod rectangle) value))


(defclass player (jumper animated-sprite input-handler direction-tracker)
  ((recurse.vert:path-to-image :initform (resource-path "others_artsets/jungle_asset_pack/Character/sprites/idle_sprite.png")
                               :reader path-to-image)
   (recurse.vert:animations ; TODO: class allocate? :allocation :class
    :initform (list :running (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/Character/sprites/run_sprite.png")
                                             :frames (vector (make-sprite-source #.(* 0 21) #.(* 0 33) 21 33)
                                                             (make-sprite-source #.(* 1 21) #.(* 0 33) 21 33)
                                                             (make-sprite-source #.(* 2 21) #.(* 0 33) 21 33)
                                                             (make-sprite-source #.(* 3 21) #.(* 0 33) 21 33)
                                                             (make-sprite-source #.(* 4 21) #.(* 0 33) 21 33)
                                                             (make-sprite-source #.(* 5 21) #.(* 0 33) 21 33)
                                                             (make-sprite-source #.(* 6 21) #.(* 0 33) 21 33)
                                                             (make-sprite-source #.(* 7 21) #.(* 0 33) 21 33))
                                             :time-between-frames-ms 50)
                    :jumping (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/Character/sprites/jump.png")
                                             :frames (vector (make-sprite-source #.(* 0 17) #.(* 0 34) 17 34))
                                             :time-between-frames-ms 50)
                    :falling (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/Character/sprites/landing.png")
                                             :frames (vector (make-sprite-source #.(* 0 20) #.(* 0 35) 20 35))
                                             :time-between-frames-ms 50)
                    :standing (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/Character/sprites/idle_sprite.png")
                                              :frames (vector (make-sprite-source #.(* 0 19) #.(* 0 34) 19 34)
                                                              (make-sprite-source #.(* 1 19) #.(* 0 34) 19 34)
                                                              (make-sprite-source #.(* 2 19) #.(* 0 34) 19 34)
                                                              (make-sprite-source #.(* 3 19) #.(* 0 34) 19 34)
                                                              (make-sprite-source #.(* 4 19) #.(* 0 34) 19 34)
                                                              (make-sprite-source #.(* 5 19) #.(* 0 34) 19 34)
                                                              (make-sprite-source #.(* 6 19) #.(* 0 34) 19 34)
                                                              (make-sprite-source #.(* 7 19) #.(* 0 34) 19 34)
                                                              (make-sprite-source #.(* 8 19) #.(* 0 34) 19 34)
                                                              (make-sprite-source #.(* 9 19) #.(* 0 34) 19 34)
                                                              (make-sprite-source #.(* 10 19) #.(* 0 34) 19 34)
                                                              (make-sprite-source #.(* 11 19) #.(* 0 34) 19 34))
                                              :time-between-frames-ms 150)))
   )
  (:documentation "Human controlled entity."))

(set-default-input-command-map
 player
 ("controller" (:13 :move-left)
               (:12 :move-down)
               (:14 :move-right)
               (:11 :move-up))
 ("sdl-keyboard" (:scancode-right :move-right)
                 (:scancode-l :move-right)
                 (:scancode-left :move-left)
                 (:scancode-h :move-left)
                 (:scancode-space :jump)))

(let* ((move-delta 3000)
       (right-vec  (make-acceleration-vector-seconds :x move-delta))
       (left-vec   (make-acceleration-vector-seconds :x (- move-delta))))
  (set-default-command-action-map
   player
   (:jump (while-active
           (jump player)))
   (:move-right (while-active
                 (when (find :west (facing player))
                   (flip player :horizontal))
                 (push-direction player :east)
                 (apply-vector player right-vec)))
   (:move-left (while-active
                (when (find :east (facing player))
                  (flip player :horizontal))
                (push-direction player :west)
                (apply-vector player left-vec)))))

(defmethod get-new-animation ((player player))
  (cond
    ((not (is-touching-ground player))
     (if (< (velocity-y player) 0)
         :jumping
         :falling))
    ((> (abs (velocity-x player)) 0.05) :running)
    (T :standing)))

(defclass my-scene-input-handler (input-handler)
  ())

(set-default-input-command-map
 my-scene-input-handler
 ("sdl-keyboard" (:scancode-q :quit)
                 (:scancode-r :reload)))

(set-default-command-action-map
 my-scene-input-handler
 (:quit              (on-deactivate (quit)))
 (:reload            (on-deactivate
                      (let ((x (x *player*))
                            (y (y *player*))
                            (z (z *player*)))
                        (change-scene *engine-manager* (launch-jumpguy))
                        (setf (x *player*) x
                              (y *player*) y
                              (z *player*) z)))))

(defclass myscene (platformer-game-scene)
  ((scene-input-handler
    :documentation "Input handler for the main scene."
    :initform (make-instance 'my-scene-input-handler
                             :active-input-device *all-input-id*)))
  (:documentation "Default scene for my game"))

(defmethod update :before ((scene myscene) delta-t-ms world-context)
  (update (slot-value scene 'scene-input-handler) delta-t-ms scene))

(defparameter *player* nil)

(defun launch-jumpguy ()
  (let* ((demo-width 1024)
         (demo-height 768)
         (world (make-instance 'myscene
                               :width demo-width :height demo-height
                               :background (make-instance 'static-sprite
                                                          :path-to-image (resource-path "art/backgrounds/mountains_and_hills.png")
                                                          :width demo-width
                                                          :height demo-height)
                               :music (resource-path "music/james_song27_riff_with_layers.ogg")
                               :camera (make-instance 'camera
                                                      :pixels-per-unit 1
                                                      :zoom 1
                                                      :min-x 0 :min-y 0
                                                      :max-x demo-width
                                                      :max-y demo-height
                                                      :target-max-offset 20)))

         (player (make-instance 'player
                                :x 50
                                :y (/ demo-height 2)
                                :width 42
                                :height 66))
         (objects (list player
                        ;; put an invisible box around the boundary
                        (make-instance 'aabb
                                       :x 0
                                       :y 0
                                       :width 1
                                       :height demo-height)
                        (make-instance 'aabb
                                       :x 0
                                       :y 0
                                       :width demo-width
                                       :height 1)
                        (make-instance 'aabb
                                       :x demo-width
                                       :y 0
                                       :height demo-height
                                       :width 1)
                        (make-instance 'aabb
                                       :x 0
                                       :y demo-height
                                       :height 1
                                       :width demo-width))))
    (setf (clear-color *engine-manager*)
          *green*)
    (loop for game-object in objects do
         (add-to-scene world game-object))
    (setf (target (camera world)) player)
    (setf (active-input-device player) -1)
    (setf *player* player)
    world))

#+nil
(recurse.vert:main #'jumpguy::game-menu
                   :game-name "JumpGuy"
                   :block nil)
