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


(defclass player (jumper rectangle input-handler)
  ()
  (:documentation "Player-controlled rectangle."))

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
                 (apply-vector player right-vec)))
   (:move-left (while-active
                (apply-vector player left-vec)))))

(defclass my-scene-input-handler (input-handler)
  ())

(set-default-input-command-map
 my-scene-input-handler
 ("sdl-keyboard" (:scancode-q :quit)))

(set-default-command-action-map
 my-scene-input-handler
 (:quit              (on-deactivate (quit))))

(defclass myscene (platformer-game-scene)
  ((scene-input-handler
    :documentation "Input handler for the main scene."
    :initform (make-instance 'my-scene-input-handler
                             :active-input-device *all-input-id*)))
  (:documentation "Default scene for my game"))

(defmethod update :before ((scene myscene) delta-t-ms world-context)
  (update (slot-value scene 'scene-input-handler) delta-t-ms scene))

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
                                :width 25
                                :height 25))
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
    world))

#+nil
(recurse.vert:main #'jumpguy::game-menu
                   :game-name "JumpGuy"
                   :block T)
