(require 'dom)
(require 'svg)
(defun musikmacs--y-to-pixel (y)
  (* dheight (- staff-y y)))
(defmacro musikmacs--def-record (name fieldlist)
  (let ((prefix (concat "musikmacs--" (symbol-name name) "-"))
        (field-no 0))
    (cons 'progn
          (mapcar (lambda (fieldname)
                    (setq field-no (+ field-no 1))
                    `(progn
                       (defun ,(intern (concat prefix (symbol-name fieldname)))
                           (rec)
                            (aref rec ,field-no))
                       (defun ,(intern (concat prefix (symbol-name fieldname) "-set"))
                             (rec val)
                            (aset rec ,field-no val)))) fieldlist))))
(musikmacs--def-record staff (y clef key))
(musikmacs--def-record note-group (x dot value ylist))

(defun musikmacs--render-row (row-data
                              staff-list
                              lower-y)
  (let ((wwidth (window-body-width nil t))
        (dheight (/ (face-attribute 'default :height) 20.0))
        (dcolor (face-attribute 'default :foreground)))
        (let ((svg (svg-create wwidth
                               (* dheight lower-y)
                               :stroke-color dcolor
                               :stroke-width (/ dheight 5.0)
                               )))
          ;; adapted from https://commons.wikimedia.org/wiki/Category:SVG_musical_notation#/media/File:Figure_rythmique_blanche_et_noire_liee.svg
          ;; license JLTB34 [CC BY-SA 3.0 (https://creativecommons.org/licenses/by-sa/3.0)]
          (svg--def svg
                    (dom-node 'g
                              '((id . "half-body"))
                              (dom-node 'path
                                        `((fill . "inherit")
                                          (stroke . "none")
                                          (transform . ,(format "scale(%f)" (/ dheight 6.0)))
                                          (d . "m 3.8,-4.5 c -3.39584,1.81997 -5.20528,5.11455 -4.09222,7.59296 1.18726,2.64364 5.26701,3.38972 9.10658,1.66537 3.83956,-1.72436 5.99216,-5.2694 4.8049,-7.91303 -1.18727,-2.64364 -5.26701,-3.38972 -9.10658,-1.66537 -0.23997,0.10777 -0.48628,0.19874 -0.71268,0.32007 z m 1.10973,2.24216 c 0.24123,-0.12433 0.48548,-0.21803 0.74119,-0.33287 3.27307,-1.46994 6.43421,-1.53907 7.05611,-0.15431 0.6219,1.38476 -1.52978,3.70162 -4.80285,5.17157 -3.27308,1.46994 -6.43422,1.53907 -7.05611,0.15431 -0.57332,-1.27658 1.21513,-3.37161 4.06166,-4.8387 z"))
                                        )))
          (svg--def svg
                    (dom-node 'g
                              '((id . "quarter-body"))
                              (dom-node 'path
                                        `((fill . "inherit")
                                          (stroke . "none")
                                          (transform . ,(format "scale(%f)" (/ dheight 6.0)))
                                          (d . "m 3.8,-4.5 c -3.39584,1.81997 -5.20528,5.11455 -4.09222,7.59296 1.18726,2.64364 5.26701,3.38972 9.10658,1.66537 3.83956,-1.72436 5.99216,-5.2694 4.8049,-7.91303 -1.18727,-2.64364 -5.26701,-3.38972 -9.10658,-1.66537 -0.23997,0.10777 -0.48628,0.19874 -0.71268,0.32007 z"))
                                        )))
          ;; adapted from https://commons.wikimedia.org/wiki/File:Figure_rythmique_ronde.svg
          ;; license Christophe Dang Ngoc Chan (cdang) [CC BY-SA 3.0 (http://creativecommons.org/licenses/by-sa/3.0/)]
          (svg--def svg
                    (dom-node 'g
                              '((id . "whole-body"))
                              (dom-node 'path
                                        `((fill . "inherit")
                                          (stroke . "none")
                                          (transform . ,(apply 'format (cons "matrix(%f, 0, 0, %f, %f, %f)" (mapcar
                                                                                                (lambda (x) (* x (/ dheight 6.0)))
                                                                                                '(1.0 1.0 -4.0 -5.0)))))
                                          (d . "M 9.125,0.0030002594 C 4.03428,0.18520026 0,2.5856003 0,5.5030003 C 0,8.5390003 4.368,11.003 9.75,11.003 C 15.132,11.003 19.5,8.5390003 19.5,5.5030003 C 19.5,2.4670003 15.132,0.0030002594 9.75,0.0030002594 C 9.53977,0.0030002594 9.33194,-0.0043997406 9.125,0.0030002594 z M 7.5,1.0655003 C 8.8579,0.92650026 10.56798,1.5561003 12,2.8467003 C 14.14502,4.7799003 14.87122,7.4906003 13.625,8.9092003 L 13.59375,8.9405003 C 12.32289,10.3506 9.53145,9.9153003 7.375,7.9717003 C 5.21855,6.0282003 4.51039,3.2881003 5.78125,1.8780003 C 6.20818,1.4043003 6.81306,1.1358003 7.5,1.0655003 z "))
                                        )))
          (svg--def svg (dom-node 'g
                                  '((id . "whole-body-left"))
                                  (dom-node 'use
                                            `((xlink:href . "#whole-body")
                                              (x . ,(* dheight -2.23))
                                              (y . 0)))))
          (svg--def svg (dom-node 'g
                                  '((id . "half-body-left"))
                                  (dom-node 'use
                                            `((xlink:href . "#half-body")
                                              (x . ,(* dheight -2.23))
                                              (y . 0)))))
          (svg--def svg (dom-node 'g
                                  '((id . "quarter-body-left"))
                                  (dom-node 'use
                                            `((xlink:href . "#quarter-body")
                                              (x . ,(* dheight -2.23))
                                              (y . 0)))))
          (dolist (staff staff-list)
            (let ((staff-y (musikmacs--staff-y staff)))
              (dotimes (i 5)
                      (let ((y (musikmacs--y-to-pixel (* 2 (- i 2)))))
                        (svg-line svg 0 y
                                  wwidth y)))))
          (let ((horizontal-unit 10))
            (mapc 'musikmacs--render-col row-data))
          (svg-image svg))))
(defun musikmacs--render-col (col-data)
  (let ((current-group-cell col-data))
    (mapc (lambda (staff)
            (let ((staff-y (musikmacs--staff-y staff)))
              (musikmacs--render-group (car current-group-cell) t)
              (musikmacs--render-group (cadr current-group-cell) nil)
              (setq current-group-cell (cddr current-group-cell))))
          staff-list)))
(defun musikmacs--render-group (note-group stem-up)
  (let ((notes-x (* horizontal-unit (musikmacs--note-group-x note-group)))
        (notes-ylist (musikmacs--note-group-ylist note-group))
        (min-y (car (musikmacs--note-group-ylist note-group)))
        (max-y (car (last (musikmacs--note-group-ylist note-group))))
        (last-y nil)
        (x-off (* dheight 1.115))
        (time-value (musikmacs--note-group-value note-group)))
    (setq notes-x (if stem-up
                      (+ notes-x x-off)
                    (- notes-x x-off)))
    (mapc (lambda (y)
            (let
                ((reversing (if (and last-y (= y (+ last-y 1)))
                                (progn
                                  (setq last-y nil)
                                  t)
                              (progn
                                (setq last-y y)
                                nil)))
                 (note-yp (musikmacs--y-to-pixel y)))
              (svg--append
               svg
               (dom-node 'use
                         `((xlink:href . ,(if (equal reversing stem-up)
                                              (format "#%s-body" time-value)
                                            (format "#%s-body-left" time-value)))
                           (x . ,notes-x)
                           (y . ,note-yp)
                           (fill . ,dcolor))))
              (when (and reversing (or (> y 5) (< y -5)))
                (svg-line svg notes-x
                          note-yp
                          (if stem-up
                              (+ notes-x (* dheight 2.63))
                            (- notes-x (* dheight 2.63)))
                          note-yp))))
          notes-ylist)
    (let ((start-x (if stem-up
                           (+ notes-x (* dheight 0.4))
                         (- notes-x (* dheight 0.4))))
              (end-x (if stem-up
                           (- notes-x (* dheight 2.63))
                       (+ notes-x (* dheight 2.63)))))
      (when (> max-y 5)
        (dotimes (i (/ (- max-y 4) 2))
          (svg-line svg start-x
                    (musikmacs--y-to-pixel (+ (* i 2) 6))
                    end-x
                    (musikmacs--y-to-pixel (+ (* i 2) 6)))))
      (when (< min-y -5)
        (dotimes (i (/ (- -4 min-y) 2))
          (svg-line svg start-x
                    (musikmacs--y-to-pixel (- -6 (* i 2)))
                    end-x
                    (musikmacs--y-to-pixel (- -6(* i 2)))))))
    (when (not (eq time-value 'whole))
      (progn
      (if stem-up
        (cond ((> max-y 0) (setq max-y (+ max-y 5)))
              ((> max-y -7) (setq max-y (+ max-y 7)))
              (t (setq max-y 0)))
      (cond ((< min-y 0) (setq min-y (- min-y 5)))
            ((< min-y 7) (setq min-y (- min-y 7)))
            (t (setq min-y 0))))
    (if stem-up
        (setq min-y (+ min-y 0.2))
      (if last-y
          (setq max-y (- max-y 0.2))
        (setq max-y (+ max-y 0.2))))
    (svg-line svg
               notes-x
               (musikmacs--y-to-pixel max-y)
               notes-x
               (musikmacs--y-to-pixel min-y))))))
(insert-image (musikmacs--render-row `((,(record 'musikmacs--note-group
                                                 10 nil 'whole '(-7 -6 -5 -4))
                                        ,(record 'musikmacs--note-group
                                                 10 nil 'quarter '(0 2))
                                        ,(record 'musikmacs--note-group
                                                 10 nil 'whole '(-7 -6 -5 -4))
                                        ,(record 'musikmacs--note-group
                                                 10 nil 'quarter '(0 2)))
                                       (,(record 'musikmacs--note-group
                                                 20 nil 'quarter '(1 3))
                                        ,(record 'musikmacs--note-group
                                                 20 nil 'quarter '(-4 -2))
                                        ,(record 'musikmacs--note-group
                                                 20 nil 'quarter '(1 3))
                                        ,(record 'musikmacs--note-group
                                                 20 nil 'quarter '(-4 -2)))
                                       )
                                     `(,(record 'staff 10 nil nil)
                                       ,(record 'staff 30 nil nil)) 50))
