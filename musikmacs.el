(require 'dom)
(require 'svg)
(defun musikmacs--y-to-pixel (y)
  (* dheight (- staff-y y)))
(defmacro musikmacs--def-record (name fieldlist)
  (let ((prefix (concat "musikmacs--" (symbol-name name) "-"))
        (typesym (intern (concat "musikmacs--" (symbol-name name))))
        (field-no 0))
    (cons 'progn
          (cons `(defun ,typesym
                     ,fieldlist
                   ,(cons 'record (cons `(quote ,typesym) fieldlist))
                   )
                (mapcar (lambda (fieldname)
                    (setq field-no (+ field-no 1))
                    `(progn
                       (defun ,(intern (concat prefix (symbol-name fieldname)))
                           (rec)
                            (aref rec ,field-no))
                       (defun ,(intern (concat prefix (symbol-name fieldname) "-set"))
                             (rec val)
                         (aset rec ,field-no val)))) fieldlist)))))
(musikmacs--def-record staff (y clef key))
(musikmacs--def-record note-group (dot value list))
(musikmacs--def-record note (y state text))
(musikmacs--def-record col (x staff-cells addons))
(musikmacs--def-record staff-cell (note-group-up note-group-down addons))

(defun musikmacs--render-row (object
                              start
                              end
                              horizontal-unit
                              cursor-pos
                              lower-y)
  (let ((dcolor (face-attribute 'default :foreground)))
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
          (dolist (staff (get-text-property start :musikmacs--staves object))
            (let ((staff-y (musikmacs--staff-y staff)))
              (dotimes (i 5)
                      (let ((y (musikmacs--y-to-pixel (* 2 (- i 2)))))
                        (svg-line svg 0 y
                                  wwidth y)))))
          (dotimes (i (- end start))
            (musikmacs--render-col
             (get-text-property (+ start i) :musikmacs--col object)
             (get-text-property (+ start i) :musikmacs--staves object)
             (equal i cursor-pos)))
          (svg-image svg))))
(defun musikmacs--render-col (col-data
                              staff-list
                              selected)
  (let* ((staff-cells (musikmacs--col-staff-cells col-data))
         (notes-x (* horizontal-unit (musikmacs--col-x col-data)))
         (addons (musikmacs--col-addons col-data)))
    (mapc (lambda (staff)
            (let ((staff-y (musikmacs--staff-y staff))
                  (staff-cell (car staff-cells)))
              (musikmacs--render-group (musikmacs--staff-cell-note-group-up staff-cell) t selected)
              (musikmacs--render-group (musikmacs--staff-cell-note-group-down staff-cell) nil selected)
              (setq staff-cells (cdr staff-cells))))
          staff-list)
    (mapc (lambda (addon)
            (cond ((eq 'barline addon)
                   (svg-line svg
                             notes-x
                             (let ((staff-y (musikmacs--staff-y (car staff-list))))
                               (musikmacs--y-to-pixel 4))
                             notes-x
                             (let ((staff-y (musikmacs--staff-y (car (last staff-list)))))
                               (musikmacs--y-to-pixel -4))))))
          addons)))
(defun musikmacs--render-group (note-group stem-up selected)
  (let ((notes-list (musikmacs--note-group-list note-group)))
    (unless (null notes-list)
      (let ((min-y (musikmacs--note-y (car (musikmacs--note-group-list note-group))))
             (max-y (musikmacs--note-y (car (last (musikmacs--note-group-list note-group)))))
             (min-stem-y)
             (max-stem-y)
             (last-y nil)
             (x-off (* dheight 1.115))
             (time-value (musikmacs--note-group-value note-group)))
        (setq notes-x (if stem-up
                                 (+ notes-x x-off)
                               (- notes-x x-off)))
       (mapc (lambda (n)
               (unless (eq 'candidate (musikmacs--note-state n))
                 (unless min-stem-y (setq min-stem-y (musikmacs--note-y n)))
                 (setq max-stem-y (musikmacs--note-y n)))) notes-list)
       (mapc (lambda (n)
               (let ((y (musikmacs--note-y n))
                     (state (musikmacs--note-state n)))
                 (let
                     ((reversing (if (and last-y (= y (+ last-y 1)))
                                     (progn
                                       (setq last-y nil)
                                       t)
                                   (progn
                                     (setq last-y y)
                                     nil)))
                      (note-yp (musikmacs--y-to-pixel y))
                      (ncolor (cond ((and selected (eq state 'selected)) (face-attribute font-lock-doc-face :foreground))
                                    ((or
                                      (eq state 'active)
                                      (eq state 'candidate)) (face-attribute font-lock-builtin-face :foreground))
                                    (t dcolor)))
                      (text (musikmacs--note-text n)))
                   (if (eq state 'candidate)
                       (svg-text svg
                                 text
                                 :x notes-x
                                 :y (+ note-yp dheight)
                                 :fill ncolor
                                 :font-family (face-attribute 'default :family)
                                 :text-anchor (if (equal reversing stem-up)
                                                  "start"
                                                "end")
                                 :stroke "none"
                                 :font-size (* dheight 4.0))
                     (svg--append
                      svg
                      (dom-node 'use
                                `((xlink:href . ,(if (equal reversing stem-up)
                                                     (format "#%s-body" time-value)
                                                   (format "#%s-body-left" time-value)))
                                  (x . ,notes-x)
                                  (y . ,note-yp)
                                  (fill . ,ncolor)))))
                   (when (and reversing (or (> y 5) (< y -5)))
                     (svg-line svg notes-x
                               note-yp
                               (if stem-up
                                   (+ notes-x (* dheight 2.63))
                                 (- notes-x (* dheight 2.63)))
                               note-yp)))))
             notes-list)
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
               (cond ((> max-stem-y 0) (setq max-stem-y (+ max-stem-y 5)))
                     ((> max-stem-y -7) (setq max-stem-y (+ max-stem-y 7)))
                     (t (setq max-stem-y 0)))
             (cond ((< min-stem-y 0) (setq min-stem-y (- min-stem-y 5)))
                   ((< min-stem-y 7) (setq min-stem-y (- min-stem-y 7)))
                   (t (setq min-stem-y 0))))
           (if stem-up
               (setq min-stem-y (+ min-stem-y 0.2))
             (if last-y
                 (setq max-stem-y (- max-stem-y 0.2))
               (setq max-stem-y (+ max-stem-y 0.2))))
           (svg-line svg
                     notes-x
                     (musikmacs--y-to-pixel max-stem-y)
                     notes-x
                     (musikmacs--y-to-pixel min-stem-y))))))))
(defun musikmacs--note-group-insert-candidate (note-group y text)
  (musikmacs--note-group-list-set note-group
                                  (cons (musikmacs--note y 'candidate text)
                                        (musikmacs--note-group-list note-group))))
(defun musikmacs--note-group-select-candidate (note-group text)
  (mapcar (lambda (note)
            (if (equal text (musikmacs--note-text note))
                (musikmacs--note-state-set note 'active)
              (when (eq 'active (musikmacs--note-state note))
                (musikmacs--note-state-set 'candidate)))) (musikmacs--note-group-list note-group)))
(defun musikmacs--note-group-enter-candidate (note-group)
  (let ((note-list (musikmacs--note-group-list-set note-group
                    (cons nil (musikmacs--note-group-list note-group)))))
    (while (cdr note-list)
      (let* ((note (cadr note-list))
             (note-state (musikmacs--note-state note)))
        (cond ((eq note-state 'active) (musikmacs--note-state-set note 'selected))
              ((eq note-state 'selected) (musikmacs--note-state-set note nil))
              ((eq note-state 'candidate) (setcdr note-list (cddr note-list))))
        (setq note-list (cdr note-list))))
    (musikmacs--note-group-list-set note-group (cdr (musikmacs--note-group-list note-group)))))

(defun musikmacs--eat-ret ()
  (while (and (char-after) (char-equal ?\n (char-after)))
    (delete-char 1)))
(defun musikmacs--note-group-units (ng)
  (let ((time-value (musikmacs--note-group-value ng)))
    (cond ((eq 'whole time-value) 3.0)
          ((eq 'half time-value) 2.0)
          ((eq 'quarter time-value) 1.5)
          ((eq 'eighth time-value) 1.0))))
(defun musikmacs-refresh-at-point ()
  "Rerender SVG scores after current-point of current-buffer if needed."
  (let ((selected-pos) (point))
    (save-excursion
            (beginning-of-line)
            (put-text-property (point) (+ (point) 1)
                               :musikmacs--line-start nil)
            (let* ((line-start-point (point))
                   (wwidth (window-body-width nil t))
                   (dheight (/ (face-attribute 'default :height) 20.0))
                   (preferred-units-per-line (/ wwidth (* dheight 4.0)))
                   (maximal-unit-width (* dheight 4.0)))
              (while (and (not (equal (point) (point-max))) ;; EOF
                          (not (get-text-property (point) :musikmacs--line-start)))
                (put-text-property (point) (+ (point) 1)
                                   :musikmacs--line-start t)
                (let ((used-units 0)
                      (current-col-data (get-text-property (point) :musikmacs--col)))
                  (while (not (or
                               (equal (point) (point-max)) ;; EOF
                               (and
                                (> used-units preferred-units-per-line)
                                (member 'barline (musikmacs--col-addons current-col-data))) ;; row filled
                               ))
                    (musikmacs--col-x-set current-col-data used-units)
                    (setq used-units (+ used-units
                                        (apply 'min (mapcar
                                                     (lambda (cell)
                                                       (min (musikmacs--note-group-units (musikmacs--staff-cell-note-group-up cell))
                                                            (musikmacs--note-group-units (musikmacs--staff-cell-note-group-down cell))))
                                                     (musikmacs--col-staff-cells current-col-data)))))
                    (forward-char)
                    (musikmacs--eat-ret)
                    (unless (equal (point) (point-max))
                      (setq current-col-data (get-text-property (point) :musikmacs--col))))
                  (setq used-units (- used-units 1.0)) ;; exclude the final bar line
                  (put-text-property line-start-point
                                     (point)
                                     'display
                                     (musikmacs--render-row (current-buffer)
                                                            line-start-point
                                                            (point)
                                                            (min maximal-unit-width
                                                                 (/ wwidth used-units))
                                                            selected-pos
                                                            20))
                  (insert-char ?\n)
                  ))))))

(defvar musikmacs-mode-map nil "keymap for `musikmacs-mode'")
(setq musikmacs-mode-map (let ((map (make-sparse-keymap)))
                             (suppress-keymap map)
                             map))
(define-derived-mode musikmacs-mode text-mode "musiKmacs"
  (delete-region (point-min) (point-max))
  (insert x)
  (musikmacs-refresh-at-point))


(setq test-group (record 'musikmacs--note-group
                         nil 'whole (list (musikmacs--note -7 'candidate "x")
                                             (musikmacs--note -5 'candidate "y")
                                             (musikmacs--note -4 nil nil))))
(musikmacs--note-group-insert-candidate test-group -10 "z")
(musikmacs--note-group-select-candidate test-group "x")
(musikmacs--note-group-enter-candidate test-group)
(setq x "123")
(put-text-property 0 1 :musikmacs--col (musikmacs--col 10 (list (musikmacs--staff-cell test-group (record 'musikmacs--note-group
                                                                                                      nil 'quarter (list (musikmacs--note 1 'selected nil) (musikmacs--note 3 nil nil) (musikmacs--note 4 'candidate "h"))) nil)) nil) x)
(put-text-property 1 2 :musikmacs--col (musikmacs--col 20 (list (musikmacs--staff-cell test-group (record 'musikmacs--note-group
                                                                                    nil 'quarter (list (musikmacs--note 1 'selected nil
                                                                                                                        ) (musikmacs--note 3 nil nil) (musikmacs--note 4 nil "x"))) nil)) nil) x)
(put-text-property 2 3 :musikmacs--col (musikmacs--col 30 (list (musikmacs--staff-cell (musikmacs--note-group nil 'whole nil)(musikmacs--note-group nil 'whole nil) nil)) '(barline)) x)

(put-text-property 0 3 :musikmacs--staves (list (record 'staff 10 nil nil)) x)
(insert-image (musikmacs--render-row x 0 3 0 20))
