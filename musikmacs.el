(require 'dom)
(require 'svg)
(defun musikmacs--y-to-pixel (y)
  (* dheight (- upper-y y)))
(defun musikmacs--render-row (row-data
                              upper-y
                              lower-y)
  (let ((wwidth (window-body-width nil t))
        (dheight (/ (face-attribute 'default :height) 10))
        (dcolor (face-attribute 'default :foreground)))
        (let ((svg (svg-create wwidth
                               (* dheight (- upper-y lower-y))
                               :stroke-color dcolor
                               )))
            (svg--def svg
                    (dom-node 'g
                              '((id . "quarter-body")
                                (transform . "matrix(-1.000014,0,0,-1.000014,247.8341,259.9378)"))
                              (dom-node 'path
                                        `((fill . ,dcolor)
                                          (d . "m 237.68484,218.18353 c -3.39584,1.81997 -5.20528,5.11455 -4.09222,7.59296 1.18726,2.64364 5.26701,3.38972 9.10658,1.66537 3.83956,-1.72436 5.99216,-5.2694 4.8049,-7.91303 -1.18727,-2.64364 -5.26701,-3.38972 -9.10658,-1.66537 -0.23997,0.10777 -0.48628,0.19874 -0.71268,0.32007 z m 1.10973,2.24216 c 0.24123,-0.12433 0.48548,-0.21803 0.74119,-0.33287 3.27307,-1.46994 6.43421,-1.53907 7.05611,-0.15431 0.6219,1.38476 -1.52978,3.70162 -4.80285,5.17157 -3.27308,1.46994 -6.43422,1.53907 -7.05611,0.15431 -0.57332,-1.27658 1.21513,-3.37161 4.06166,-4.8387 z"))
                                        )))
          (dotimes (i 5)
            (let ((y (musikmacs--y-to-pixel (- i 2))))
              (svg-line svg 0 y
                        wwidth y)))
          (svg-image svg))))
(defun musikmacs--render-group (svg
                                note-group
                                element-width
                                element-height
                                spacing-width)
  (svg--append
   svg
   (dom-node 'use
             `((xlink:href . "#quarter-body")
               (x. 10)
               (y. 10))))
  )
(insert-image (musikmacs--render-row nil 4 -4))
