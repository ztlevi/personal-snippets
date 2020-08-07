(defun ivy-posframe-display-at-frame-top-center (str)
  (ivy-posframe--display str #'posframe-poshandler-frame-top-center))

(defun posframe-poshandler-frame-top-center (info)
  "Posframe's position handler.

Get a position which let posframe stay onto its
parent-frame's top center.  The structure of INFO can
be found in docstring of `posframe-show'."
  (let* ((posframe (plist-get info :posframe))
         (parent-frame (plist-get info :parent-frame)))
    (cons (/ (- (frame-pixel-width parent-frame)
                (frame-pixel-width posframe))
             2)
          10)))
