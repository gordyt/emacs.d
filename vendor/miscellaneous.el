(defun pot-volume-in-gallons (diameter-in height-in)
  "Computes the volume, in gallons, of a cooking pot whose interior 
  diameter is DIAMETER-IN and whose interior height is HEIGHT-IN"
  (let* ((radius-in (/ (float diameter-in) 2))
		 (vol-cu-in (* pi radius-in radius-in height-in))
		 (vol-str (format "%s in^3" vol-cu-in)))
	(string-to-number (calcFunc-uconv (calc-eval vol-str 'raw) "gal"))))

