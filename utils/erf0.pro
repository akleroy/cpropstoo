function erf0, x, erftarg = erftarg

; Helper function allowing error function at a target value to be
; solved by the bisection root finder.

  return, errorf(x)-erftarg

end
