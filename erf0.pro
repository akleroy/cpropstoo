function erf0, x, erftarg = erftarg
; Where error function equals a given value.
  return, errorf(x)-erftarg
end
