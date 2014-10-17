function moments_template $
   , props $
   , x = xin $
   , y = yin $
   , v = vin $
   , t = tin $
   , define_fields = define_fields $   
   , calculate = calculate $
   , moments = moments $
   , properties = properties
  
;+
;
; Moment calculation module template. Will either define fields,
; calculate moments, or convert moments into properties, depending on
; the calling syntax. This version is a template showing the necessary
; parts of the code, but it does not do anything.
;
;-

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; DEFINE FIELDS 
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; Add the necessary fields to the properties structure

; FOR CONVENIENCE, ASSIGN "not-a-number" TO THE nan VARIABLE
  nan = !values.f_nan

  if keyword_set(define_fields) then begin

;    ... ADD FIELDS FOR MOMENTS
     if keyword_set(moments) then begin
        
; Here you add any fields to the input property structure that will be
; calculated in the moments calculation. See the "classic" calculation
; for examples of syntax.

        return, props

     endif

;    ... ADD FIELDS FOR PROPERTIES
     if keyword_set(properties) then begin

; Here you add any fields to the input property structure that will be
; calculated when converting moments into properties. See the
; "classic" calculation for examples of syntax.

        return, props

     endif

  endif

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; CALCULATE MOMENTS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  if keyword_set(calculate) and keyword_set(moments) then begin

; Here you calculate the moments and record them in the property
; structure and then return the structure. See the "classic"
; calculation for examples.

     return, props

  endif
  
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; CONVERT MOMENTS TO PROPERTIES
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  if keyword_set(calculate) and keyword_set(props) then begin

; Here you convert from a set of measured moments and use them to
; construct physical properties. See the "classic" calculation for
; examples.

     return, props

  endif

; YOU SHOULD NOT GET HERE
  message, "No calculation carried out. Check calling sequence.", /info
  return, nan

end
   
