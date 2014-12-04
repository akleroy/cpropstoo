function rob_mapfit,map,ndeg, cc, sig, single=precision, floor=minval
;
;+
; NAME:
;	 ROB_MAPFIT 
;
; PURPOSE:
;	Robustly fit a polynomial surface to a 2D floating-point array
;
; CALLING SEQUENCE:
;	surface = ROB_MAPFIT( map,ndeg, coefficients, Sigma, [/SINGLE, FLOOR= ])
;
; INPUTS:
;	map = array to fit, floating-point
;	ndeg= degree of polynomials to be fitted. 1: Z=a+bX+cY
;						2: Z=a+bX+cY+eX^2+eY^2+fXY
;				3: Z=a+bX+cY+eX^2+eY^2+fX^3+gY^3+hX^2Y+iY^2X
;						and so on...
;		Maximum degree = 7 (35 terms)
;
; OUTPUT:
;	ROB_MAPFIT returns the calculated fitted surface at each pixel: 
;	an array like the input array.
;
; OPTIONAL INPUT KEYWORDS:
;	FLOOR = the minimum value allowed. Pixels <= this are considered vacant.
;		The default = -1.0e20
;	SINGLE  if set, single precision is used. This should be done only if 
;		there is not enough memory for the default double-precision 
;		calculations.
;
; OPTIONAL OUTPUT:
;	coefficients = the coefficients of the fit, in the order [a,b,c,...] 
;		as above.
;	sigma = a robust analog of the std. deviation of the residuals
;
; SUBROUTINES NEEDED:
;	Planefit, Robust_planefit, Quarticfit, Rob_Quarticfit, Rob_Checkfit,
;	Robust_Sigma, Med, REGRESS (in USERLIB)
;
; NOTES:
;	This routine will usually give excellent results as long as no more than
;	25% of the pixels are contaminated. When more than 25% are affected, it
;	becomes harder to obtain the underlying shape. However, it will still do
;	a pretty good job even if the data is a mess. But try to keep the degree
;	of the fit down. Best if #pts >> degree
; 
; REVISION HISTORY: 
;	Written,  H.T. Freudenreich, HSTX, 5/20/93
;-

on_error,2

if keyword_set(minval)    then empty=minval else empty=1.0e-20
if keyword_set(precision) then singlep=1    else singlep=0

nterms=[1,3,5,9,14,20,27,35]
maxdeg = n_elements(nterms)

if ndeg gt maxdeg then begin
   print,' ROB_MAPFIT: Maximum degree = ',maxdeg,'!'
   return,map   
endif

; Get the needed dimensions:

syz=size(map)
nx=syz(1)   &   ny=syz(2)
ntot=nx*ny

q=where(map gt empty, ngood )

if ngood lt (nterms(ndeg)+2) then begin
   print,'ROB_MAPFIT: Too few points!'
   pap=map
   return,pap
endif

if ndeg gt 7 then begin
   print,'ROB_MAPFIT: Maximum degree in X and Y is 7! Setting degree to 7'
endif

; Obtain (X,Y) coordinates of the pixels:

u=findgen(nx)     &  v=findgen(ny)
xx=fltarr(nx,ny)  &  yy=fltarr(nx,ny)

for i=0,ny-1 do xx(*,i)=u  &  u=0
for i=0,nx-1 do yy(i,*)=v  &  v=0

x=reform(xx,ntot) & xx=0
y=reform(yy,ntot) & yy=0 
z=reform(map,ntot)

; For efficiency, use specialized routines in the case of a plane or surface
; quadratic in X and Y.

if ndeg eq 1 then begin
   cc=robust_planefit(x,y,z,zfit,sig,floor=empty)  
   
endif else begin
;  Go to double-precision:
   if n_params() lt 4 then begin
      if singlep eq 0 then begin
         x=double(x)  &  y=double(y)  &  z=double(z)
      endif
   endif 
   if ndeg eq 2 then begin
      cc = rob_quarticfit(x,y,z,zfit,sig,floor=empty)
   endif else begin
      if singlep eq 1 then xxx=fltarr(nterms(ndeg),ntot) else $
                           xxx=dblarr(nterms(ndeg),ntot)

      x2 = x*x       & y2 = y*y

      xxx(0,*)=x     & xxx(1,*)=y
      xxx(2,*)=x2    & xxx(3,*)=y2
      xxx(4,*)=x*y  

      xxx(5,*)=x2*x   & xxx(6,*)=y2*y
      xxx(7,*)=x2*y   & xxx(8,*)=y2*x

      if ndeg gt 3 then begin
         xxx(9,*) =x2*x2    & xxx(10,*)=y2*y2
         xxx(11,*)=x2*x*y   & xxx(12,*)=y2*y*x
         xxx(13,*)=x2*y2
      
         if ndeg gt 4 then begin
            xxx(14,*)=x^5      & xxx(15,*)=y^5
            xxx(16,*)=x2*x2*y  & xxx(17,*)=y2*y2*x
            xxx(18,*)=x^3*y2   & xxx(19,*)=y^3*x2

            if ndeg gt 5 then begin
               xxx(20,*)=x^6      & xxx(21,*)=y^6
               xxx(22,*)=x^5*y    & xxx(23,*)=y^5*x
               xxx(24,*)=x^4*y2   & xxx(25,*)=y^4*x2
               xxx(26,*)=x^3*y^3  

               if ndeg gt 6 then begin
                  xxx(27,*)=x^7      & xxx(28,*)=y^7
                  xxx(29,*)=x^6*y    & xxx(30,*)=y^6*x
                  xxx(31,*)=x^5*y2   & xxx(32,*)=y^5*x2
                  xxx(33,*)=x^4*y^3  & xxx(34,*)=y^4*x^3
               endif
            endif
         endif
      endif

      cc = robust_regress( xxx,z, zfit, sig, floor=empty )

      xxx=0
   endelse
;  Convert back to single-precision:
   cc=float(cc)
   zfit=float(zfit)
   sig=float(sig)
endelse

; If the fit was made, convert back to 2D:
if n_elements(cc) gt 1 then pap=reform(zfit,nx,ny) else $
                            pap=map

return,pap
end
