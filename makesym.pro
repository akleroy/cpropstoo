pro MakeSym, symbol
;+
;  PROCEDURE:
;	MAKESYM
;  PURPOSE:
;	Call USERSYM to create one of the below plot symbols.
;  CALLING SEQUENCE:
;  	Makesym, SYMBOL
;  INPUTS:
;	SYMBOL - number indicating which symbol you want.  If not within the 
;	range of available symbols, nothing is done.
;	9  - Circle
;	10  - Filled Circle
;	11 - Filled Diamond
;	12 - Filled Triangle
;	13 - Filled Square
;	14 - 5-point star
;	15 - Filled 5-point star
;	16 - Pentagon
;	17 - Filled pentagon
;	18 - Hexagon
;	19 - Filled Hexagon
;  OUTPUTS:
;	None.
;	USERSYM is called, which sets the default user plot symbol.
;  Restrictions:
;	Does not account for the histogram plotting style.
;  History:
;	Joel D. Offenberg, Hughes STX, 21 Jan 1993
;	Added symbols 14-19.  JDO, March 1994
;-

Case SYMBOL of
9:	BEGIN	;Circle
		a = findgen(25)*2*!PI/24.
		USERSYM, cos(a), sin(a)
	END
10:	BEGIN	;Filled Circle
		a = findgen(25)*2*!PI/24.
		USERSYM, cos(a), sin(a),/FILL
	END
11:	USERSYM, [0,1,0,-1],[-1,0,1,0],/FILL 	;Filled Diamond
12:	USERSYM, [1,0,-1,1],[-1,1,-1,-1],/FILL	;Filled Triangle
13:	USERSYM, [1,1,-1,-1],[-1,1,1,-1],/Fill	;Filled Square
14: 	USERSYM, 2*[.5,.85,0,1,.15,.5]-1,2*[1,0,.6,.6,0,1]-1 ; 5-point star
15:	USERSYM, 2*[.5,.85,0,1,.15,.5]-1,2*[1,0,.6,.6,0,1]-1,/fill 
							; Filled 5-point star
16:	USERSYM, 2*[.5,1,.85,.15,0,.5]-1,2*[1,.6,0,0,.6,1]-1 ;Pentagon
17:	USERSYM, 2*[.5,1,.85,.15,0,.5]-1,2*[1,.6,0,0,.6,1]-1,/FILL	
							;Filled pent.
18:	USERSYM, 2*[1,.75,.25,0,.25,.75,1]-1,2*[.5,1,1,.5,0,0,.5]-1 ;Hexagon
19:	USERSYM, 2*[1,.75,.25,0,.25,.75,1]-1,2*[.5,1,1,.5,0,0,.5]-1,/fill 
							;Filled Hexagon
else:	BEGIN
		;do nothing
	endELSE
endCASE

end
