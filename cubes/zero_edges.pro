pro zero_edges $
   , data $
   , thick=thick
;+
;
; Zero the edges of spectrum, image, or data cube.
;
;-

  if n_elements(thick) eq 0 then $
     thick=1

  sz = size(data)

  if (sz[0]) eq 1 then begin
     data[0:(thick-1)] = 0
     data[(sz[1]-thick):sz[1]-1] = 0
  endif

  if (sz[0]) eq 2 then begin
     data[0:(thick-1),*] = 0
     data[*,0:(thick-1)] = 0
     data[(sz[1]-thick):(sz[1]-1),*] = 0
     data[*,(sz[2]-thick):(sz[2]-1)] = 0
  endif

  if (sz[0]) eq 3 then begin    
     data[0:(thick-1),*,*] = 0
     data[(sz[1]-thick):(sz[1]-1),*,*] = 0
     data[*,0:(thick-1),*] = 0
     data[*,(sz[2]-thick):(sz[2]-1),*] = 0
     data[*,*,0:(thick-1)] = 0
     data[*,*,(sz[3]-thick):(sz[3]-1)] = 0
  endif

end
