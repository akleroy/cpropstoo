pro extract_dendro $
   , infile=infile $   
   , merger_matrix = merger_matrix $
   , levels = levels $
   , xdend = xdend $
   , ydend = ydend $
   , dendind = dendind $
   , outfile=outfile $
   , verbose=verbose
  
;+
;
; Accepts either the output of level_moments or the corresponding
; properties and uses the merger matrix and kernel list to output the
; info for the dendrogram plot and the corresponding indices to the
; moment array.
;
;-

  compile_opt idl2

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; READ IN THE DATA
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(infile) gt 0 then begin
     restore, infile
;     if file_ct eq 0 then begin
;        message, "Data not found.", /info
;        return
;     endif
  endif

  if n_elements(merger_matrix) eq 0 then begin
     message, "Requires merger matrix", /info
     return
  endif

  if n_elements(levels) eq 0 then begin
     message, "Requires levels", /info
     return
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; GET SET OF INDICES HOLD THE MOST DATA BEFORE A MERGER
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; GET THE SIZE OF THE MERGER MATRIX
  sz = size(merger_matrix) 

  first_point = 1B

; LOOP OVER COLUMNS (LOCAL MAXIMA)
  for i=0, sz[2]-1 do begin 

     if keyword_set(verbose) then begin
        counter, i, sz[2], "Checking mergers for column "
     endif

;    FIND FINITE VALUES > 0 IN EACH COLUMN
     positive = where(merger_matrix[i,*] ge 0, pos_ct)
     
;    SHOULD NOT HAPPEN
     if pos_ct eq 0 then $
        continue

;    SAVE THESE AS A VECTOR
     merger_values = merger_matrix[i, positive]
     merger_values = merger_values[sort(merger_values)]

;    FIND THE UNIQUE ELEMENTS   
     uniq_mergers = merger_values[uniq(merger_values)]

;    FLIP TO GO FROM HIGH TO LOW
     uniq_mergers = reverse(uniq_mergers)
     
;    NOTE NUMBER OF LEVELS
     n_levels = n_elements(levels)

;    LOOP OVER ALL MERGERS EXCEPT THE DIAGONAL/KERNEL VALUE
     for j = 1, n_elements(uniq_mergers)-1 do begin

;       FIND THE LEVELS ABOVE THE MERGER
        ind = where(levels gt uniq_mergers[j], ct)
        if ct eq 0 then begin
;          ... IF THERE AREN'T ANY, RECORD THE MAX LEVEL
           dummy = max(levels, lev_ind)
        endif else begin
;          ... PICK THE LOWEST LEVEL STILL ABOVE THE MERGER
           this_levs = levels[ind]
           dummy = min(this_levs, minind)
           lev_ind = ind[minind]
        endelse

;       SAVE THE INDICES FOR THIS LOCAL MAX AND LEVEL
        if first_point then begin
           xdend = i
           ydend = ind[lev_ind]
           dendind = xdend+long(sz[1])*ydend
           first_point = 0B
        endif else begin
           xdend = [xdend, i]
           ydend = [ydend, ind[lev_ind]]
           dendind = [dendind, long(i)+long(sz[1])*ind[lev_ind]]
        endelse

     endfor

;    FINALLY, ALSO SAVE THE LOWEST VALUE IN THE COLUMN
     dummy = min(levels, minlevind)
     if first_point then begin
        xdend = i
        ydend = minlevind
        dendind = xdend+sz[1]*ydend
        first_point = 0B
     endif else begin
        xdend = [xdend, i]
        ydend = [ydend, minlevind]
        dendind = [dendind, long(i)+long(sz[1])*minlevind]
     endelse     
   
  endfor 

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; REDUCE THESE INDICES TO A SET OF NON-DEGENERATE POINTS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if keyword_set(verbose) then begin
     counter, i, sz[2], "Removing degenerate points "
  endif

; INITIALIZE A FLAG INDICATING THAT WE SHOULD KEEP A POINT
  keep = bytarr(n_elements(xdend)) + 1B

; LOOP OVER ALL LOCAL MAX PAIRS
  for i = 0, sz[1]-1 do begin
     for j = i+1, sz[2]-1 do begin

;       SKIP THE CASE WHERE THERE IS NO MERGER
        if finite(merger_matrix[i,j]) eq 0 then $
           continue

;       IDENTIFY ALL CASES OF MERGERS WITH THE PRESENT LOCAL MAX
        degen_ind = where((xdend eq j) and $
                          (levels[ydend] lt merger_matrix[i,j]) $
                          , degen_ct)

;       FLAG THOSE MERGERS AS BAD
        if degen_ct gt 0 then $
           keep[degen_ind] = 0B
     endfor
  endfor

; KEEP ONLY THE CASES WE HAVE NOT FLAGGED AS BAD
  keep_ind = where(keep)
  xdend = xdend[keep_ind]
  ydend = ydend[keep_ind]
  dendind = dendind[keep_ind]

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; SAVE TO DISK
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(outfile) gt 0 then begin
;    ... WE LOADED A PROPERTIES FILE
     if n_elements(props) gt 0 then begin
        save, file=outfile $
              , props, levels, kernel_ind, hdr, merger_matrix $
              , xdend, ydend, dendind
     endif else if n_elements(moments) gt 0 then begin
;    ... WE LOADED A MOMENTS FILE
        save, file=outfile $
              , moments, levels, kernel_ind, hdr, merger_matrix $
              , xdend, ydend, dendind        
     endif else begin
;    ... WE LOADED SOMETHING ELSE
        save, file=outfile $
              , levels, merger_matrix, xdend, ydend, dendind        
     endelse
  endif

  return

end 
