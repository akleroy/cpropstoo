; Object structure for peak visualization, manipulation, and analysis


; INIT 
; Basic usage : peaks = obj_new('peaks', "../measurements/lmax.txt")
function PEAKS::init, kernfile, idlformat=idlformat
  self.ptr=ptr_new(/allocate)
  self.filters=ptr_new(/allocate)
  self.indices=ptr_new(/allocate)
  if n_elements(kernfile) gt 0 then begin
     if keyword_set(idlformat) then begin
        restore, kernfile
        if n_elements(merger_matrix) gt 0 then begin
           have_merger_matrix = 1B
        endif
     endif else begin
        readcol, kernfile, comment="#" $
                 , format="L,L,L,F,F,F,F" $
                 , kern_xpix, kern_ypix, kern_zpix $
                 , kern_ra, kern_dec, kern_vel, kern_int
        xyv_to_ind, x=kern_xpix, y=kern_ypix, v=kern_zpix $
                    , sz=size(data), ind=kernel_ind
     endelse
     
     *(self.ptr) = [obj_new('peak')]
        self->set_peak, 0 $
                        , kern_xpix[0] $
                        , kern_ypix[0] $
                        , kern_zpix[0] $
                        , kern_ra[0] $
                        , kern_dec[0] $
                        , kern_vel[0] $
                        , kern_int[0]

     for i=1,n_elements(kern_xpix)-1 do begin
        *(self.ptr) = [*(self.ptr), obj_new('peak' $
                                            , x=kern_xpix[i] $
                                            , y=kern_ypix[i] $
                                            , z=kern_zpix[i] $
                                            , ra=kern_ra[i] $
                                            , dec=kern_dec[i] $
                                            , vel=kern_vel[i] $
                                            , int=kern_int[i])]

     endfor
     *(self.indices) = indgen(n_elements(kern_xpix))
     *(self.filters) = [{ind:bytarr(n_elements(kern_xpix))+1B, label:''}]
  endif else begin
     *(self.ptr) = [obj_new('peak')]
     *(self.indices) = [0]
     *(self.filters) = [{ind:[1B], label:''}]
  endelse
  return, 1
end

; XXXXXXXXXXXXXXXXXX
; Modification
; XXXXXXXXXXXXXXXXXX

; This currently doesn't work with filters
pro PEAKS::set_peak, i, x, y, z, ra, dec, vel, int
  (*(self.ptr))[i] $
     ->set_peak, x, y, z, ra, dec, vel, int
end

; Neither does this
pro PEAKS::remove_peak, ind
  if ind eq 0 then $
     (*(self.ptr)) = (*(self.ptr))[1:n_elements(*(self.ptr))-1] $
  else if ind eq n_elements(*(self.ptr))-1 then $
     (*(self.ptr)) = (*(self.ptr))[0:ind-1] $
  else $
     (*(self.ptr)) = [(*(self.ptr))[0:ind-1] $
                      , (*(self.ptr))[ind+1:n_elements(*(self.ptr))-1]]

end

; Neither does this... 
pro PEAK::add_peak, x, y, z, ra, dec, vel, int
  (*(self.ptr)) = [(*(self.ptr)), obj_new('peak' $
                   , x=x $
                   , y=y $
                   , z=z $
                   , ra=ra $
                   , dec=dec $
                   , vel=vel $
                   , int=int)]
end

; XXXXXXXXXXXXXXXXXXX
; Getters
; XXXXXXXXXXXXXXXXXXX
; Convenience functions first, then the workhorse
function PEAKS::getX, id, nofilter=nofilter
  return, self->getVAR(0, id=id, nofilter=nofilter)
end

function PEAKS::getY, id, nofilter=nofilter
  return, self->getVAR(1, id=id, nofilter=nofilter)
end

function PEAKS::getZ, id, nofilter=nofilter
  return, self->getVAR(2, id=id, nofilter=nofilter)
end

function PEAKS::getRA, id, nofilter=nofilter
  return, self->getVAR(3, id=id, nofilter=nofilter)
end

function PEAKS::getDEC, id, nofilter=nofilter
  return, self->getVAR(4, id=id, nofilter=nofilter)
end

function PEAKS::getVEL, id, nofilter=nofilter
  return, self->getVAR(5, id=id, nofilter=nofilter)
end

function PEAKS::getINT, id, nofilter=nofilter
  return, self->getVAR(6, id=id, nofilter=nofilter)
end

;This function gets a variable by variable number: peak.(VAR) 
; e.g.: peak.(0) = peak.x
function PEAKS::getVAR, var, id=id, nofilter=nofilter
  if n_elements(id) then $
     return, (*(self.ptr))[id]->getVAR(var) $ 
  else begin
     if keyword_set(nofilter) then begin
        vararr = [(*(self.ptr))[0]->getVAR(var)]
        for i=1,n_elements(*(self.ptr))-1 do $
           vararr = [vararr, (*(self.ptr))[i]->getVAR(var)]
     endif else begin
        filter = *(self.indices)
        vararr = [(*(self.ptr))[filter[0]]->getVAR(var)]
        for i=1,n_elements(filter)-1 do $
           vararr = [vararr, (*(self.ptr))[filter[i]]->getVAR(var)]        
     endelse
  endelse 
     return, vararr
end

; XXXXXXXXXXXXXXXX
; SORTING FUNCTIONS
; XXXXXXXXXXXXXXXX

pro PEAKS::sortX 
  self->sortVAR, 0
end

pro PEAKS::sortY
  self->sortVAR, 1
end

pro PEAKS::sortZ 
  self->sortVAR, 2
end

pro PEAKS::sortRA
  self->sortVAR, 3
end

pro PEAKS::sortDEC
  self->sortVAR, 4
end

pro PEAKS::sortVEL 
  self->sortVAR, 5
end

pro PEAKS::sortINT
  self->sortVAR, 6
end

pro PEAKS::sortVAR, ind
  vararr = self->getVAR(ind, /nofilter)
  s = sort(vararr)
  (*(self.ptr)) = (*(self.ptr))[s]
  self->sortFilters, s
end

pro PEAKS::sortFilters, s
  for i=0,n_elements(*(self.filters))-1 do $
     (*(self.filters))[i].ind = ((*(self.filters))[i].ind)[s]
  self->updateFilter
end

; XXXXXXXXXXXXXXXXX
; FILTERING TOOLS
; XXXXXXXXXXXXXXXXX

pro PEAKS::updateFilter
  indices = bytarr(n_elements((*(self.filters))[0].ind))+1B
  for i=0,n_elements(*(self.filters))-1 do $
     indices *= (*(self.filters))[i].ind
  *(self.indices) = where(indices, ct)
  if ct lt 1 then $
     *(self.indices) = bytarr(n_elements((*(self.filters))[0].ind))+1B
end

pro PEAKS::listFilters
  for i=0,n_elements(*(self.filters))-1 do $
     print, (*(self.filters))[i].label+" Num = "+str(total((*(self.filters))[i].ind))
end

; behind-the-scenes function that also allows for arbitrary inputs
pro PEAKS::filter, filterArray, label=label
; filterArray should be a binary array, give label if you want
; (e.g. 'x lt 2')
; NOTE: filterArray MUST have the same length as the original number
; of kernels
  if n_elements(label) lt 1 then label = ''
  filter = {ind:filterArray, label:label}
  *(self.filters) = [*(self.filters), filter]
  self->updateFilter

end

;This is just be a convienence function
pro PEAKS::threshold, x=x, y=y, z=z, ra=ra, dec=dec, vel=vel, int=int
  variables = [-1]
  strings = ['']
  variableNames = ['x', 'y', 'z', 'ra', 'dec', 'vel', 'int']
  if n_elements(x) GT 0 then begin 
     variables = [variables, 0]
     strings = [strings, x]
  endif
  if n_elements(y) GT 0 then begin 
     variables = [variables, 1]
     strings = [strings, y]
  endif
  if n_elements(z) GT 0 then begin
     variables = [variables, 2]
     strings = [strings, z]
  endif
  if n_elements(ra) GT 0 then begin 
     variables = [variables, 3]
     strings = [strings, ra]
  endif
  if n_elements(dec) GT 0 then begin
     variables = [variables, 4]
     strings = [strings, dec]
  endif
  if n_elements(vel) GT 0 then begin
     variables = [variables, 5]
     strings = [strings, vel]
  endif
  if n_elements(int) GT 0 then begin
     variables = [variables, 6]
     strings = [strings, int]
  endif
  for j=1,n_elements(variables)-1 do begin
     id = variables[j]
     filters = strsplit(strings[j], '&', /extract)
     for i=0,n_elements(filters)-1 do begin
                                ; get the pieces of the argument 
        filterPieces = strsplit(filters[i], ' ', /extract)
                                ; first piece is the comparison type (lt, gt, etc)
        comparisonType = strlowcase(filterPieces[0])
                                ; second piece is the number
        threshold = float(filterPieces[1]) ; add some sort of error checking
        label = variableNames[id]+" "
        case comparisonType of 
           'gt': begin
              label += filters[i]
              filterArray = self->getVAR(id, /nofilter) gt threshold           
           end
           'lt': begin
              label += filters[i]
              filterArray = self->getVAR(id, /nofilter) lt threshold           
           end
           'ge': begin
              label += filters[i]
              filterArray = self->getVAR(id, /nofilter) ge threshold           
           end
           'le': begin
              label += filters[i]
              filterArray = self->getVAR(id, /nofilter) le threshold           
           end
           'eq': begin
              label += filters[i]
              filterArray = self->getVAR(id, /nofilter) eq threshold           
           end
           'ne': begin
              label += filters[i]
              filterArray = self->getVAR(id, /nofilter) ne threshold           
           end
           
           else: begin
              print, "Invalid comparison type, must be standard (e.g. lt, gt, ne...)"
              return
           end
        endcase
        self->filter, filterArray, label=label
     endfor
  endfor
end

; XXXXXXXXXXXXXXX
; VISUALIZATION
; XXXXXXXXXXXXXXX

pro PEAKS::view_peaks, map, header=header, nofilter=nofilter
  ; settings
  !p.multi = 0
  loadct, 5
  if n_elements(map) GT 0 then begin 
     if n_elements(header) gt 0 then begin
        make_axes, header, raxis=raxis, daxis=daxis, vaxis=vaxis
        disp, peak_map, raxis, daxis, /radec, /sq
        loadct, 0
        oplot, self->getRA(nofilter=nofilter) $
               , self->getDEC(nofilter=nofilter), color=255, ps=1
     endif else begin
        disp, peak_map, /sq
        loadct, 0
        oplot, self->getX(nofilter=nofilter) $
               , self->getY(nofilter=nofilter), color=255, ps=1  
     endelse 
  endif else begin 
     plot, self->getX(nofilter=nofilter), self->getY(nofilter=nofilter), ps = 1
  endelse 
end

; XXXXXXXXXXXXXX
; CLASS DEFINITION
; XXXXXXXXXXXXXX


pro peaks__define
  class={peaks, ptr:ptr_new(), indices:ptr_new(), filters:ptr_new()}

end
