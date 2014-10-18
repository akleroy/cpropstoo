function PEAKS::init, kernfile, idlformat=idlformat
  self.ptr=ptr_new(/allocate)
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
  endif else *(self.ptr) = [obj_new('peak')]
  return, 1
end

function PEAKS::num_peaks
  return, n_elements(*(self.ptr))
end

pro PEAKS::set_peak, i, x, y, z, ra, dec, vel, int
  (*(self.ptr))[i] $
     ->set_peak, x, y, z, ra, dec, vel, int
end

pro PEAKS::remove_peak, ind
  if ind eq 0 then $
     (*(self.ptr)) = (*(self.ptr))[1:self->num_peaks-1] $
  else if ind eq self->num_peaks-1 then $
     (*(self.ptr)) = (*(self.ptr))[0:ind-1] $
  else $
     (*(self.ptr)) = [(*(self.ptr))[0:ind-1] $
                      , (*(self.ptr))[ind+1:self->num_peaks-1]]

end

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

function PEAKS::getX, id
  if n_elements(id) then $
     return, (*(self.ptr))[id]->getX() $
  else begin
     xarr = [(*(self.ptr))[0]->getX()]
     for i=1,n_elements(*(self.ptr))-1 do $
        xarr = [xarr, (*(self.ptr))[i]->getX()]
  endelse 
     return, xarr
end

function PEAKS::getY, id
  if n_elements(id) then $
     return, (*(self.ptr))[id]->getY() $ 
  else begin
     yarr = [(*(self.ptr))[0]->getY()]
     for i=1,n_elements(*(self.ptr))-1 do $
        yarr = [yarr, (*(self.ptr))[i]->getY()]
  endelse 
     return, yarr
end

function PEAKS::getZ, id
  if n_elements(id) then $
     return, (*(self.ptr))[id]->getZ() $ 
  else begin
     zarr = [(*(self.ptr))[0]->getZ()]
     for i=1,n_elements(*(self.ptr))-1 do $
        zarr = [zarr, (*(self.ptr))[i]->getZ()]
  endelse 
     return, zarr
end

function PEAKS::getRA, id
  if n_elements(id) then $
     return, (*(self.ptr))[id]->getRA() $ 
  else begin
     raarr = [(*(self.ptr))[0]->getRA()]
     for i=1,n_elements(*(self.ptr))-1 do $
        raarr = [raarr, (*(self.ptr))[i]->getRA()]
  endelse 
     return, raarr
end

function PEAKS::getDEC, id
  if n_elements(id) then $
     return, (*(self.ptr))[id]->getDEC() $ 
  else begin
     decarr = [(*(self.ptr))[0]->getDEC()]
     for i=1,n_elements(*(self.ptr))-1 do $
        decarr = [decarr, (*(self.ptr))[i]->getDEC()]
  endelse 
     return, decarr
end

function PEAKS::getVEL, id
  if n_elements(id) then $
     return, (*(self.ptr))[id]->getVEL() $ 
  else begin
     velarr = [(*(self.ptr))[0]->getVEL()]
     for i=1,n_elements(*(self.ptr))-1 do $
        velarr = [velarr, (*(self.ptr))[i]->getVEL()]
  endelse 
     return, velarr
end

function PEAKS::getINT, id
  if n_elements(id) then $
     return, (*(self.ptr))[id]->getINT() $ 
  else begin
     intarr = [(*(self.ptr))[0]->getINT()]
     for i=1,n_elements(*(self.ptr))-1 do $
        intarr = [intarr, (*(self.ptr))[i]->getINT()]
  endelse 
     return, intarr
end

pro PEAKS::sortX 
  xarr = self->getX()
  s = sort(xarr)
  (*(self.ptr)) = (*(self.ptr))[s]
end

pro PEAKS::sortY
  yarr = self->getY()
  s = sort(yarr)
  (*(self.ptr)) = (*(self.ptr))[s]
end

pro PEAKS::sortZ 
  zarr = self->getZ()
  s = sort(zarr)
  (*(self.ptr)) = (*(self.ptr))[s]
end

pro PEAKS::sortRA
  raarr = self->getRA()
  s = sort(raarr)
  (*(self.ptr)) = (*(self.ptr))[s]
end

pro PEAKS::sortDEC
  decarr = self->getDEC()
  s = sort(decarr)
  (*(self.ptr)) = (*(self.ptr))[s]
end

pro PEAKS::sortVEL 
  velarr = self->getVEL()
  s = sort(velarr)
  (*(self.ptr)) = (*(self.ptr))[s]
end

pro PEAKS::sortINT
  intarr = self->getINT()
  s = sort(intarr)
  (*(self.ptr)) = (*(self.ptr))[s]
end

pro peaks__define
  class={peaks, ptr:ptr_new()}

end
