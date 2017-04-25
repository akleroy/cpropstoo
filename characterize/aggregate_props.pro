pro aggregate_props $
   , infile=infile $
   , outfile=outfile $
   , dendro=dendro $
   , classic=classic

;
; Hack to collapse the large properties structure to a smaller set of
; useful values. Will turn into a full program later.
;  

  nan = !values.f_nan
  empty_meas = $
     { $
     ra: nan, $
     dec: nan, $     
     rad: nan, $
     e_rad: nan, $
     sig: nan, $
     e_sig: nan, $
     lum: nan, $
     e_lum: nan, $
     tpeak: nan $
     }

  restore, infile

  meas = replicate(empty_meas, n_elements(props))

  if keyword_set(dendro) then begin
     sz = size(props)
     meas = reform(meas, sz[1], sz[2])
     
     meas.ra = props.xpos
     meas.dec = props.ypos
     if keyword_set(classic) then begin
        meas.rad = props.RADRMS_GCORR_DECONV
     endif else begin
        meas.rad = $
           median([[[props.RADRMS_GCORR_DECONV]] $
                ,  [[props.RADELL_DECONV]] $
                   ,  [[props.RADAREA_DECONV]]], dim=3, /even)
     endelse
     meas.sig = props.VRMS_GCORR_DECONV
     meas.lum = props.LUM_GCORR
     meas.tpeak = props.maxval

     extract_dendro $
        , merger_matrix = merger_matrix $
        , levels = levels $
        , xdend = xdend $
        , ydend = ydend $
        , dendind = dendind $
        , /verbose     

     save, file=outfile $
           , meas $
           , levels, kernel_ind, hdr, merger_matrix $
           , xdend, ydend, dendind

  endif else begin
     
     meas.ra = props.xpos
     meas.dec = props.ypos
     if keyword_set(classic) then begin
        meas.rad = props.RADRMS_GCORR_DECONV
     endif else begin
        meas.rad = $
           median([[props.RADRMS_GCORR_DECONV] $
                   ,  [props.RADELL_DECONV] $
                   ,  [props.RADAREA_DECONV]], dim=2, /even)
     endelse
     meas.sig = props.VRMS_GCORR_DECONV
     meas.lum = props.LUM_GCORR
     meas.tpeak = props.maxval       
     
     save, file=outfile, meas

  endelse

end
