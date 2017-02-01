pro cprops_m31_ring

  cube_file = 'm31ring.merge.m3d.cube.fits'

; CLEAN UP UNITS
  prep_cube $
     , in_file='m31ring.merge.m3d.cube.fits' $
     , out_file="m31_ring_correct.fits" $
     , line_name="CO1-0" $
     , restfreq_hz=115.271d9

; CHECK THE HEADER
  print, cprops_check_header(in_file="m31_ring_correct.fits" $
                             , perfect=perfect $
                             , comments=comments)

; PRINT ANY COMMENTS FROM THE HEADER CHECK
  print, comments

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; MAKE A MASK
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; Estimate the noise from the data. We will use this to make a mask
; identifying bright emission. The noise estimate can have a wide
; range of complexities. Here the call uses a moving box of 9x9 pixels
; and calculates a two-d noise map. Optionally, it can fit a spectral
; dependence of the noise or just treat the whole cube as a single
; distribution. The example case here would probably be better served
; with a single value.

  make_noise_cube $
     , cube_file = "m31_ring_correct.fits" $
     , out_file = "m31_ring_noise.fits" $
     , /iterate $
     , /twod_only $
     , /collapse $
     , /show

; Now make a mask of bright emission. Require two channels above
; signal-to-noise five and expand these regions into a surface with at
; least two channels above signal-to-noise two.

  make_cprops_mask $
     , infile = "m31_ring_correct.fits" $
     , outfile="m31_ring_mask.fits" $   
     , rmsfile = "m31_ring_noise.fits" $
     , hi_thresh = 5 $
     , hi_nchan = 2 $
     , lo_thresh = 2 $
     , lo_nchan = 2

; Rerun the masking with the cube "inverted" (flipped to negative) in
; order to check the stringency of the mask. The number of pixels in
; the mask here give an estimate of the flase positive rate.

  make_cprops_mask $
     , infile = "m31_ring_correct.fits" $
     , outfile="m31_ring_mask_neg.fits" $   
     , rmsfile = "m31_ring_noise.fits" $
     , hi_thresh = 5 $
     , hi_nchan = 2 $
     , lo_thresh = 2 $
     , lo_nchan = 2 $
     , /invert

; ... for our case we can load this cube and see that the mask is
; empty. To some approximation we expect no false positives.  

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; IDENTIFY LOCAL MAXIMA IN THE CUBE
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; Identify local maxima in the cube. The program slides a sub-cube
; through the masked region and identifies significant local
; maxima. The results are saved in either a text file or IDL save file
; for further future analysis.
;
; This step allows a good deal of user input in what defines a
; significant local maximum. DELTA defines the contrast required above
; any contour shared with another maximum (this can be defined in
; intensity or SNR units). MINPIX and MINAREA define the area and
; volume required to be uniquely associated with an individual maximum
; for it to be valid. SPECFRIENDS and FRIENDS define the search box in
; pixel units. Think about this as having knobs to turn the contrast
; with the background, the search area, and the region required for a
; maximum to be valid.

  find_local_max $
     , infile = "m31_ring_correct.fits" $
     , inmask="m31_ring_mask.fits" $   
     , text_out="m31_lmax.txt" $
     , /verbose $
     , delta=2 $
     , /snr $
     , minpix=25 $
     , minarea=8 $
     , minvchan=1 $
     , friends=3 $
     , specfriends=1

; Visualize the selected peaks:

  peak_map = max(readfits("m31_ring_correct.fits", hdr),dim=3,/nan)
  loadct, 33
  disp, peak_map, /sq
  readcol, "m31_lmax.txt", comment="#", x, y
  loadct, 0
  oplot, x, y, color=255, ps=1

; Manipulate the FIND_LOCAL_MAX call and iterate until happy.

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; ASSIGN VOXELS IN THE CUBE TO LOCAL MAXIMA
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
; Based on the list of local maxima and the mask, "color" the cube
; with assignments of voxels to discrete objects. The result is a cube
; with pixels corresponding to object number. The current propgram
; includes two methodologies to do this: the "CPROPS" way of assigning
; only emission uniquely associated with a maximum (defined as
; emission above any shared contour with another maximum) and the
; "CLUMPFIND" way of assigning emission within a shared contour to the
; nearest maximum on the basis of proximity.

; CPROPS ASSIGNMENT
  assign_cprops $
     , kernfile="m31_lmax.txt" $
     , infile="m31_ring_correct.fits" $
     , inmask="m31_ring_mask.fits" $
     , outfile="m31_assign_cprops.fits" $
     , /verbose

; CLUMPFIND ASSIGNMENT
  assign_clfind $
     , kernfile="m31_lmax.txt" $
     , infile="m31_ring_correct.fits" $
     , inmask="m31_ring_mask.fits" $
     , outfile="m31_assign_clfind.fits" $
     , /verbose

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; MEASURE CLOUD MOMENTS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
; Using the data cube (note that we now use the primary-beam corrected
; cube) and the assignment cubes generated in the last step measure
; moments for each object in the assignment cube. Here moments is used
; loosely to refer to pixelwise properties (rather than properties
; with physical units, which we compute in the next step). 
;
; Additional analysis code could also be plugged in here to operate on
; the assignment+data combination.

; THE CPROPS CASE
  cube_to_moments $ 
     , infile="m31_ring_correct.fits" $
     , inassign="m31_assign_cprops.fits" $
     , outfile="m31_moments_cprops.idl" $
    , /verbose           

; THE CLUMPFIND CASE
  cube_to_moments $
     , infile="m31_ring_correct.fits" $
     , inassign="m31_assign_clfind.fits" $
     , outfile="m31_moments_clfind.idl" $
     , /verbose           

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; CONVERT MOMENTS TO PROPERTIES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; We now have a structure of moments for each object. The last step
; before science is to convert this into physical units. This step
; accepts moment measurements and uses a header and other assumptions
; (e.g., distance, conversion factor, etc.) to write a "properties"
; IDL structure. We run it once each for the two assignment cubes.

; THE CPROPS CASE
  moments_to_props $
     , hdrfile="m31_ring_correct.fits" $
     , infile="m31_moments_cprops.idl" $
     , idl_file="m31_props_cprops.idl" $
     , dist=780d3 $
     , /verbose

; THE CLUMPFIND CASE
  moments_to_props $
     , hdrfile="m31_ring_correct.fits" $
     , infile="m31_moments_clfind.idl" $
     , idl_file="m31_props_clfind.idl" $
     , dist=780d3 $
     , /verbose

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; DENDROGRAMS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; The code also contains the machinery for an approach based on the
; "dendrograms" formalism. In this case moments are measured for each
; combination of local maximum and contour level and then saved. These
; can be analyzed or visualized using the dendrogram tree diagrams or
; some other approach.

; GET THE MOMENTS BY LEVEL
  cube_to_level_moments $
     , kernfile = "m31_lmax.txt" $
     , infile="m31_ring_correct.fits" $
     , inmask="m31_ring_mask.fits" $
     , outfile="m31_level_moments.idl" $
     , /verbose
  
; GET THE PROPERTIES FOR THOSE MOMENTS  
  moments_to_props $
     , hdrfile="m31_ring_correct.fits" $
     , infile="m31_level_moments.idl" $
     , idl_file="m31_level_props.idl" $
     , dist=780d3 $
     , /verbose

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; AGGREGATE PROPERTIES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
  xctr = 10.6847929  
  yctr = 41.2690650
  incl = 78.
  posang = 37.7

  restore, "m31_props_cprops.idl", /v
  
  deproject, props.xpos, props.ypos $
             , [posang, incl, xctr, yctr] $
             , rgrid=galrad, /vec
  galrad_kpc = galrad*!dtor*780.

  rad = median([[props.RADRMS_GCORR_DECONV] $
                , [props.RADRMS_EXTRAP_DECONV] $
                ,  [props.RADELL_DECONV] $
                ,  [props.RADAREA_DECONV]], dim=2, /even)

  sig = median([[props.VRMS_EXTRAP_DECONV] $
                , [props.VRMS_GCORR_DECONV]] $
               , dim=2, /even)

  lum =  median([[props.LUM_EXTRAP] $
                 , [props.LUM_GCORR]] $
                , dim=2, /even)

  save, file = "m31_ring_agg.idl", rad, sig, lum, galrad_kpc

; ...

  restore, "m31_props_clfind.idl", /v
  
  deproject, props.xpos, props.ypos $
             , [posang, incl, xctr, yctr] $
             , rgrid=galrad, /vec
  galrad_kpc = galrad*!dtor*780.

  rad_clf = median([[props.RADRMS_GCORR_DECONV] $
                , [props.RADRMS_EXTRAP_DECONV] $
                ,  [props.RADELL_DECONV] $
                ,  [props.RADAREA_DECONV]], dim=2, /even)

  sig_clf = median([[props.VRMS_EXTRAP_DECONV] $
                , [props.VRMS_GCORR_DECONV]] $
               , dim=2, /even)

  lum_clf =  median([[props.LUM_EXTRAP] $
                 , [props.LUM_GCORR]] $
                , dim=2, /even)

  save, file = "m31_ring_clfind.idl", rad_clf, sig_clf, lum_clf, galrad_kpc

; ...

  restore, "m31_level_props.idl", /v

  rad_dend = median([[[props.RADRMS_GCORR_DECONV]] $
                , [[props.RADRMS_EXTRAP_DECONV]] $
                , [[props.RADELL_DECONV]] $
                , [[props.RADAREA_DECONV]]], dim=3, /even)

  sig_dend = median([[[props.VRMS_EXTRAP_DECONV]] $
                , [[props.VRMS_GCORR_DECONV]]] $
               , dim=3, /even)

  lum_dend =  median([[[props.LUM_EXTRAP]] $
                 , [[props.LUM_GCORR]]] $
                , dim=3, /even)
  
  deproject, props.xpos, props.ypos $
             , [posang, incl, xctr, yctr] $
             , rgrid=galrad, /vec
  galrad_dend = galrad*!dtor*780.

  extract_dendro $
     , merger_matrix = merger_matrix $
     , levels = levels $
     , xdend = xdend $
     , ydend = ydend $
     , dendind = dendind $
     , /verbose
  
  props_dend = props

  save, file="m31_dendro_agg.idl" $
        , Rad_dend, sig_dend, lum_dend, galrad_dend $
        , props_dend, levels, kernel_ind, hdr, merger_matrix $
        , xdend, ydend, dendind


end
