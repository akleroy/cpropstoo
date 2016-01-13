pro prep_cube $
   , in_file=in_file $
   , out_file=out_file $
   , dist_pc=dist_pc $
   , skip_units=skip_units $
   , skip_beam=skip_beam $
   , bmaj_deg = bmaj_deg $
   , bmin_deg = bmin_deg $
   , bpa_deg = bpa_deg $
   , skip_vel=skip_vel $
   , restfreq_hz=restfreq_hz $
   , line_name=line_name $
   , new_ctype3=new_ctype3 $
   , ms_to_kms=ms_to_kms $
   , skip_blank=skip_blank $
   , skip_check=skip_check

;+
;
; Process a cube into brightness temperature and km/s units and ensure
; header information required for further analysis. Wraps a few common
; first steps for a CPROPS style analysis. Can be used to do a rough
; check of header conformity for further analysis.
; 
; Steps:
;
; 1) Add info to header: distance, rest frequency, jy/beam, beam_fwhm,
; spectral resolution.
;
; 2) Check that the header contains velocity information. Ideally, we
; want the cube to be in LSRK velocity units with channel spacing
; specified in KM/S.
;
; 3) Check that the header contains beam information.
;
; 4) Check that the cube is in units of Kelvin.
;
; 5) Set blank values in the cube to Not-a-Number.
;
; 6) Verify that these steps have been applied.
;
; 7) Write the processed cube to disk.
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; READ IN
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if not file_test(in_file, /read) then begin
     message, in_file+' is not accessible.', /con
     return
  endif

  cube = readfits(in_file, hdr)

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; ADDITIONAL INFORMATION
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
  if n_elements(dist_pc) gt 0 then begin
     sxaddpar, hdr, 'DIST', dist_pc, ' parsec', after='OBJECT'
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; VELOCITY/FREQUENCY UNITS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

;    PUT IN A USER SUPPLIED LINE NAME
  if n_elements(line_name) gt 0 then begin
     sxaddpar, hdr, 'LINE', line_name, after='OBJECT'
  endif

  if keyword_set(skip_vel) eq 0 then begin

     if n_elements(restfreq_hz) eq 0 then begin
;       PULL THE REST FERQUENCY FROM THE HEADER
        restfreq_hz = sxpar(hdr,'RESTFREQ')
        restfreq_hz = restfreq_hz > sxpar(hdr,'FREQ0')
        restfreq_hz = restfreq_hz > sxpar(hdr,'RESTFRQ')

        sxdelpar, hdr, 'FREQ0'
        sxdelpar, hdr, 'RESTFREQ'
        sxaddpar, hdr, 'RESTFRQ', restfreq_hz, ' rest frequency (Hz)', after='OBJECT'

        if restfreq_hz lt 1e8 then begin
;       LESS THAN 1000 MEANS ALMOST CERTAINLY GHZ        
           if restfreq_hz lt 1e3 then $
              restfreq_hz *= 1e9
;       BETWEEN 1e3 and 1e6 MEANS MHZ
           if restfreq_hz lt 1e6 and restfreq_hz gt 1e3 then $
              restfreq_hz *= 1e6
        endif
     endif else begin
        sxdelpar, hdr, 'FREQ0'
        sxdelpar, hdr, 'RESTFREQ'
        sxaddpar, hdr, 'RESTFRQ', restfreq_hz, ' rest frequency (Hz)', after='OBJECT'
     endelse

;    TRY TO ENFORCE KM/S
     cdelt3 = sxpar(hdr, 'CDELT3')
     ctype3 = strupcase(strcompress(sxpar(hdr, 'CTYPE3'),/rem))
     if ctype3 eq "FREQ" then begin
        message,'Original cube in frequency units... attempting conversion.',/con
        c_kms = 2.99792458d5
        center_velocity = (sxpar(hdr,'RESTFRQ')-sxpar(hdr,'CRVAL3'))/sxpar(hdr,'RESTFRQ')*c_kms
        deltav_kms = sxpar(hdr,'CDELT3')/sxpar(hdr,'RESTFRQ')*c_kms
        sxaddpar,hdr,'CRVAL3',center_velocity
        sxaddpar,hdr,'CDELT3',deltav_kms
        sxaddpar,hdr,'CTYPE3','VRAD-F2V'
        sxaddpar,hdr,'CUNIT3','KM/S',after='CTYPE3'
     endif else begin
        if keyword_set(ms_to_kms) or abs(cdelt3) gt 1e2 then begin
           sxaddpar, hdr, 'CDELT3', cdelt3/1e3        
           crval3 = sxpar(hdr, 'CRVAL3')
           sxaddpar, hdr, 'CRVAL3', crval3/1e3        
           sxaddpar, hdr, 'CTYPE3', 'VRAD' ; assume radio velocity
           sxaddpar, hdr, 'CUNIT3', 'KM/S', after='CTYPE3'
                                ; ctype3 = strupcase(strcompress(sxpar(hdr, 'CTYPE3'), /rem))
                                ; if ctype3 eq 'M/S' then begin
                                ;    sxaddpar, hdr, 'CTYPE3', 'KM/S'
                                ; endif
        endif
        if n_elements(new_ctype3) gt 0 then begin
           sxaddpar, hdr, 'CTYPE3', new_ctype3
        endif
     endelse 
  endif
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; BEAM
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if keyword_set(skip_beam) eq 0 then begin
     
;    THE USER SUPPLIES VALUES
     if n_elements(bmaj_deg) ne 0 then begin
        sxaddpar, hdr, "BMAJ", bmaj_deg
        if n_elements(bmin_deg) ne 0 then begin
           sxaddpar, hdr, "BMIN", bmin_deg
        endif else begin
;          ... ASSUME A SYMMETRIC BEAM
           sxaddpar, hdr, "BMIN", bmaj_deg
        endelse
        if n_elements(bpa_deg) ne 0 then begin
           sxaddpar, hdr, "BPA", bpa_deg, after='BMIN'
        endif else begin
;          ... ASSUME A SYMMETRIC BEAM
           sxaddpar, hdr, "BPA", 0.0, after='BMIN'
        endelse
     endif else begin

;       CHECK EXISTENCE IN HEADER
        bmaj_deg = sxpar(hdr, "BMAJ", count=bmaj_ct)
        bmin_deg = sxpar(hdr, "BMIN", count=bmin_ct)
        bpa_deg = sxpar(hdr, "BPA", count=bpa_ct)
        
        if bmaj_ct eq 0 then begin

;          TRAP AIPS HEADER CASE
           ind = where(strpos(hdr, 'CLEAN') gt 0 and $
                       strpos(hdr, 'BMAJ') gt 0 and $
                       strpos(hdr, 'AIPS') gt 0,  ct)
           if ct gt 0 then begin
              str = hdr[max(ind)]
              str = strsplit(str, /ext)
              bmaj_deg = float(str[4])/3600
              bmin_deg = float(str[6])/3600
              bpa_deg = float(str[8])
              
              sxaddpar, hdr, "BMAJ", bmaj_deg
              sxaddpar, hdr, "BMIN", bmin_deg
              sxaddpar, hdr, "BPA", bpa_deg
           endif
;          TRAP SOME FLAVORS OF ALMA HEADERS
           ind = where(strpos(hdr,'restoration:') gt 0 and $
                       strpos(hdr,'(arcsec)') gt 0, ct)
           if ct gt 0 then begin
              str = hdr[max(ind)]
              str = strsplit(str, /ext)
              bmaj_deg = float(str[3])/3600
              bmin_deg = float(str[5])/3600
              bpa_deg = float(str[9])
              sxaddpar, hdr, "BMAJ", bmaj_deg
              sxaddpar, hdr, "BMIN", bmin_deg
              sxaddpar, hdr, "BPA", bpa_deg
           endif

        endif else begin

;          TRAP CASE WHERE ONLY BMAJ IS SPECIFIED
           if bmin_ct eq 0 then $
              sxaddpar, hdr, "BMIN", bmaj_deg, after='BMAJ'
           if bpa_ct eq 0 then $
              sxaddpar, hdr, "BPA", 0.0, after='BMIN'
        endelse

     endelse

  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; INTENSITY UNITS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if keyword_set(skip_units) eq 0 then begin
     units = strupcase(strcompress(sxpar(hdr, "BUNIT"), /rem))
     if units eq "K" then $
        sxaddpar, hdr, "BUNIT", "K"
     if total(units eq ["JY/BEAM","JY/BM"]) then begin
        jtok = calc_jtok(hdr=hdr)
        cube *= jtok
        sxaddpar, hdr, "BUNIT", "K"
     endif
     if units eq "JY" then begin
;       Factor for JY/BEAM
        jtok = calc_jtok(hdr = hdr,pixels_per_beam = ppbeam)
;       Divide by pixels per beam to get JY/PIX
        jtok /= ppbeam
        cube *= jtok
        sxaddpar, hdr, "BUNIT", "K"
     endif
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; 
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CHECK THE CUBE FOR CONFORMITY
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if keyword_set(skip_check) eq 0 then begin

     okay = cprops_check_header(hdr = hdr $
                                , comments=comment $
                                , perfect=perfect)     
     
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; WRITE TO DISK
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  sxaddhist, 'CPROCESS v1.0 prep_cube applied.', hdr
  if sxpar(hdr,'BITPIX') eq -32 then  $
     writefits, out_file, float(cube), hdr else $
        writefits, out_file, cube, hdr

end
