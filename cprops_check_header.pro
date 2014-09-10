function cprops_check_header $
   , in_file = in_file $
   , hdr = hdr $
   , perfect = perfect $
   , comments = comments
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; INITIALIZE OUTPUT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  pass = 1B
  perfect = 1B
  comments = ["cprops_check_header comments:"]

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; READ
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(hdr) eq 0 then begin
     if not file_test(in_file, /read) then begin
        message, in_file+' is not accessible.', /con
        pass = 0B
        perfect = 0B
        return, pass
     endif
     hdr  = headfits(in_file)
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; UNITS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  units = sxpar(hdr, "BUNIT", count=unit_ct)

  if unit_ct eq 0 then begin
     pass = 0B
     perfect = 0B
     comments = [comments, "... MAJOR: no BUNIT keyword"]
  endif

  if strcompress(units,/rem) ne "K" and $
     strcompress(units,/rem) ne "KELVIN" and $
     strcompress(units,/rem) ne "KELVINS" $
  then begin
     pass = 0B
     perfect = 0B
     comments = [comments, "... MAJOR: units not Kelvin"]     
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; BEAM
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  bmaj = sxpar(hdr, "BMAJ", count=bmaj_ct)
  bmin = sxpar(hdr, "BMIN", count=bmin_ct)
  bpa = sxpar(hdr, "BPA", count=bpa_ct)

  if bmaj_ct eq 0 then begin
     pass = 0B
     perfect = 0B
     comments = [comments, "... MAJOR: no BMAJ keyword"]
  endif

  if bmin_ct eq 0 then begin
     pass = 0B
     perfect = 0B
     comments = [comments, "... MAJOR: no BMIN keyword"]
  endif

  if bpa_ct eq 0 then begin
     perfect = 0B
     comments = [comments, "... MINOR: no BPA keyword"]
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; Z-AXIS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  valid_veltypes = $
     ["KM/S", $
      "VELO-LSR", $
      "VELOCITY", $
      "FELO-HEL"]

  ctype3 = sxpar(hdr, "CTYPE3", count=ctype_ct)
  if ctype_ct eq 0 then begin
     pass = 0B
     perfect = 0B
     comments = [comments, "... MAJOR: no CTYPE3 keyword"]
  endif else begin
     if total(strcompress(strupcase(ctype3),/rem) eq valid_veltypes) eq 0 then begin
        comments = [comments, "... MAJOR: CTYPE3 keyword does not match valid vel (km/s)"]     
     endif
  endelse

  channel = abs(sxpar(hdr, "CDELT3", count=cdelt_ct))
  if cdelt_ct eq 0 then begin
     pass = 0B
     perfect = 0B
     comments = [comments, "... MAJOR: no CDELT3 keyword"]
  endif else begin
     if channel gt 1d2 then begin
        pass = 0B
        perfect = 0B
        comments = [comments, "... MAJOR: wide channels, suggest m/s units."]     
     endif
  endelse

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; BONUS STUFF
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
  dist = sxpar(hdr, "DIST", count=dist_ct)
  if dist_ct eq 0 then begin
     perfect = 0B
     comments = [comments, "... MINOR: no DIST keyword"]
  endif

  restfreq = sxpar(hdr, "RESTFRQ", count=restfreq_ct)
  if restfreq_ct eq 0 then begin
     perfect = 0B
     comments = [comments, "... MINOR: no RESTFRQ keyword"]

     if restfreq lt 1e8 then $
        comments = [comments, "... MINOR: RESTFRQ amplitude suggests not Hz"]
  endif

  line = sxpar(hdr, "LINE", count=line_ct)
  if line_ct eq 0 then begin
     perfect = 0B
     comments = [comments, "... MINOR: no LINE keyword"]
  endif

  return, pass

end
