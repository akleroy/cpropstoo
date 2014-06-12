pro write_text_file, data, field, filestem 
    outfile = filestem+"_"+field+".txt"
    OPENW, lun, outfile, /get_lun
    ; check to see if this is a structure (type 8)
    ; or a single value
    if size(data[0], /TYPE) EQ 8 then begin
       names = tag_names(data)
       len = n_elements(data)
       N_tags = n_elements(names)
       head = strjoin(str(names), ' ')
       ; put the structure names up top
       printf, lun, head
       for i = 0, len-1 do begin  
          row = fltarr(N_tags)
          for j = 1, N_tags-1 do $
             row[j] = data[i].(j)
          
          ; we now have the values from each tag
          ; as an array
          row = str(row)
          row[0] = data[i].(0)
          row = strjoin(row, ' ')
          ; add in the units ".(0)" 
          ; and print to file
          printf, lun, row
       endfor
    endif else begin
       ; if we have a single value we have an easy job
       ; BUT THESE SINGLE VALUES SHOULD ALREADY 
       ; BE WRITTEN IN THE HEADER
       printf, lun, str(data)
    endelse

    close, lun
    free_lun, lun
 end     ; OF WRITE_TEXT_FILE

function write_text_header, data, filestem 
  ; SHOULD THIS BE HARDCODED? 
    names = tag_names(data)
    head_tags = [ 'ID' $              
                  , 'TAG' $
                  , 'GAL' $
                  , 'LINE' $            
                  , 'FILENAME' $
                  , 'DIST_PC' $ 
                  , 'BEAMFWHM_PIX' $
                  , 'BEAMFWHM_DEG' $
                  , 'BEAMFWHM_PC' $
                  , 'DEGPERPIX' $
                  , 'PCPERPIX' $
                  , 'PIXPERBEAM' $
                  , 'SRPERBEAM' $
                  , 'PC2PERBEAM' $
                  , 'CHANWIDTH_KMS' $
                  , 'CHAN_TO_SIG' $
                  , 'RMS_TO_RAD' $
                  , 'XCO' $
                  , 'VIRCOEFF']

    fle_open = 0
    for i=0, n_elements(head_tags)-1 do begin
       ; locate header tags in our tags
       w = where(head_tags[i] eq names, ct)
       if ct GT 0 then begin 
          if not fle_open then begin ; check if the file is open
             outfile = filestem+"_Header.txt"
             OPENW, lun, outfile, /get_lun
             fle_open = 1
          endif   
          ; if we have the tags then write them out
          printf, lun, names[w]+" "+str(data.(w))
          if n_elements(indices) LT 1 then $
             indices = [w] else $
                indices = [indices, w]
       endif   
    endfor


    if fle_open then begin
       close, lun
       free_lun, lun
    endif

    return,  indices
 end                 ; OF WRITE_TEXT_HEADER 


pro write_props_text, props,  file=outfile
  ; First check to see if the directory they requested is already made
  dir =   strmid(string, 0, strpos(string, "/", /reverse_search))
  ; This will find everything before the last backslash
  if not file_test(dir) $ ; if it is not
    then  file_mkdir, dir ; then make it
    
    names = tag_names(props)
    N_tags = n_elements(names)
    head_ind = write_text_header(props, outfile)
    for i = 0, N_tags - 1 do begin 
       ; check if this was already written in the header file
       if total(i EQ head_ind) GT 0 then $
          continue
       ; Check to see if this is the moments structure
       ; if it is then we can just call this function recursively
       if names[i] eq 'MOMENTS' then $
          write_props_text $
          , (props.(i))[*] $
          , file=outfile+"_MOMENTS" $
       else $
          write_text_file, (props.(i))[*], names[i], outfile
       ; if not we can just call the actual writer
    endfor

 end   ; OF WRITE_PROPS_TEXT
