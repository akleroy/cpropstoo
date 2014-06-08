pro write_text_file, data, field, filestem 
    outfile = filestem+"_"+field+".txt"
    OPENW, lun, outfile, /get_lun
    ; check to see if this is a structure or a single value
    if ISA(data[0], /ARRAY) then begin
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
       ; BUT THESE SINGLE VALUES SHOULD REALLY BE SPLIT OFF!
       printf, lun, str(data)
    endelse

    close, lun
    free_lun, lun
end



pro write_props_text, props,  file=outfile
    names = tag_names(props)
    N_tags = n_elements(names)
    for i = 0, N_tags - 1 do begin
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

end
