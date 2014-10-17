function alphabetize_struct, struct_in

;+
;
; NAME
;
;   alphabetize_struct
;
; SYNTAX
;
;   new_struct = alphabetize_struct(struct_in)
;
; OUTPUT
;
; A new structure with the fields arranged into alphabetical order.
;
; NOTE
; 
; This is an infrastructure program and not documented for general
; use. Though it's really simple and potentially general.
;
;-  

; GET THE TAG NAMES
  tags = tag_names(struct_in)
  n_tags = n_elements(tags)

; GET THE SORTED ORDER OF THE TAGS
  order_tags = sort(tags)

; INITIALIZE A NEW STRUCTURE
  new_struct = create_struct(tags[order_tags[0]] $
                             , struct_in.(order_tags[0]))  

; LOOP OVER FIELDS AND EXPAND THE STRUCTURE
  for i = 1, n_tags-1 do begin

     new_struct = create_struct(new_struct $
                                , tags[order_tags[i]] $
                                , struct_in.(order_tags[i]))  
  endfor

; RETURN
  return, new_struct

end
