;+
; NAME
; hans(nhan,spec)
;
; DESCRIPTION:
; Hanning smooths (hanning 1, 3, 5 or 7) a 1-D spectrum

function hans, nhan, $	; hanning channels: 1,3,5,7
               spec, $	; input spectrum
               nan=nan  ; pass the nan keyword
case nhan of
1 : hansmo=[1.]
3 : hansmo=[0.5,1.,0.5]/2.
5 : hansmo=[0.25,0.75,1.,0.75,0.25]/3.
7 : hansmo=[0.146,0.5,0.854,1.,0.854,0.5,0.146]/4.
else : Message, 'nhan can only be 1, 3, 5, 7'
endcase

return, convol(spec,hansmo,/edge_truncate,nan=nan)

end
