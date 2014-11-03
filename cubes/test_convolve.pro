pro test_convolve

  test = fltarr(201,201)
  test[100,100] = 1.0
  
  for i = 0, 35 do begin    
     conv_with_gauss $
        , data=test $
        , start_beam=[0,0,0] $
        , pix_deg = 1./3600. $,
        , target_beam=[20,10,i*10] $
        , out_data = test_out_temp
     disp, test_out_temp
     wait, 0.1
  endfor

  conv_with_gauss $
     , data=test $
     , start_beam=[0,0,0] $
     , pix_deg = 1./3600. $,
     , target_beam=[20,10,45.] $
     , out_data = test_out_1

  conv_with_gauss $
     , data=test_out_1 $
     , start_beam=[20,10,45] $
     , pix_deg = 1./3600. $,
     , target_beam=[22,22,0] $
     , out_data = test_out_2


  !p.multi=[0,3,1]
  disp, test, /sq
  disp, test_out_1, /sq
  disp, test_out_2, /sq
  !p.multi=0

  stop

end
