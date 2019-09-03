pro half_table $
   , table1 = val1 $
   , reverse1 = reverse1 $
   , table2 = val2 $
   , reverse2 = reverse2 $
   , pivot = pivot

  if n_elements(val1) eq 0 then val1 = 0
  if n_elements(val2) eq 0 then val2 = 33
  if n_elements(pivot) eq 0 then pivot = 0.5

  loadct, val1
  if keyword_set(reverse1) then reversect
  tvlct, r, g, b, /get
  
  newr = r
  newg = g
  newb = b

  pval = round(pivot*255)  

  newr[0:pval-1] = r[round(indgen(pval)*255./(pval-1))]
  newg[0:pval-1] = g[round(indgen(pval)*255./(pval-1))]
  newb[0:pval-1] = b[round(indgen(pval)*255./(pval-1))]

  loadct, val2
  if keyword_set(reverse2) then reversect
  tvlct, r, g, b, /get

  rest = 256 - pval
  newr[(pval):*] = r[round(indgen(rest)*255./(rest-1))] 
  newg[(pval):*] = g[round(indgen(rest)*255./(rest-1))]
  newb[(pval):*] = b[round(indgen(rest)*255./(rest-1))]
  
  tvlct, newr, newg, newb
  
end
