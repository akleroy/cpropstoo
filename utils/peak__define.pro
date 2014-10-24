; Object structure for a peak, currently just stores values for the
; peaks objects. Is there functionality we should add here?

; INIT - set to 0 by default
function peak::init, x=x, y=y, z=z, ra=ra, dec=dec, vel=vel, int=int
  if n_elements(x) eq 0 then $
     self.x = 0.0 else self.x = x
  if n_elements(y) eq 0 then $
     self.y = 0.0 else self.y = y
  if n_elements(z) eq 0 then $
     self.z = 0.0 else self.z = z
  if n_elements(ra) eq 0 then $
     self.ra = 0.0 else self.ra = ra
  if n_elements(dec) eq 0 then $
     self.dec = 0.0 else self.dec = dec
  if n_elements(vel) eq 0 then $
     self.vel = 0.0 else self.vel = vel
  if n_elements(int) eq 0 then $
     self.int = 0.0 else self.int = int
  return, 1
end

; Setter
pro PEAK::set_peak, x, y, z, ra, dec, vel, int
  self.x = x
  self.y = y
  self.z = z
  self.ra = ra
  self.dec = dec
  self.vel = vel
  self.int = int
  
end

; Get variable by number reference
function PEAK::getVAR, id
  return, self.(id)
end

; Get variables by name:

function PEAK::getX
  return, self.x
end

function PEAK::getY
  return, self.y
end

function PEAK::getV
  return, self.z
end

function PEAK::getRA
  return, self.ra
end

function PEAK::getDEC
  return, self.dec
end

function PEAK::getVEL
  return, self.vel
end

function PEAK::getINT
  return, self.int
end

; Object definition

pro peak__define
  class={peak, x:0.0, y:0.0, z:0.0, ra:0.0, dec:0.0, vel:0.0, int:0.0}

end
