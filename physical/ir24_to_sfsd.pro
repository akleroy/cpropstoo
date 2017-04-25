function ir24_to_sfsd, ir_mjy_sr, unit=unit

  @constants.bat

  restfreq24_hz = 1.26582d13

  dist_pc = 10.0d6
  
  fac = 0.031

  sfr_per_erg_s = fac*5.3d-42

  ir_erg_s_cm2_sr = ir_mjy_sr * restfreq24_hz * 1d6 * 1d-23

  ir_erg_s_sr = ir_erg_s_cm2_sr * 4.0 * !pi * (dist_pc * pc)^2

  sr_per_kpc2 = (1d3/dist_pc)^2

  ir_erg_s_kpc2 = ir_erg_s_sr * sr_per_kpc2

  sfsd_msun_yr_kpc2 = ir_erg_s_kpc2 * sfr_per_erg_s

  return, sfsd_msun_yr_kpc2

end
