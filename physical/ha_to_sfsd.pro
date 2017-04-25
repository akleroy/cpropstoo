function ha_to_sfsd, ha_erg_s_cm2_sr, unit=unit

  @constants.bat

  dist_pc = 10.0d6
  
  sfr_per_erg_s = 5.3d-42

  ha_erg_s_sr = ha_erg_s_cm2_sr * 4. * !pi * (dist_pc * pc)^2

  sr_per_kpc2 = (1d3/dist_pc)^2

  ha_erg_s_kpc2 = ha_erg_s_sr * sr_per_kpc2

  sfsd_msun_yr_kpc2 = ha_erg_s_kpc2 * sfr_per_erg_s

  return, sfsd_msun_yr_kpc2

end
