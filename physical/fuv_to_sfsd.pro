function fuv_to_sfsd, fuv_mjy_sr, unit=unit

  @constants.bat

  restfreqfuv = 1.93548d15

  dist_pc = 10.0d6
  
  sfr_per_erg_s_hz = 0.68d-28

  fuv_erg_s_cm2_hz_sr = fuv_mjy_sr * 1d6 * 1d-23

  fuv_erg_s_hz_sr = fuv_erg_s_cm2_hz_sr * 4.0 * !pi * (dist_pc * pc)^2

  sr_per_kpc2 = (1d3/dist_pc)^2

  fuv_erg_s_hz_kpc2 = fuv_erg_s_hz_sr * sr_per_kpc2
  
  sfsd_msun_yr_kpc2 = fuv_erg_s_hz_kpc2 * sfr_per_erg_s_hz

  return, sfsd_msun_yr_kpc2

end
