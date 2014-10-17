 help,/str,props
   ;; ALPHA           FLOAT           4.35000  ## CO-to-H2 conversion factor. Can be changed using keyword alpha in routine moments_to_props. Default is Galactic value.
   ;; AREA            FLOAT           400.000  ## Area of region in AREA_UNIT 
   ;; AREAMAJ_PHYS    FLOAT           23.2393  ## Physical area of full region in units of pc [xxx what does the MAJ mean? why didn't here a MIN like the rest of the areas?]
   ;; AREAMAJ_PHYS_HALFMAX                     ## Physical area of region down to half-maximum intensity in units of pc [xxx again, what does MAJ mean?]
   ;;                 FLOAT           13.2485
   ;; AREAMAJ_PHYS_HALFMAX_DECONV              ## Physical area of region out to half-maximum with beam deconvolved in units of pc.
   ;;                 FLOAT           7.95531
   ;; AREA_HALFMAX    FLOAT           130.000  ## Area of region out to half-maximum in AREA_UNIT.
   ;; AREA_UNIT       STRING    'pix^2'        ## Unit for AREA.
   ;; BEAMFWHM_DEG    FLOAT       0.000142887  ## Full-width half max of beam in degrees.
   ;; BEAMFWHM_PC     FLOAT           24.9385  ## Full-width half max of beam in pc.
   ;; BEAMFWHM_PIX    FLOAT           10.2879  ## Full-width half max of beam in pixels.
   ;; BMAJ_DEG        FLOAT       0.000150000  ## Major axis of beam in degrees.
   ;; BMIN_DEG        FLOAT       0.000136111  ## Minor axis of beam in degrees
   ;; BPA_DEG         FLOAT           28.0000  ## Position angle of beam in degrees.
   ;; CHANTOSIG       FLOAT          0.398942  ## conversion from channel width to a Gaussian width (=1/sqrt(2pi))
   ;; CHANWIDTH_KMS   FLOAT           2.00000  ## Channel width in km/s
   ;; CLIPPED         BYTE         0           ## xxx
   ;; CLIPVAL         FLOAT               NaN  ## xxx
   ;; CLIPVAL_UNIT    STRING    'K'            ## xxx
   ;; COVAR_XY        FLOAT               NaN  ## xxx
   ;; DEGPERPIX       FLOAT       1.38889e-05  ## Degrees per pixel
   ;; DELTAV          FLOAT           15.0000  ## Velocity width. Unit given by deltav_unit. [xxx in the paper, deltav is the FWHM line width, so how is this parameter different from DELTAV_HALFMAX]
   ;; DELTAV_HALFMAX  FLOAT           5.00000  ## Velocity width at half max intensity. Unit given by DELTAV_UNIT. [xxx or is this at half intensity??]
   ;; DELTAV_PHYS     FLOAT           30.0000  ## Velocity width in km/s.
   ;; DELTAV_PHYS_HALFMAX                      ## Velocity width at half max intensity in km/s.
   ;;                 FLOAT           10.0000  
   ;; DELTAV_PHYS_HALFMAX_DECONV               ## Velocity width at half max intensity  [xxx with channel size deconvolved or beam deconvolved] in km/s.
   ;;                 FLOAT           9.96812
   ;; DELTAV_UNIT     STRING    'pix'          ## Unit for DELTAV values.
   ;; DIST_PC         FLOAT       1.00000e+07  ## Distance to galaxy in parsecs. Set in prep_cube routine.
   ;; ELLFITMAJ       FLOAT           6.13820  ## Major axis of ellipse fitted to the entire region in ELLFITMAJ_UNIT. Note that this measurement is NOT intensity weighted; it reflects the overall shape of the region.
   ;; ELLFITMAJ_HALFMAX                        ## Major axis size of ellipse fitted to region defined by the half maximum of emission in units of ELLFITMAJ_UNIT.
   ;;                 FLOAT           3.75092
   ;; ELLFITMAJ_UNIT  STRING    'pix'          ## Unit for ELLIFITMAJ_* values.
   ;; ELLFITMIN       FLOAT           5.80426  ## Minor axis of ellipse fitted to region shape in ELLFITMIN_UNIT. Note that this measurement is NOT intensity weighted; it reflect the overall shape of the region.
   ;; ELLFITMIN_HALFMAX                        ## Minor axis size of ellipse fitted to region defined by the half maximum of emission in units of ELLFITMIN_UNIT.
   ;;                 FLOAT           2.84482  
   ;; ELLFITMIN_UNIT  STRING    'pix'          ## Unit for ELLFITMIN_* values.
   ;; ELLFITPOSANG    FLOAT           2.39995  ## Position angle for ellipse fitting in units of ELLFITPOSANG_UNIT. Major and minor axes given by ELLFITMAJ and ELLFITMIN.
   ;; ELLFITPOSANG_HALFMAX                     ## Position angle for ellipse fitting down to half-maximum of emission in units of ELLFITPOSANG_UNIT. Major and minor axes given by ELLFITMAJOR_HALFMAX and ELLFITMIN_HALFMAX
   ;;                 FLOAT           1.61223
   ;; ELLFITPOSANG_UNIT                         ## Unit for ELLFITPOSANG_*.
   ;;                 STRING    'rad'
   ;; ELLMAJ          FLOAT           25.2260   ## ELLFITMAJ in phyxical units [xxx Might be worth rehnaming to ELLMAJ_PHYS if so, although I shudder at changing more variable names.]
   ;; ELLMAJ_HALFMAX  FLOAT           15.4151   ## ELLFITMAJ_HALFMAX in physical units (ELLMAJ_UNIT)
   ;; ELLMAJ_HALFMAX_DECONV                     ## ELLFITMAJ_HALFMAX with beam deconvolved in physical units (ELLMAJ_UNIT)
   ;;                 FLOAT           11.5033   ## xxx
   ;; ELLMAJ_UNIT     STRING    'pc'            ## Unit for ELLMAJ*
   ;; ELLMIN          FLOAT           23.8536   ## ELLMIN in physical units (ELLMIN_UNIT).
   ;; ELLMIN_HALFMAX  FLOAT           11.6913   ## ELLMIN_HALFMAX in physical units (ELLMIN_UNIT). [xxx hmm...doesn't seem to be a corresponding ELLMIN_UNIT.]                                   
  ;;  ELL_TO_SIG_HALF FLOAT           1.69536   ## xxx
  ;;  FILENAME        STRING    ''              # xxx
  ;;  FLUX            FLOAT           9.87023   # line flux in FLUX_UNIT
  ;;  FLUX_EXTRAP     FLOAT           15.3060   # extrapolated line flux for region in FLUX_UNIT
  ;;  FLUX_GCORR      FLOAT           13.4698   # line flux for Gaussian region in FLUX_UNIT
  ;;  FLUX_UNIT       STRING    'K*km/s*as^2'   # Unit for fluxes
  ;;  GAL             STRING    ''              # Galaxy name [xxx I assume, but thought I should double-check. Where is this set? It's not in prep_cube.]
  ;;  GCORR_1D        FLOAT           1.29968   # xxx
  ;;  GCORR_FLUX      FLOAT           1.36469   # xxx
  ;;  LINE            STRING    'CO'            # line name
  ;;  LUM             FLOAT           23199.4   # measured line luminosity in LUM_UNIT. 
  ;;  LUM_EXTRAP      FLOAT           35975.9   # Line luminosity extrapolated using curve of growth method  in LUM_UNIT.
  ;;  LUM_GCORR       FLOAT           31660.0   # Line lumionsity calculated assuming Gaussian emission profile in LUM_UNIT.
  ;;  LUM_UNIT        STRING    'K*km/s*pc^2'   # Unit for luminosities (LUM*)
  ;;  MAJRMS          FLOAT           12.5996   # RMS velocity along major axis. Unit is MAJRMS_UNIT. 
  ;;  MAJRMS_EXTRAP   FLOAT           16.4793   # RMS size along major axis from curve of growth extrapolation. Unit is MAJRMS_UNIT.
  ;;  MAJRMS_EXTRAP_DECONV                      # RMS size along major axis from curve of growht extrapolation with beam deconvolved. Unit is MAJRMS_UNIT.
  ;;                  FLOAT           12.7459   
  ;;  MAJRMS_GCORR    FLOAT           16.3755   # RMS size along major axis from Gaussian fit. Unit is MAJRMS_UNIT.
  ;;  MAJRMS_GCORR_DECONV                       # RMS size along major axis from Gaussian fit with beam deconvolved. Unit is MAJRMS_UNIT.
  ;;                  FLOAT           12.6567
  ;;  MAJRMS_UNIT     STRING    'pc'          # Unit for RMS size along major axis (MAJRMS_UNIT).
  ;;  MASS            FLOAT           100917.   # Mass of clump in unit MASS_UNIT. Calculated by multiplying LUM by ALPHA. 
  ;;  MASS_EXTRAP     FLOAT           156495.   # Extrapolated mass of clump in unit MASS_UNIT. Calculated by multiplying LUM_EXTRAP by ALPHA. 
  ;;  MASS_GCORR      FLOAT           137721.   # Guassian fit mass of clump in unit MASS_UNIT. Calculated by multiplying LUM_GCORR by ALPHA.
  ;;  MASS_UNIT       STRING    'Msun'          # Unit for masses.
  ;;  MAXVAL          FLOAT           2.79141   # Maximum value for clump in unit MAXVAL_UNIT.
  ;;  MAXVAL_UNIT     STRING    'K'             # Unit for MAXVAL.
  ;;  MINRMS          FLOAT           11.6019   # RMS size along minor axis. Unit is MINRMS_UNIT.
  ;;  MINRMS_EXTRAP   FLOAT           13.5740   # RMS size along minor axis from curve of growth extrapolation. Unit is MINRMS_UNIT.
  ;;  MINRMS_EXTRAP_DECONV                      # RMS size along minro axis from curve of growth extrapolation with beam deconvolved. Unit is MINRMS_UNIT.
  ;;                  FLOAT           8.23611
  ;;  MINRMS_GCORR    FLOAT           15.0788   # RMS size along minor axis from Gaussian fit. Unit is MINRMS_UNIT.
  ;;  MINRMS_GCORR_DECONV                       # RMS size along minor axis from Gaussian fit with beam deconvolved. Unit is MINRMS_UNIT.
  ;;                  FLOAT           10.4787
  ;;  MINRMS_UNIT     STRING    'pc'          # Unit for MINRMS* values.
  ;;  MINVAL          FLOAT          0.411439   # Minimum value in region in MINVAL_UNIT. [xxx what does this really mean? Is this just the lowest contour it goes down to.]
  ;;  MINVAL_UNIT     STRING    'K'             # Unit for minimum value (MINVAL).
  ;;  MOM0            FLOAT           1974.05   # Integrated flux in image units (MOM0_UNIT).
  ;;  MOM0_EXTRAP     FLOAT           3061.20   # Integrated flux extrapolated along the curve of growth in MOM0_UNIT.
  ;;  MOM0_GCORR      FLOAT           2693.96   # Integrated flux from Gaussian fit in MOM0_UNIT
  ;;  MOM0_UNIT       STRING    'K*pix^3'       # Integrated flux units (in image units).
  ;;  MOM1V           FLOAT           48.3746   # Moment one of the velocity axis, i.e., the mean velocity of the pixels in the clump, in units of MOM1V_UNIT.
  ;;  MOM1V_UNIT      STRING    'pix'           # Unit for MOM1V.
  ;;  MOM1X           FLOAT           241.883   # Moment one of the X axis, i.e., the mean position of the pixels in the clump in the X direction, in units of MOM1X_UNIT. 
  ;;  MOM1X_UNIT      STRING    'pix'           # Unit for MOM1X
  ;;  MOM1Y           FLOAT           255.340   # Moment one of the Y axis, i.e., the mean position of the pixels in the clump in the Y direction, in units of MOM1Y_UNIT.
  ;;  MOM1Y_UNIT      STRING    'pix'           # Unit for MOM1Y
  ;;  MOM2MAJ         FLOAT           5.19771   # Moment two  along major axis of clump (i.e., RMS size [xxx is this a correct characterization of this parameter?]) in units of MOM2MAJ_UNIT.
  ;;  MOM2MAJ_EXTRAP  FLOAT           6.79819   # Moment two  along major axis of clump (i.e., RMS size) from curve of growth extrapolation in units of MOM2MAJ_UNIT.
  ;;  MOM2MAJ_GCORR   FLOAT           6.75536
  ;;  MOM2MAJ_UNIT    STRING    'pix'           # Moment two  along major axis (i.e., RMS size) from Gaussian fit in units of MOM2MAJ_UNIT.
  ;;  MOM2MIN         FLOAT           4.78614   # Moment two  along minor axis (i.e., RMS size)  in units of MOM2MIN_UNIT.
   ;; MOM2MIN_EXTRAP  FLOAT           5.59968   # Moment two  along minor axis (i.e., RMS size) from curve of growth extrapolation in units of MOM2MIN_UNIT.
   ;; MOM2MIN_GCORR   FLOAT           6.22045   # Moment two  along minor axis (i.e., RMS size) from Gaussian fit in units of MOM2MIN_UNIT.
   ;; MOM2MIN_UNIT    STRING    'pix'           # Unit for MOM2MIN*
   ;; MOM2V           FLOAT           2.57977   # Moment two along velocity axis (i.e., RMS velocity)
   ;; MOM2V_EXTRAP    FLOAT           3.23670   # Moment two along velocity axis (i.e., RMS velocity) from curve of growth extrapolation in units of MOM2V_UNIT.
   ;; MOM2V_GCORR     FLOAT           3.35288   # Moment two along velocity axis (i.e., RMS velocity) from Gaussian fit in units of MOM2V_UNIT.
   ;; MOM2V_UNIT      STRING    'pix'           # Unit for MOM2V*
   ;; MOM2X           FLOAT           5.13468   # Moment two along x axis (i.e., RMS size) in units of MOM2X_UNIT.
   ;; MOM2X_EXTRAP    FLOAT           6.72150   # Moment two along x axis (i.e., RMS size) from curve of growth extrapolation in units of MOM2X_UNIT.
   ;; MOM2X_GCORR     FLOAT           6.67344   # Moment two along x axis (i.e., RMS size) from Gaussian fit in units of MOM2X_UNIT.
   ;; MOM2X_UNIT      STRING    'pix'           # Unit for MOM2X*
   ;; MOM2Y           FLOAT           4.85370   # Moment two along y axis (i.e., RMS size) in units of MOM2Y_UNIT.
   ;; MOM2Y_EXTRAP    FLOAT           5.69351   # Moment two along y axis (i.e., RMS size) from curve of growth extrapolation in units of MOM2Y_UNIT.
   ;; MOM2Y_GCORR     FLOAT           6.30826   # Moment two along y axis (i.e., RMS size) from Gaussian fit in units of MOM2Y_UNIT.
   ;; MOM2Y_UNIT      STRING    'pix'           # Unit for MOM2Y*.
   ;; MOMPOSANG       FLOAT           2.73213   # Position angle for major and minor axis of clump: MOM2MAJ and MOM2MIN. [xxx check this]
   ;; MOMPOSANG_UNIT  STRING    'rad'           # Unit for MOMPOSANG.
   ;; NOISE           FLOAT               NaN   # xxx
   ;; NOISE_UNIT      STRING    'K'             # Unit for NOISE.
   ;; NPIX            LONG              2116    # Number of pixels in region.
   ;; PC2PERBEAM      FLOAT           704.699   # area in parcsec^2 per beam.
   ;; PCPERPIX        FLOAT           2.42407   # size of pixel in parcsecs.
   ;; PEAKNUM         LONG                 1    # label for peak.
   ;; PEAK_TO_EDGE    FLOAT           6.78451   # xxx
   ;; PIXPERBEAM      FLOAT           119.926   # Number of pixels per beam.
   ;; POSANG_EXTRAP_DECONV                      # Position angle for clump from curve of growth extrapolation with beam deconvolution in units of POSANG_UNIT. Major and minor axes are MAJRMS_EXTRAP_DECONV and MINRMS_EXTRAP_DECONV. [xxx check this]
   ;;                 FLOAT         -0.522936
   ;; POSANG_GCORR_DECONV                       # Position angle for clump from Gaussian fit with beam deconvolution in units of POSANG_UNIT. Major and minor axes are MAJRMS_GCORR_DECONV and MINRMS_GCORR_DECONV.
   ;;                 FLOAT         -0.627581
   ;; POSANG_UNIT     STRING    'rad'           # Unit for POSANG_*
   ;; RADAREA_DECONV  FLOAT           15.1946   # Radius derived from beam deconvolved area in units of xxx.
   ;; RADELL_DECONV   FLOAT           13.0472   # Radius derived from beam deconvolved area from ellipse fit in units xxx.
   ;; RADRMS_EXTRAP_DECONV                      # Radius derived from beam deconvolved RMS size from curve of growth extrapolation. In units of RADRMS_UNIT. The conversion factor for RMS to radius is given by RMSTORAD.
   ;;                 FLOAT           19.5695   
   ;; RADRMS_GCORR_DECONV                       # Radius derived from beam deconvolved RMS size from Gaussian fit. In units of RADRMS_UNIT. The conversion factor for RMS to radius is given by RMSTORAD.
   ;;                 FLOAT           21.9962
   ;; RADRMS_UNIT     STRING    'pc'            # Unit for RADRMS*
   ;; RESOLVED_ELLFIT BYTE         1             # Is clump resolved by elliptical method?
   ;; RESOLVED_EXTRAP BYTE         1             # Is clump resolved by extrapolated method?
   ;; RESOLVED_GCORR  BYTE         1             # Is clump resolved by gaussian method?
   ;; RMSTORAD        FLOAT           1.91000    # Conversion between RMS and radius. The empirical Solomon+ 1987 value is 1.91.
   ;; SIG_TO_FWHM     FLOAT           2.35400    # Conversion between sigma and full-width half max of Gaussian.
   ;; SRPERBEAM       FLOAT       7.04699e-12    # Steradians per beam
   ;; TAG             STRING    ''               # xxx 
   ;; VIRCOEFF        FLOAT           1040.00    # Coefficient for converting to virial mass. [ xxx Not quite sure how 1040 was arrived at. It's 189 in the cprops paper. 189 * rmstorad = 360.99. Then the coefficient in front of the velocity is 1.69, which happens to be similar to ELL_TO_SIG_HALF.]
   ;; VIRMASS_EXTRAP_DECONV                      # Virial mass using the extrapolated curve of growth radius with the beam deconvolved. In units of VIRMASS_UNIT. [xxx what velocity parameter do these estimates use? DELTAV?]
   ;;                 FLOAT           839901.
   ;; VIRMASS_GCORR_DECONV                      # Virial mass using the Gaussian fit  radius with the beam  deconvolved. In units of VIRMASS_UNIT.[xxx what velocity parameter do these estimates use? DELTAV?]
   ;;                 FLOAT       1.01411e+06   
   ;; VIRMASS_RADAREA_DECONV                    # Virial mass using the radius from the area with the beam deconvolved. In units of VIRMASS_UNIT.[xxx what velocity parameter do these estimates use? DELTAV?]
   ;;                 FLOAT       1.57018e+06   
   ;; VIRMASS_RADELL_DECONV                     # Virial mass using the radius from the area estimated using the ellipse fitting technique with the beam deconvolved. In units of VIRMASS_UNIT. [xxx what velocity parameter do these estimates use? DELTAV?]
   ;;                 FLOAT               NaN
   ;; VIRMASS_UNIT    STRING    'msun'           # xxx This should be Msun. Should probably file a bug report on this.
   ;; VPOS            FLOAT           746.749    # Mean velocity of clump in units of VPOS_UNIT
   ;; VPOS_UNIT       STRING    'km/s'           # Unit for VPOS
   ;; VRMS            FLOAT           5.15954    # RMS velocity of clump in units of VRMS_UNIT.
   ;; VRMS_EXTRAP     FLOAT           6.47340    # RMS velocity from curve of growth extrapolation in units of VRMS_UNIT.
   ;; VRMS_EXTRAP_DECONV                         # RMS velocity from curve of growth extrapolation with channel width deconvolved in units of VRMS_UNIT.
   ;;                 FLOAT           6.42404   
   ;; VRMS_GCORR      FLOAT           6.70575    # RMS velocity from Gaussian fit in units of VRMS_UNIT.
   ;; VRMS_GCORR_DECONV                          # RMS velocity from Gaussian fit with the channel size deconolved in units of VRMS_UNIT
   ;;                 FLOAT           6.65812    
   ;; VRMS_UNIT       STRING    'km/s'           # Unit for VRMS*
   ;; XPOS            FLOAT           88.9278    # Mean position of clump in units of XPOS_UNIT. This value is an intensity-weighted, so it will not correspond to the peaks of the emission.
   ;; XPOS_UNIT       STRING    'deg'            # Unit for XPOS_UNIT
   ;; XRMS            FLOAT           12.4468    # RMS size in the X direction in units of XRMS_UNIT.
   ;; XRMS_EXTRAP     FLOAT           16.2934    # RMS size in the X direction from curve of growth extrapolation in units of XRMS_UNIT.
   ;; XRMS_EXTRAP_DECONV                         # RMS size in the X direction from curve of growth extrapolation with the beam deconvolved in units of XRMS_UNIT.
   ;;                 FLOAT           12.3790
   ;; XRMS_GCORR      FLOAT           16.1769    # RMS size in the X direction from Gaussian fit in units of XRMS_UNIT.
   ;; XRMS_GCORR_DECONV                          # RMS size in the X direction from Gaussian fit with the beam deconvolved in units of XRMS_UNIT.
   ;;                 FLOAT           12.2253
   ;; XRMS_UNIT       STRING    'pc'             # Unit for XRMS* parameters.
   ;; YPOS            FLOAT           3.39236    # Mean Position in Y direction in units of YPOS_UNIT. This value is an intensity-weighted, so it will not correspond to the peaks of the emission.
   ;; YPOS_UNIT       STRING    'deg'            # Unit for YPOS
   ;; YRMS            FLOAT           11.7657    # RMS size in the Y direction in units of YRMS_UNIT.
   ;; YRMS_EXTRAP     FLOAT           13.8015    # RMS size in the Y direction from curve of growth extrapolation in units of YRMS_UNIT.
   ;; YRMS_EXTRAP_DECONV                         # RMS size in the Y direction from curve of growth extrapolation with the beam deconvolved in units of YRMS_UNIT.
   ;;                 FLOAT           8.84565
   ;; YRMS_GCORR      FLOAT           15.2916    # RMS size in the Y direction from Gaussian fit in units of YRMS_UNIT.
   ;; YRMS_GCORR_DECONV                          # RMS size in the X direction from Gaussian fit with the beam deconvolved in units of XRMS_UNIT.
   ;;                 FLOAT           11.0272
   ;; YRMS_UNIT       STRING    'pc'             # Unit for YPOS
