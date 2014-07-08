# CPROPStoo

Welcome!

This is an IDL package to aid in the analysis of emission line data
cubes. It expands and modularizes the CPROPS package introduced by
Rosolowsky & Leroy (2006). It provides utilities to estimate noise and
mask cubes, identify local maxima, partition cubes into object
assignments, and derive properties from object assignments.

### User Tasks

These are high level tasks are intended to be accessed by the user. A
normal cube analysis path calls them in sequency.

###### Units and Book Keeping

* cprops_check_header : verify that a cube is appropriate for
  analysis: units are K, km/s, and has beam info in header.

* prep_cube : attempt to place a cube in correct units. This can be
  hit-or-miss but will save you some annoying work if it fires off
  correctly.

###### Signal Identification

* make_noise_cube : accept a cube and optionally a mask and return 0,
  1, 2, or 3d noise estimates.

* make_cprops_mask : makes a mask based on joint thresholding.

###### Feature Identification

* find_local_max : accept a cube and mask and return a set of local
  maxima.

* assign_clfind : accept a list of kernels and a cube and use the
  CLUMPFIND approach (nearest neighbors) to generate an assignment
  cube.

* assign_cprops : accept a list of kernels and a cube use the CPROPS
  approach (unique associated isocontours) to generate an assignment
  cube.

###### Feature Characterization

* cube_to_moments : extract moment measurements for a list of clouds
  given an assignment cube and a data cube

* cube_to_level_moments : measure moments for each kernel plus contour
  combination in a cube. Feeds dendrogram analysis or other multiscale
  approaches.

* moments_to_props : calculate cloud properties based on moment
  measurements and other physical information.

* extract_dendro : extract a tree diagram from the level moments type
  structure.

###### Results Analysis

###### Monte Carlo

* add_noise_to_cube : add correlated noise to a data cube. Useful for
  Monte Carlo calculations.

### Lower Level Routines

These programs are called by the higher level routines. They may still
be of general use.

###### Signal Identification

* grow_mask : manipulate masks.

###### Peak Identification

* alllocmax : find all candidate local maxima via rolling a cube.

* decimate_kernels : reject candidate local maxima

* mergefind_approx : solve for merger levels among a set of kernels

* write_kernels : write a set of kernels to a text or IDL file

##### Feature Characterization

* calc_props_from_moments: convert moment measurements to properties.

###### Structure Handling

* add_props_fields [documented] : add fields to a structure related to property
  measurements

* alphabetize_struct [documented] : alphabetize the fields in a
  structure.

* empty_moment_struct [documented] : initialize an empty moment structure.

###### Vector Analysis

<em> Note that these are potentially of wide general use. </em>

* contour_values : return contours given data and some criteria

* mad : meadian absolute deviation. Cheap, robust noise estimate.

###### Cube Infrastructure

<em> Note that these are potentially of general use for moving fluidly
between cube data and vectorized analysis. </em>

* cubify [documented] : convert a vector of {x,y,v,t} measurements into a cube

* vectorify [documented] : convert a subset of a cube into {x,y,v,t} vector

* xyv_to_ind : convert pixels to cube index

* ind_to_xyv : convert cube index to pixels

###### Display

<em> A long term goal would be to deprecate these in favor the Coyote
graphic libraries. But the overhead in getting the disp functionality
from the cg routines may preclude ever actually doing that. </em>

* disp : two-d image display program.

* fasthist : quick histogram program.

###### Moment Calculation

* extrap : use a curve-of-growth analysis to correct moments for
  finite sensitivity

* measure_moments : given a vector x, y, v, and t calculate a
  structure containing moments, wrapping the extrapolation

* calc_props : expands a moment structure into a property structure.

* pa_moment : use PCA to find suggest the major and minor axis

* stat_mask : extract statistics on regions inside a mask
