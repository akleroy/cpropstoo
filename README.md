CPROPStoo
=========

Welcome!

This is an IDL package to aid in the analysis of emission line data
cubes. It expands and modularizes the CPROPS package introduced by
Rosolowsky & Leroy (2006). It provides utilities to estimate noise and
mask cubes, identify local maxima, partition cubes into object
assignments, and derive properties from object assignments.

#### User Tasks

These tasks are intended to be accessed by the user.

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

###### Feature Identification ---

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
  combination in a cube. Feeds dendrogram analysis or other
  multiscale approaches.

* moments_to_props : calculate cloud properties based on moment
  measurements and other physical information.

* extract_dendro : extract a tree diagram from the level moments type
  structure.

###### Monte Carlo

* add_noise_to_cube : add correlated noise to a cube.   

----------------------
--- Infrastructure ---
----------------------

These programs are called by other routines and not intended to be
user facing. They may still be of general use, of course.

* add_noise_to_cube : add correlated noise to a data cube. Useful for
  Monte Carlo calculations.

* add_props_fields : structure utility

* alllocmax : find all candidate local maxima via rolling a cube.

* calc_props_from_moments: convert moment measurements to properties.

* contour_values : return contours given data and some criteria

* cubify : convert a vector of {x,y,v,t} measurements into a cube

* decimate_kernels : reject candidate local maxima

* empty_moment_struct : initialize an empty moment structure.

* empty_prop_struct : initialize an empty property structure.

* empty_cloud_struct : initialize an empty cloud structure.

* extrap : use a curve-of-growth analysis to correct moments for
  finite sensitivity

* grow_mask : manipulate masks.

* ind_to_xyv : convert cube index to pixels

* mad : meadian absolute deviation. Cheap, robust noise estimate.

* measure_moments : given a vector x, y, v, and t calculate a
  structure containing moments, wrapping the extrapolation

* mergefind_approx : solve for merger levels among a set of kernels

* calc_props : expands a moment structure into a property structure.

* pa_moment : use PCA to find suggest the major and minor axis

* stat_mask : extract statistics on regions inside a mask

* vectorify : convert a subset of a cube into {x,y,v,t} vector

* write_kernels : write a set of kernels to a text or IDL file

* xyv_to_ind : convert pixels to cube index
