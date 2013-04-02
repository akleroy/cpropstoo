CPROPStoo

--- User Tasks ---

These are intended to be accessed by the user.

* cprops_check_header : verify that a cube is appropriate for
  analysis: units are K, km/s, and has beam info in header.

* prep_cube : attempt to place a cube in correct units

* make_noise_cube : accept a cube and optionally a mask and return 0,
  1, 2, or 3d noise estimates.

* make_cprops_mask : makes a mask based on joint thresholding

* find_local_max : accept a cube and mask and return a set of local
  maxima.

* assign_clfind : accept a list of kernels and a cube and use the
  CLUMPFIND approach (nearest neighbors) to generate an assignment
  cube.

* assign_cprops : accept a list of kernels and a cube use the CPROPS
  approach (unique associated isocontours) to generate an assignment
  cube.

* (additional assignment procedures)

* cube_to_moments : extract moment measurements for a list of clouds
  given an assignment cube and a data cube

* cube_to_level_moments : measure moments for each kernel plus contour
  combination in a cube. Feeds dendrogram analysis or other
  multiscale approaches.

* moments_to_props : calculate cloud properties based on moment
  measurements and other physical information.

* (dendrogram) : extract a tree diagram from the level moments type
  structure.

--- Infrastructure ---

These are called by other programs, though they may be of general use.

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

--- Next Moles ---

Roughly in priority order

* add a multiline decomposer

* map levelprops to levelmoments in from dendro

* add monte carlo uncertainty to moment measurement 

* put derivative decimation of kernels back in

* put a curve-of-growth tester in

* gaussianity and roundedness based region rejection

* that's not a mole, that's a python

* look into 2-d decomposer / gridcore
