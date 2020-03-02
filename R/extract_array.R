#' extract_array
#' 
#' This function extracts the values from an array for a set of spatial coordinates. 
#' The coordinates must be in the same projection as the raster layers used to create the array.
#'   
#' Note: currently this function works on only one layer at a time. 
#' To extract the value from different layers at the same coordinates this function will need to be
#' used in a loop, changing *third_dim_ind* for each iteration to move through the array temporally.
#' This functionality will be updated in future versions.
#' 
#' @param array an array with dimensions 1 and 2 being the y and x values (respectively) of the rasters layers,
#'  the 3rd dimension is the multiple layers for same spatial extent (i.e. multiple layers of the same area through time)
#' @param extent an extent object. contains xmin, xmax, ymin, ymax of the raster layers used to make the array
#' @param coords a 2 column matrix/dataframe with the x and y coordinates in same projection as extent
#' @param third_dim_ind index of which layer to be used, i.e. third dimension
#' @param resolution a vector with 2 values indicating the x and y resolution of each raster cell
#'
#' @return
#' @export
#'
#' @examples
extract_array <- function(array, extent, coords, third_dim_ind, resolution) {
  a <- ceiling((coords[,1] - raster::xmin(extent))/resolution[1])
  b <- (nrow(array):1)[ceiling((coords[,2] - raster::ymin(extent))/resolution[2])]
  ndvi <- array[cbind(b, a, third_dim_ind)]
  ndvi
}

