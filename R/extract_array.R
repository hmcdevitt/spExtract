#' extract_array
#'
#' @param array an array with dimensions 1 and 2 being the x and y values of the rasters layers identified in files_desc,
#'  the 3rd dimension is multiple dates for same spatial extent
#' @param extent an extent object. contains xmin, xmax, ymin, ymax of layers in array
#' @param coords  a 2 column matrix/dataframe with the x and y coordinates in same projection as extent
#' @param third_dim_ind index of which layer to be used, i.e. third dimension
#' @param resolution a vector of 2 numers indicating the cell resolution of the spatial object
#'
#' @return
#' @export
#'
#' @examples
extract_array <- function(array, extent, coords, third_dim_ind, resolution) {
  a <- ceiling((coords[,1] - xmin(extent))/resolution[1])
  b <- (nrow(array):1)[ceiling((coords[,2] - ymin(extent))/resolution[2])]
  ndvi <- array[cbind(b, a, third_dim_ind)]
  ndvi
}

