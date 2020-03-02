#' build_ndvi_array
#'
#' @param files_desc object returned from *build_ndvi_desc*
#'
#' @return
#' @export
#'
#' @examples
build_ndvi_array <- function(files_desc) {
  # Compile list of .tif files to be in the array
  array_files <- as.list(paste0(files_desc$folder,"/",files_desc$filenames))
  # Read in as list of matrices
  array_list <- lapply(X = array_files, FUN = readTIFF)
  # Turn lists into array
  ndvi_array <- simplify2array(array_list)
  # Return array
  ndvi_array

}
