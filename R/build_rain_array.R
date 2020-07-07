#' Generate array of CHIRPS rainfall data
#'
#' @param rain_folder the folder path where the daily chirps data are stored
#' @param rain_files list object of the files in that folder, can be creates using list.files()
#'
#' @return 
#' @export
#'
#' @examples
build_rain_array <- function(rain_folder, rain_files) {
  # Compile list of .tif files to be in the array
  array_files <- as.list(paste0(rain_folder,"/",rain_files))
  # Read in as list of matrices
  array_list <- lapply(X = array_files, FUN = tiff::readTIFF)
  # Turn lists into array
  rain_array <- simplify2array(array_list)
  # Return array
  rain_array
}
