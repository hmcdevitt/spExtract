#' extract_matrix
#'
#' @param matrix a matrix created from a Raster layer
#' @param coords a 2 column matrix/dataframe with the x and y coordinates in same projection as the Raster layer
#' @param raster_extent an extent object. contains xmin, xmax, ymin, ymax of the Raster layer the matrix was created from
#' @param raster_res a vector with 2 values indicating the x and y resolution of each Raster cell
#'
#' @return
#' @export
#'
#' @examples
extract_matrix <- function(matrix, coords, raster_extent, raster_res) {
  i <- ceiling((coords[,1] - xmin(raster_extent))/raster_res[1])
  i[i==0] <- 1
  i[i>=ncol(matrix)] <- ncol(matrix)
  j <- (nrow(matrix):1)[ceiling((coords[,2] - ymin(raster_extent))/raster_res[2])]
  j[j==0] <- 1
  j[j>=nrow(matrix)] <- nrow(matrix)
  values <- matrix[cbind(j,i)]
  values
}
