#' extract_matrix
#'
#' @param matrix tbc
#' @param coords tbc
#' @param raster_extent tbc
#' @param raster_res tbc
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
