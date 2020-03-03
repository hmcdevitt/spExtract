#' ndvi_extraction
#'
#' @description This function matches NDVI values to spatial point data. It will return:
#' 
#'   1. \strong{iNDVI}: interpolated NDVI, giving the estimated value of NDVI if the
#'   date of the spatial data falls in between the dates of NDVI layers
#'   (which are produced at 16 day intervals)     
#'
#'   2. \strong{dNDVI}: delta NDVI describes the change in NDVI between the closest
#'   date layer and the layer previous  
#'   
#'   3. \strong{aNDVI}: anomally NDVI is \emph{(iNDVI-mNDVI)/mNDVI} 
#'   where mNDVI is the long-term mean NDVI value for the given cell
#'
#' The function is most accurate using projected data, i.e. both the NDVI files and point data should
#' be in UTM (or similar). The function will still work if the data are in latlong, however there may be
#' some cases where the values returned are associated with a neighbouring NDVI cell. This is due to
#' rounding issues.
#' 
#' The code used in this function is based off code written by Jon Lindsay and Thomas Morrison.
#'
#' @param files_desc object returned from \emph{build_ndvi_desc}
#' @param array an array with dimensions 1 and 2 being the y and x values (respectively) of the rasters layers
#'  identified in files_desc, the 3rd dimension is multiple dates for same spatial extent.
#'  Build using \emph{build_ndvi_array}
#' @param extent an extent object. contains xmin, xmax, ymin, ymax of the raster layers used to make the array
#' @param resolution a vector with 2 values indicating the x and y resolution of each raster cell
#' @param avg_matrix (default: NULL) if calculating anomally NDVI this is a matrix with the long-term mean NDVI values
#' @param date a vector of the dates
#' @param coords a 2 column matrix/dataframe with the x and y coordinates in same projection as extent
#'
#' @return
#' @export
#'
#' @examples
ndvi_extraction <- function(files_desc,
                            array,
                            extent,
                            resolution,
                            avg_matrix = NULL,
                            date,
                            coords) {

  # Warning messages
  if (length(date) > 1 && length(date) != nrow(coords))
    stop("Dates must be either one date for all points, or one date for each point")

  # Check if extraction is across multiple dates
  if (length(date) == 1) {multi_extract <- T} else {multi_extract <- F}

  # Initiate results dataframe
  results <- data.frame(indvi = rep(NA, length(date)),
                        dndvi = rep(NA, length(date)),
                        andvi = rep(NA, length(date)))
  
  # Loop throught each dat
  for(i in 1:length(date)) {
    # find the index of the closest date in the date vector
    z_ind <- which.min(abs(as.numeric(files_desc$date-(date[i]))))

    # initialize the before and after layer indices as closest date index
    lo_zind <- z_ind
    hi_zind <- z_ind

    ## now calculate whether we want to use the previous or next raster for interpolation;
    ## switch is faster for performing these kind of ifelse operations, so scale and normalize
    diff <- as.numeric(files_desc$date[z_ind]) - as.numeric(date[i])      # -n, 0, n
    if (diff != 0) diff <- diff/abs(diff)
    diff <- diff + 2                                                      #  1, 2, 3

    ## 1 means interpolate forward, 3 means interpolate backwards; 2 means we are on an NDVI
    ## layer date, so no interpolation needed
    switch(diff, hi_zind <- z_ind + 1, interp <- 1, lo_zind <- z_ind - 1)

    ## get the scaling value for interpolation if needed; check if we are interpolating
    if (hi_zind != lo_zind){
      ## calculate the linear scaling value from the previous to the next NDVI values
      interp <- (as.numeric(date[i]) - as.numeric(files_desc$date[lo_zind])) /
        (as.numeric(files_desc$date[hi_zind]-files_desc$date[lo_zind]))
    } else {

      ## we are bang on an NDVI date; check how we need to adjust where we are interpolating to:
      ## full interpolation from the previous NDVI date
      if (lo_zind > 1){
        lo_zind <- lo_zind - 1
        ## no interpolation from the current NDVI date
      } else {
        hi_zind <- hi_zind + 1
        interp <- 0
      }
    }

    ## get the before and after NDVI values
    if (multi_extract){
      ndvi_start <- extract_array(array = array, extent = extent, coords = coords, third_dim_ind = lo_zind, resolution = resolution)
      ndvi_end <- extract_array(array = array,  extent = extent, coords = coords, third_dim_ind = hi_zind, resolution = resolution)
    } else {
      ndvi_start <- extract_array(array = array, extent = extent, coords = coords[i,], third_dim_ind = lo_zind, resolution = resolution)
      ndvi_end <- extract_array(array = array, extent = extent, coords = coords[i,], third_dim_ind = hi_zind, resolution = resolution)
    }

    ## calculate NDVI changes, and then interpolate the estimated NDVI value
    # interpolated NDVI value
    indvi <- ndvi_start + (interp * (ndvi_end - ndvi_start))
    # delta NDVI value
    dndvi <- (ndvi_end - ndvi_start) / as.numeric(files_desc$date[hi_zind] - files_desc$date[lo_zind], units = "days")

    if(length(avg_matrix) > 0){
      avg.ndvi.val <- extract_matrix(matrix = avg_matrix, coords = coords[i,], raster_extent = extent, raster_res  = resolution)
      andvi <- (indvi-avg.ndvi.val)/avg.ndvi.val # anomaly NDVI
    }else{
      andvi <- NA
    }

    results$indvi[i] <- indvi
    results$dndvi[i] <- dndvi
    results$andvi[i] <- andvi

  }

  return(results)
}
