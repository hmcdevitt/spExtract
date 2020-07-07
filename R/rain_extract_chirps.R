#' Extract daily rainfall from CHIRPS data
#'
#' @param rain_array an array with dimensions 1 and 2 being the y and x values (respectively) of the rain raster/tiff layers, 
#' and the 3rd dimension is the multiple dates for same spatial extent.
#'  Build using \emph{build_rain_array}
#' @param rain_dates a vector of class Date containing the dates corresponding to each "layer" in the 3rd dimension of the array
#' @param extent an extent object. contains xmin, xmax, ymin, ymax of the raster/tiff layers used to make the array
#' @param resolution a vector with 2 values indicating the x and y resolution of each raster cell
#' @param cumrain (defualt = 0) the number of days used to calculate the cumulative rainfall at a location
#' @param coords_dates a vector of class Date of the dates for each of the coordinates in coords
#' @param coords a 2 column matrix/dataframe with the x and y coordinates in same projection as extent
#'
#' @return
#' @export
#'
#' @examples
rain_extract_chirps <- function(rain_array,
                                rain_dates,
                                extent,
                                resolution,
                                cumrain = 0, 
                                coords_dates,
                                coords)  { # needs to be in same crs as rain data
  
  # warning messages
  if (length(coords_dates) > 1 && length(coords_dates) != nrow(coords))
    stop("Dates must be either one date for all points, or one date for each point")
  
  # empty data storage
  rain_results <- NULL
  
  # use loop as spatial ratsers may be quite large to use apply  
  for(i in 1:length(coords_dates)){
    
    # array layer index
    day_ind <- match(coords_dates[i], rain_dates)
    
    
    # if cumulative rainfall is needed
    if(cumrain!=0) {
      
      rain_seq <- c()
      # subset rain layers corresponding to date i
      for(day in 0:cumrain) {
        
        # extract values at location i
        rain <- extract_array(array = rain_array, extent = extent, coords = coords[i,], 
                              third_dim_ind = day_ind-day,
                              resolution = resolution)
        
        rain_seq <- c(rain_seq, rain)
        
      }
      rain_seq
      
      
      # fill in results
      rain_results <- rbind(rain_results,
                            data.frame(daily=rain_seq[1],
                                       cum_rain = sum(rain_seq,na.rm=T)))
      
      # if cumulative rainfall is NOT needed
    }else{
      
      # extract values at location i
      rain <- extract_array(array = rain_array, extent = extent, coords = coords[i,], 
                            third_dim_ind = day_ind,
                            resolution = resolution)
      
      # fill in results
      rain_results <- rbind(rain_results,
                            data.frame(daily=rain[1]))
    }
  }
  
  
  
  return(rain_results)
  
  
  
}

