#' build_ndvi_desc
#'
#' @param folder tbc
#' @param date.loc tbc
#' @param date.format tbc
#' @param start.date tbc
#' @param end.date tbc
#'
#' @return
#' @export
#'
#' @examples
build_ndvi_desc <- function(folder, # full path location of folder containing NDVI geotifs
                            date.loc=c(15,21), # character location in filenames of NDVI containing dates
                            date.format="%Y%j", # format of dates
                            start.date = NULL, 
                            end.date = NULL){

  ndvi.data <- data.frame(filenames=list.files(folder,pattern = "MOD13Q1"),
                        year=NA,
                        date=ymd("1970-01-01"))

  ndvi.data$filenames <- as.character(ndvi.data$filenames)

  ndvi.data$date <- as.Date(substr(ndvi.data$filenames, date.loc[1], date.loc[2]),
                          format=date.format,
                          tz="GMT")

  ndvi.data$year <- year(ndvi.data$date)

  ndvi.data$folder <- folder

  ## order by date
  ndvi.data <- ndvi.data[order(ndvi.data$date),]
  ndvi.data[,1] <- as.character(ndvi.data[,1])

  ## filter by start and end date if provided
  if (!is.null(start.date)){
    start.date <- as.Date(start.date)
    ndvi.data <- ndvi.data[ndvi.data$date >= start.date,]
  }

  if (!is.null(end.date)){
    end.date <- as.Date(end.date)
    ndvi.data <- ndvi.data[ndvi.data$date <= end.date,]
  }

  return(ndvi.data)
}
