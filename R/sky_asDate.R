#' Change sky date string to date
#'
#' @param skyDate 
#'
#' @return Date vector
#' @export
#'
sky_asDate <- function(skyDate) {
  chr_skyDate <- as.character(skyDate)
  subStart <- stringr::str_locate(chr_skyDate, "day")[, 2]
  dateString <- stringr::str_sub(chr_skyDate, 
                                 start = subStart + 2,
                                 end = nchar(chr_skyDate))
  
  ifelse(is.na(subStart), 
         chr_skyDate,
         as.character(as.Date(lubridate::dmy(dateString))))
  
}