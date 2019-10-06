get_long_name <- function(x, long_name, short_name) {
  if(any(is.na(x))) stop("x has NA(s) this function wont work", call. = FALSE)
  clean_x <-
    ifelse(!is.na(match(x, short_name)), 
           long_name[match(x, short_name)], 
           x)
  if(any(is.na(clean_x))) {
    not_mapped <- unique(ifelse(is.na(clean_x), x, ""))
    stop(paste("get_long_name has returned NAs please check name mappings for ",
               paste(not_mapped, collapse = ", ")
               ), 
         call. = FALSE)
  }
  clean_x
}