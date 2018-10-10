.path_to_export <- function(path, filename, ext, date=FALSE) {
  n <- nchar(path)
  if (stringr::str_sub(path,start = n,end = n)!="/") path <- paste0(path,"/")
  if (date) d <- paste0("_",Sys.Date()) else d <- ""
  paste0(path,filename,d,".",ext)
}
