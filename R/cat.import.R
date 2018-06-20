cat.import <- function(file, ...){
  type = stringr::str_extract(file,'[^.]*$')
  if(type == "xlsx") {
    l <- list(...)
    if (is.null(l$sheetName) & is.null(l$sheetIndex)) l$sheetIndex = 1
    f = list(file)
    l = c(f,l)
    out = data.table::setDT(do.call(xlsx::read.xlsx, l))
  } else {
    out = data.table::fread(file, ...)
  }
  out[out==''] <- NA
  return(out)
}
