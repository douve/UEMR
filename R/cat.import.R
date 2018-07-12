## Import data in (excel, csv, textfile) types

cat.import <- function(file, ...){
  type = stringr::str_extract(file,'[^.]*$')
  if (grepl("xlsx|xls",type)) {
    l <- dots_f(list(readxl="read_excel"), list(...))
    if (is.null(l$sheet)) l$sheet = 1
    l = c(list(file),l)
    out = data.table::setDT(do.call(readxl::read_excel, l))
  } else {
    l <- dots_f(list(data.table="fread"), list(...))
    l = c(list(file),l)
    out = do.call(data.table::fread, l)
  }
  out[out==''] <- NA
  return(out)
}
