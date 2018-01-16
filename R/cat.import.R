cat.import <- function(file, ...){
  type = stringr::str_extract(file,'[^.]*$')

  if(type == "xlsx")
    out = xlsx::read.xlsx(file, encoding = "UTF-8", ...)
  if(type == "csv")
    out = data.table::fread(file, encoding = "UTF-8", ...)
  if(type == "txt")
    out = sidiap.import(file, ...)

  return(out)
}

