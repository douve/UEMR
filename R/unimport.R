

## unimport: Import any kind of file used in a UEM statistical analysis project.

# 4 type of files: .R, .rds, .txt, .csv
# Requires the path and the filename

unimport <- function(type,path,filename,exact_match=F, ...){

  if (type=="sidiap") {
    lf = list.files(path=path,recursive=T,pattern = ".txt")
    fn = paste0(path,"/",grep(filename,lf,value=T))
    aux = sidiap.import(fn,...)
  } else {
    lf = list.files(path=path,recursive=T, pattern = paste0(".",type))
    if (exact_match) {
      fn = paste0(path,"/",
                  grep(paste0("^",filename,".",type,"$"),
                       lf,value=T, ignore.case = T))
    } else {
      fn = paste0(path,"/",grep(filename,lf,value=T, ignore.case = T))
    }
    dots = list(...)

    if (length(fn) > 1L) {
      fnames = names(formals(file_choose))
      fn = do.call(file_choose,
                   c(type,path,filename,dots[names(dots) %in% fnames]))
    }

    if (type == "rds") {
      aux = readRDS(fn)
    } else if (type == "R") {
      aux = source(fn)
    } else if (type %in% c("txt","csv","xlsx")) {

      aux = do.call(cat.import,
                    c(normalizePath(fn),
                      dots_f(list(
                        xlsx="read.xlsx",
                        data.table=c("fread")), dots)))
    }
  }
  return(aux)
}




