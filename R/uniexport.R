
.path_to_export <- function(path, filename, ext, date=FALSE) {
  n <- nchar(path)
  if (stringr::str_sub(path,start = n,end = n)!="/") path <- paste0(path,"/")
  if (date) d <- paste0("_",Sys.Date()) else d <- ""
  paste0(path,filename,d,".",ext)
}

.dots_f <- function(l, args) {
  y <- names(l)
  funs <- lapply(y, function(y) paste0(y,"::",l[[y]])) %>% unlist
  params <- lapply(funs,
                   function(x) eval(parse(text = x)) %>% formals %>% names) %>% unlist
  params <- unique(params)
  args[names(args) %in% params]
}



## uniexport: Export any kind of file used in a UEM statistical analysis project.

# 5 type of files: (Robject, Rimage, excel, csv and textfile)
# Requires the Robject, type, path and the filename

uniexport <- function(Robject,type,path,filename=NULL,date=F, ...){
  elli <- list(...)
  if (is.null(filename)) filename <- Robject
  Robject <- get(Robject)
  eqDT <- data.table(
    Filetype=c("Rimage","Robject","excel","csv","textfile"),
    ext=c("RData","rds","xlsx","csv","txt"))

  ext <- eqDT[Filetype==type,ext]
  ad <- .path_to_export(path,filename,ext,date)

  if (type %in% c("textfile","csv")) {
    do.call(write.table,
            c(list(x=Robject,file=ad),.dots_f(list(utils="write.table"),elli)))
  } else if (type == "excel") {
    do.call(xlsx::write.xlsx2,
            c(list(x=Robject,file=ad),.dots_f(list(xlsx="write.xlsx2"),elli)))
  } else if (type == "Robject") {
    saveRDS(Robject,ad)
  } else if (type == "Rimage") {
    save.image(ad)
  }
}

