

## unimport: Import any kind of file used in a UEM statistical analysis project.

# 6 type of files: (Rscript, Rimage, data.frame, excel, csv, sidiap and textfile)
# Requires the type, path and the filename

unimport <- function(type,path,filename,exact_string=F, ...){
  elli <- list(...)
  equivalences <-data.table(
    Filetype=c("Rscript","Rimage","Robject","data.frame","excel","csv","sidiap","textfile"),
    Extension=c("R","RData|rda","rds","rds","xlsx|xls","csv","txt","txt"))

  ext <- equivalences[Filetype==type,Extension] # we get the extension of our type

  lf <- list.files(path=path, full.names = T)
  lf <- grepl(ext,tools::file_ext(lf)) %>% lf[.] # lf are the files with the desired type
  if (length(lf)==0) stop('Error: There is no file with the type specified')

  fn <- grep(filename, tools::file_path_sans_ext(basename(lf)),
             ignore.case = T) %>% lf[.] # fn are the candidates to import
  if (length(fn)==0) stop('Error: There is no file with the filename specified')

  if (exact_string) {
    fn <- fn[tools::file_path_sans_ext(basename(fn))==filename]
    if (length(fn)==0) stop('Error: There is no file with the exact filename specified')
  }

  if (length(fn) > 1L) {
    file_choose_names = names(formals(file_choose))
    fn = do.call(file_choose,c(list(fn),dots_f(list(UEMR="file_choose"), elli)))
  }

  if (type %in% c("textfile","excel","csv")) {
    do.call(cat.import,c(fn, elli))
  } else if (type == "Rscript") {
    do.call(source, c(list(fn),dots_f(list(base="source"),elli)))
  } else if (type %in% c("Robject","data.frame")) {
    readRDS(fn)
  } else if (type=="sidiap") {
    sidiap.import(fn)
  } else if (type == "Rimage") {
    load(fn, envir = .GlobalEnv)
  }

}




