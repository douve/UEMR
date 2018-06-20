file_choose <- function(type,path,filename,dialogue="console") {
  names.fl = grep(paste0("(?=.*",filename,
                         ")(?=.*\\.",type,")"),
                  list.files(path=path), perl = T, value = T, ignore.case = T)
  if (dialogue == "window") {
    setwd(path)
    Myfile = choose.files(default=paste0(getwd(), "/*.*"))
  }

  if (dialogue == "console") {
    cat("Select the desired file: (row number)\n")
    unname(cbind(names.fl)) %>% print
    pos = scan(n=1, quiet=T)
    Myfile = paste0(path,"/", names.fl[pos])
  }
  return(Myfile)
}
