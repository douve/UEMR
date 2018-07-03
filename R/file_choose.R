## Return the path of a selected file

file_choose <- function(input,dialogue="console") {

  if (dialogue == "console") {
    cat("Select the desired file: (row number)\n")
    input %>% basename %>% cbind %>% unname %>% print
    pos = scan(n=1, quiet=T)
    Myfile = input[pos]
  } else if (dialogue == "window") {
    path <- sub('/([^/]*)$', '', input[1])
    setwd(path)
    Myfile = choose.files(default=paste0(getwd(), "/*.*"))
  }
  return(Myfile)
}
