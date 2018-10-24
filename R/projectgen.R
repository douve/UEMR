project.gen <- function(name,unit,folders=c("data", "scr", "output", "doc")) {
  R_files <- c("main","dqa","functions","analysis")
  path <- paste0(.unit.path(unit),name)
  if (!file.exists(path)) {
    dir.create(path)
    setwd(path)
  } else stop('The project you are trying to create already exists')
  sapply(paste0(path,"/",folders), dir.create) %>% invisible
  sapply(paste0(name,"_",R_files,".R"), file.create) %>% invisible
}
