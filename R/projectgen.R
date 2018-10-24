.unit.path <- function(unit) {
  os <- Sys.info()['sysname']
  if (os=="Windows") {
    unit.path = paste0("//EPOFS/",unit,"/")
  } else if (os=="Linux"){
    unit.path = paste0("/run/user/1000/gvfs/smb-share:server=epofs,share=",unit,"/")
  }
  return(unit.path)
}

project.gen <- function(name,unit,folders=c("data", "scr", "output", "doc")) {
  R_files <- c("main","dqa","functions","analysis")
  path <- paste0(.unit.path(unit),name)
  if (!file.exists(path)) {
    dir.create(path)
    setwd(path)
  } else stop('The project you are trying to create already exists')
  sapply(paste0(path,"/",folders), dir.create) %>% invisible
  setwd(paste0(path,"/scr"))
  sapply(paste0(name,"_",R_files,".R"), file.create) %>% invisible
}

