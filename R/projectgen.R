.unit.path <- function(unit) {
  os <- get_os()
  if (os=="windows") {
    unit.path = paste0("//EPOFS/",unit,"/")
  } else if (os=="linux"){
    unit.path = paste0("/run/user/1000/gvfs/smb-share:server=epofs.fjgol.loc,share=",unit,"/")
  } else if (os=='osx' & grepl('Air',Sys.info()['nodename'])){
    unit.path = paste0("/Users/user/Documents/",unit,"/")
  } else if(OS=='osx' & !grepl('Air',Sys.info()['nodename'])) {
    path = paste0('/Volumes/virievac/',unit,"/")
  }
  return(unit.path)
}



project.gen <- function(name,unit,folders=c("data", "scr", "output", "doc","trash"),paths=TRUE) {

  R_files <- c("main","data_management","dqa","variables","analysis")
  path <- paste0(.unit.path(unit),name)
  if (!file.exists(path)) {
    dir.create(path)
    setwd(path)
  } else stop('The project you are trying to create already exists')
  sapply(paste0(path,"/",folders), dir.create) %>% invisible
  setwd(paste0(path,"/scr"))
  sapply(paste0(name,"_",R_files,".R"), file.create) %>% invisible

  if(paths){
    paths = list(
      path.data = grep('data',list.dirs(path,recursive = FALSE),value=T),
      path.output = grep('output',list.dirs(path,recursive = FALSE),value=T),
      path.scr = grep('scr',list.dirs(path,recursive = FALSE),value=T))

    return(paths)
  }

}


