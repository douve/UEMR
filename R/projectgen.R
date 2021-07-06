.unit.path <- function(unit) {
  os <- get_os()
  if (os=="windows") {
    unit.path = paste0("//EPOFS/",unit,"/")
  } else if (os=="linux"){
    unit.path = paste0("/run/user/1000/gvfs/smb-share:server=epofs.fjgol.loc,share=",unit,"/")
  } else if (os=='osx' & grepl('Air',Sys.info()['nodename'])){
    unit.path = paste0("/Users/danouchi/Google Drive/My Drive/",unit,"/")
  } else if(OS=='osx' & !grepl('Air',Sys.info()['nodename'])) {
    unit.path = paste0('/Volumes/virievac/',unit,"/")
  }
  return(unit.path)
}



project.gen <- function(name,unit,folders=c("data", "scr", "output", "doc","trash")) {

  R_files <- c("main","data_management","dqa","variables","analysis")
  path <- paste0(.unit.path(unit),name)
  if (!file.exists(path)) {
    dir.create(path)
    setwd(path)
  } else stop('The project you are trying to create already exists')
  sapply(paste0(path,"/",folders), dir.create) %>% invisible

  # path definition
  paths = list(
      path.data = grep('data',list.dirs(path,recursive = FALSE),value=T),
      path.output = grep('output',list.dirs(path,recursive = FALSE),value=T),
      path.scr = grep('scr',list.dirs(path,recursive = FALSE),value=T))

  # add scripts in scr folder, then write on main script:
  setwd(paste0(path,"/scr"))
  sapply(paste0(name,"_",R_files,".R"), file.create) %>% invisible

  # Open rscript:
  fc <- file(grep('main',list.files(path,'.R',recursive=T,full.names = T),value=T))

  # add command to the script:
  writeLines(text = c('\n\n',
                      paste0('#### ',name,': main script --------------------------------------------------------------------'),
                      paste0('project_name=\"',name,'\"'),
                      'date = Sys.Date()\n\n',

                      '# Load packages --------------------------------------------------------------------',
                      'require(UEMR)',
                      'load.packages()\n',

                      '## Set path to folders:',
                      paste0('path.data = \"',paths$path.data,"\""),
                      paste0('path.output = \"',paths$path.output,"\""),
                      paste0('path.scr = \"',paths$path.scr,"\""),

                      '\n\n\n\n\n',
                      '# Load data --------------------------------------------------------------------',
                      'scriptname = paste0(project_name,"_data_management")',
                      'unimport(type="Rscript",path.scr,scriptname)',


                      '\n\n\n\n\n',
                      '# Data Quality Assessment --------------------------------------------------------------------',
                      'scriptname = paste0(project_name,"_dqa")',
                      'unimport(type="Rscript",path.scr,scriptname)',



                      '\n\n\n\n\n',
                      '# Variables and objects definition --------------------------------------------------------------------',
                      'scriptname = paste0(project_name,"_variables")',
                      'unimport(type="Rscript",path.scr,scriptname)',


                      '\n\n\n\n\n',
                      '# Statistical analysis --------------------------------------------------------------------',
                      'scriptname = paste0(project_name,"_analysis")',
                      'unimport(type="Rscript",path.scr,scriptname)'),fc)

  # close connection:
  close(fc)


}

