

## unimport: Import any kind of file used in a UEM statistical analysis project.

# 4 type of files: .R, .rds, .txt, .csv
# Requires the path and the filename

unimport = function(type="sidiap",path,filename,...){

  if(type=="R"){
    lf = list.files(path=path,recursive=T,pattern = ".R",full.names=T)
    fn = grep(filename,lf,value=T) # full file name
    source(fn)
  }
  if(type=="rds"){
    lf = list.files(path=path,recursive=T,pattern = ".rds",full.names=T)
    fn = grep(filename,lf,value=T) # full file name
    aux = readRDS(fn)

    return(aux)
  }
  if(type=="sidiap"){
    lf = list.files(path=path,recursive=T,pattern = ".txt",full.names=T)
    fn = grep(filename,lf,value=T) # full file name
    aux = sidiap.import(fn,...)

    return(aux)
  }
  if(type=="catalogue"){
    lf = list.files(path=path,recursive=T,pattern = ".txt|.csv|.xlsx",full.names=T)
    fn = grep(filename,lf,value=T) # full file name
    aux = cat.import(fn,...)

    return(aux)
  }

}



