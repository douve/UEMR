

## unimport: Import any kind of file used in a UEM statistical analysis project.

# 4 type of files: .R, .rds, .txt, .csv
# Requires the path and the filename

unimport = function(type="sidiap",path,filename){

  if(type=="R"){
    lf = list.files(path=path,pattern = ".R",full.names=T)
    fn = grep(filename,lf,value=T) # full file name
    source(fn)
  }
  if(type=="rds"){
    lf = list.files(path=path,pattern = ".rds",full.names=T)
    fn = grep(filename,lf,value=T) # full file name
    readRDS(fn)
  }
  if(type=="sidiap"){
    lf = list.files(path=path,pattern = ".txt",full.names=T)
    fn = grep(filename,lf,value=T) # full file name
    aux = sidiap.import(fn)

    # busca i formateja dates....
    return(aux)
  }
  if(type=="catalogue"){
    lf = list.files(path=path,pattern = ".txt|.csv",full.names=T)
    fn = grep(filename,lf,value=T) # full file name
    cat.import(fn)
  }

}
