

## sidiap.import function

# Use fread configured to SIDIAP databases:
sidiap.import = function(x,sep="|",...){

  data.table::fread(x,sep=sep,...)

}
