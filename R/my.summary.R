# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

my.summary <- function(x,factor=FALSE,showNA=TRUE,digits=NULL,...){

  if(factor){

    if(class(x)!='factor') x= as.factor(x)

    useNA = ifelse(showNA,'always','no')
    tab = table(x,useNA=useNA)
    tab = as.data.frame(tab)

    n= tab$Freq
    N=sum(tab$Freq)
    p = perc(n,N)

    res = n_per(n,p)
    names(res) = as.character(tab$x)


  }else{

  stopifnot(is.numeric(x))

  res = c(mean=mean(x, ...),
    sd=sd(x, ...),
    median=median(x, ...),
    min=min(x, ...),
    max=max(x,...),
    n=length(x))

  if(showNA) res = c(res,'<NA>' = sum(is.na(x)))

  if(!is.null(digits)) res = round(res,digits = digits)

  }

  return(res)

}
