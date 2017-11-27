uemFormatC <- function(x,estimate=FALSE,lower,upper,digits=2,format="f",...){


  if(estimate){

    return(paste0(formatC(x, digits=digits, format=format), " (",
                  formatC(lower, digits=digits, format=format), "-",
                  formatC(upper, digits=digits, format=format), ")"))

    odm = vector(length=n)
    n = nrow(x)
    for (i in 1:n) {
      odm[i] = paste0(formatC(x[i,1],digits = digits,format = format), " (",
                      formatC(x[i,2],digits = digits,format = format),"-",
                      formatC(x[i,3],digits = digits,format = format), ")")
    }
    return(odm)
    # lower,upper són paràmetres que s'activen si OR=TRUE (opcionals)
    # si lower,upper són NA no podem interval
  }

  if(any(x>1, na.rm = TRUE)){
    N = sum(x)
    return(
      paste0(formatC(x,digits=2,format='f'), " (",
             formatC(x*100/N,digits=digits,format=format),")")
    ) # freq i el seu %
  } else formatC(x,digits=digits,format=format) # pval

}
