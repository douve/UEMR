info_packages <- function(x) {
  boolps <- x %in% available.packages()[,1]
  if (any(!boolps)) {
    n <- sum(!boolps)
    p <- "Package"
    if (n>1) {
      be <- "are"
      p <- paste0(p,"s")
    } else be <- "is"

    frase <- paste0(p," ",paste(x[!boolps],collapse=",")," ",be,
                    " not available (for R version ",getRversion(),")\n")
    l <- list(boolps, frase)
  } else {l <- list(boolps, "Packages are available")}

  return(l)
}
