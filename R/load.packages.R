.prepare.packages <- function(x, update=F) {

  pcs_inst <- x[x %in% installed.packages()[,1]]

  if (length(pcs_inst)) {
    pcs_inst_not_up <- pcs_inst[pcs_inst %in% old.packages()[,1]] # packages installed but not updated
    pcs_inst_up <- setdiff(pcs_inst,pcs_inst_not_up) # packages installed and updated

    pcs_do_not_update <- c()
    if (length(pcs_inst_up)) pcs_do_not_update <- pcs_inst_up
    if (!update) pcs_do_not_update <- c(pcs_do_not_update, pcs_inst_not_up)
    prep.pcs <- setdiff(x,pcs_do_not_update)
  } else prep.pcs <- x

  if (length(prep.pcs)) install.packages(prep.pcs, dependencies = T)
}



.info.packages <- function(x) {
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



load.packages <- function(x=NULL, update=F, dependent_pcs=T) {
  default.pcs <- c("data.table","tidyverse","tableone",
                   "anytime","VennDiagram","lubridate",
                   "epitools", "survival","readxl",'ggplot2')

  if (dependent_pcs) x <- c(x,default.pcs)

  recom <- installed.packages() %>% data.table %>% .[Priority=="recommended"] %>%
    .[["Package"]] # recommended packages
  xrecom <- intersect(x,recom)
  x <- setdiff(x,xrecom)

  boolps <- .info.packages(x)

  if (sum(!boolps[[1]])) warning(boolps[[2]])
  if (sum(boolps[[1]])) x = x[boolps[[1]]] # packages available to update or install

  .prepare.packages(x,update)

  invisible(lapply(c(x,xrecom), require, character.only = TRUE))
}
