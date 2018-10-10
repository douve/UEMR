load.packages <- function(x=NULL, update=F, dependent_pcs=F) {
  default.pcs <- c("data.table","tidyverse","tableone",
                   "anytime","VennDiagram","lubridate","epitools", "survival",
                   "readxl")

  if (dependent_pcs) x <- c(x,default.pcs)

  boolps <- .info.packages(x)

  if (sum(!boolps[[1]])) warning(boolps[[2]])
  if (sum(boolps[[1]])) x = x[boolps[[1]]] # packages available to update or install

  .prepare.packages(x,update)

  invisible(lapply(x, require, character.only = TRUE))
}
