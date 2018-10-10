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
