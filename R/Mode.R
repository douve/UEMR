Mode <- function(x) {
  ux <- unique(x)
  ux <- ux[!is.na(ux)]
  ux[which.max(tabulate(match(x, ux)))]
}
