
# flength round and add a 0 before in case necessary
flength = function(x, digits = 2, length = 1){
  # afegir number of digits
  sprintf(paste0("%.", length, "f"), round(x, digits))
}
