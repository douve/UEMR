ORsummary <- function(x,...){

  od <- epitools::oddsratio(x,...)

  d <- od$d[-nrow(od$d),-ncol(od$d)]
  n = nrow(d)
  odm = vector(length=n)

  for (i in 1:n)
    odm[i] = .uemFormatC(od$m[i,1], lower = od$m[i,2], upper = od$m[i,3], estimate = T)

  t <- cbind(.uemFormatC(d[,1]),
             .uemFormatC(d[,2]),
             rowSums(d),
             odm,
             .uemFormatC(od$p[1:n,1]))

  colnames(t) <- c(colnames(od$data)[-3], "Sum_row","OR","P-value")

  return(t)

}




