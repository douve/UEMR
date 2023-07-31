

#' Plots a series of barplots and connects them
#' Modified from https://stackoverflow.com/questions/22560850/barplot-with-connected-series
#'
#' @param dat NxM matrix with N rows as features and M columns as samples
#' @param color Vector of N colors
#' @param space Space between barplots
#' @param alpha Alpha for area connecting barplots
#'
#' @examples
#' dat <- matrix(rnorm(100),10,10)
#' dat <- abs(matrix(rnorm(100),10,10))
#' connectedBarplot(dat, color=rainbow(nrow(dat)))
#'
connectedBarplot <- function(dat, color=rainbow(nrow(dat)), space=1, alpha=0.25,p.border='gray50', ...) {
  b <- barplot(dat, col=color, space = space, ...)

  for (i in seq_len(ncol(dat) - 1)) {
    lines(c(b[i]+0.5, b[i+1]-0.5), c(0, 0),lwd=0.5) ## bottom line

    for (j in seq_len(nrow(dat))) {
      if (j == 1) {
        lines(c(b[i]+0.5, b[i+1]-0.5), c(dat[j,i], dat[j,i+1]),lwd=0.5)
        polygon(c(b[i]+0.5, b[i]+0.5, b[i+1]-0.5, b[i+1]-0.5),
                c(0, dat[j,i], dat[j,i+1], 0),
                col=adjustcolor(color[j], alpha.f=alpha),border=p.border)
      }
      if (j == 2) {
        lines(c(b[i]+0.5, b[i+1]-0.5), c(colSums(dat[1:j,])[i], colSums(dat[1:j,])[i+1]),lwd=0.5)
        polygon(c(b[i]+0.5, b[i]+0.5, b[i+1]-0.5, b[i+1]-0.5),
                c(dat[1,i], colSums(dat[1:j,])[i], colSums(dat[1:j,])[i+1], dat[1,i+1]),
                col=adjustcolor(color[j], alpha.f=alpha),border=p.border)
      }
      if (j > 2) {
        lines(c(b[i]+0.5, b[i+1]-0.5), c(colSums(dat[1:j,])[i], colSums(dat[1:j,])[i+1]),lwd=0.5)
        polygon(c(b[i]+0.5, b[i]+0.5, b[i+1]-0.5, b[i+1]-0.5),
                c(colSums(dat[1:(j-1),])[i], colSums(dat[1:j,])[i], colSums(dat[1:j,])[i+1], colSums(dat[1:(j-1),])[i+1]),
                col=adjustcolor(color[j], alpha.f=alpha),border=p.border)
      }
    }
  }
}


