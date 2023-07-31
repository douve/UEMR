.reduce.month <- function(x) {
  while(!x%in%1:12) x <- x - 12L
  return(x)
}

.reduce.month <- function(x) {
  i <- 1L
  while(!x%in%1:12) {
    x <- x - 12L
    i <- i + 1L
  }
  return(list(month=x,year=i))
}


.days_in_month2 <- function(x,year) {
  Matr <- sapply(x, reduce.month)
  X <- mapply(function(x,y)Days_in_month(x,y),
              x=Matr[1,], y=year[unlist(Matr[2,])])
  return(X)
}




`Pr(Yj|?R^dose)` <- function(Date_month,nddd,compra,year) {
  if (!is.Date(Date_month)) {
    m <- days_in_month2(Date_month,year)
  } else m <- days_in_month2(month(Date),year)

  if (compra) {
    x <- 1:m
    p0 <- ifelse(nddd>=x,x,nddd)/m
  } else p0 <- rep(0,m)

  return(p0)
}


.last_TRUE <- function(x) length(x)-match(TRUE,rev(x))+1
.last_m_len <- function(vec) {
  len_vec <- length(vec)
  if (len_vec < 12L) vec <- c(rep(F,12-len_vec),vec)
  m <- vector(mode = 'integer', length = len_vec)
  m[1] <- NA
  for (i in 2:len_vec) {
    m[i] <- last_TRUE(vec[seq(1,i-1)])
  }
  return(m)
}


`Pr(Y)` <- function(compra,nddd_DC,nddd_Dinc,p_bona=.9,year) {

# ---------------------------------------------------------------------
  # aux probabilities functions
# --------------------------------------------------------------------
  `Pr(Y|dose)` <- function(compra,nddd_DC,nddd_Dinc,p_bona=.9,dose,year) {

    `Pr(R|DCDC)` <- `Pr(R|DC?DC)` <- `Pr(R|dose)`(compra,nddd_DC,year)
    `Pr(R|?DCDC)` <- `Pr(R|?DC?DC)` <- `Pr(R|dose)`(compra,nddd_Dinc,year)

    `Pr(Y|?R^DCDC)` <- `Pr(Y|?R^?DCDC)` <- `Pr(Y|?R^dose)`(compra,nddd_DC,year)
    `Pr(Y|?R^DC?DC)` <- `Pr(Y|?R^?DC?DC)` <- `Pr(Y|?R^dose)`(compra,nddd_Dinc,year)

    if (dose == 'DCDC') {
      p <- `Pr(Y|?R^DCDC)`*(1-`Pr(R|DCDC)`)+`Pr(R|DCDC)`
    } else if (dose == '?DCDC') {
      p <- `Pr(Y|?R^?DCDC)`*(1-`Pr(R|?DCDC)`)+`Pr(R|?DCDC)`
    } else if (dose == 'DC?DC') {
      p <- `Pr(Y|?R^DC?DC)`*(1-`Pr(R|DC?DC)`)+`Pr(R|DC?DC)`
    } else if (dose == '?DC?DC') {
      p <- `Pr(Y|?R^?DC?DC)`*(1-`Pr(R|?DC?DC)`)+`Pr(R|?DC?DC)`
    }
    return(p)
  }

  # ---------------------------------------------------------------------

  `Pr(R|dose)` <- function(compra,nddd,year) {

    `Pr(Rj|dose)` <- function(m,m2,nddd) {
      pmin(pmax(0,nddd-(1:m2))/m,1)
    }


    get_mcum <- function(m,m2,year) {
      dt <- data.table(m,m2,
                       card_m=days_in_month2(m,year),
                       card_m2=days_in_month2(m2,year))
      dt[,.(cum_m2=sum(card_m2)),.(m,card_m)]
    }
    mes_ini <- match(TRUE,compra)   # mes_ini <- which.max(compra)

    m <- last_m_len(compra) %>% .[!is.na(.)]
    m2 <- (min(m)+1):length(compra)

    dt_mcum <- get_mcum(m,m2,year)

    nddd_pos <- which(!is.na(nddd))
    dt_mcum <- dt_mcum[
      data.table(m=nddd_pos,nddd=nddd[nddd_pos]),on='m',nomatch=0L]
    vec_Pr_R_dose <-
      mapply(function(x,y,nddd) `Pr(Rj|dose)`(x,y,nddd),
             x=dt_mcum$card_m,y=dt_mcum$cum_m2,nddd=dt_mcum$nddd) %>% unlist

    vec_0_p <- rep(0L,sum(Days_in_month(1:mes_ini,year[1])))
    vec_Pr_R_dose <- c(vec_0_p,vec_Pr_R_dose)

    return(vec_Pr_R_dose)

  }

  # -------------------------------------------------------------------

  `Pr(Y|?R^dose)` <- function(compra,nddd,year) {
    len_compra <- length(compra)
    p0 <- vector(mode = 'list',length = len_compra)
    rep.year <- rep(year,each=12)
    for (i in 1:len_compra) {
      p0[[i]] <- `Pr(Yj|?R^dose)`(i,nddd[i],compra[i],year)
    }
    p0 <- unlist(p0)
    return(p0)
  }

# -------------------------------------------------------------------
# main function
# -------------------------------------------------------------------

  if (!length(nddd_DC)>1L) {
    nddd_DC <- rep(nddd_DC,length(compra))
    nddd_Dinc <- rep(nddd_Dinc,length(compra))
  }

  `Pr(DCDC)` <- p_bona^2
  `Pr(?DCDC)` <- `Pr(DC?DC)` <- p_bona*(1-p_bona)
  `Pr(?DC?DC)` <- (1-p_bona)^2

  `Pr(Y|DCDC)` <- `Pr(Y|dose)`(compra,nddd_DC,nddd_Dinc,p_bona,dose='DCDC',year)
  `Pr(Y|?DCDC)` <- `Pr(Y|dose)`(compra,nddd_DC,nddd_Dinc,p_bona,dose='?DCDC',year)
  `Pr(Y|DC?DC)` <- `Pr(Y|dose)`(compra,nddd_DC,nddd_Dinc,p_bona,dose='DC?DC',year)
  `Pr(Y|?DC?DC)` <- `Pr(Y|dose)`(compra,nddd_DC,nddd_Dinc,p_bona,dose='?DC?DC',year)

  `Pr(Y|DCDC)`*`Pr(DCDC)` + `Pr(Y|?DCDC)`*`Pr(?DCDC)` +
    `Pr(Y|DC?DC)`*`Pr(DC?DC)` + `Pr(Y|?DC?DC)`*`Pr(?DC?DC)`
}

