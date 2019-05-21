

# smooth.process2: smooth process for prescription or dispensations --------

#' @param data: Input data.frame
#' @param n.cores: Number of cores tu use during parallelization
#' @param Wt: smooth window in days
#' @param st: minimum splitting days between prescriptions or dispensations

smooth.process2 = function(data,
                           n.cores=parallel::detectCores()-1,
                           Wt=30,st=60){

  options(warn=-1)

  # - Individuals to process and Window parameter_
  id = unique(data$idp)
  Wt = Wt
  st = st
  start = min(data$dat)
  end = max(data$dbaixa)
  seq = seq(start,end,'day')

  # - Drugs to smooth_
  select.drugs = colnames(data)[(grep('drugs',colnames(data))+1):ncol(data)]

  # - Configure parallelization_
  niter <- n_distinct(id)             # iterations
  n.cores = n.cores
  cl <- snow::makeSOCKcluster(n.cores) # make cluster
  doSNOW::registerDoSNOW(cl)           # register into cluster

  # - Progress bar_
  pb <- utils::txtProgressBar(min=1, max=niter, style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)

  # - smooth parallelized by individual_
  s = Sys.time()
  spresc = foreach(i = 1:niter,
                   .packages = c('Kendall','UEMR','tidyverse','data.table'),
                   .options.snow = opts,
                   .combine = 'rbind.data.frame',
                   .multicombine = T) %dopar% {

                     aux = data %>% filter(idp==id[i]) %>% mutate(long = dbaixa-dat+1)

                     saux = smooth.algorithm2(x=aux,select.drugs = select.drugs,Wt=Wt,id=id[i],st=st)

                     return(saux)

                   }

  e = Sys.time()
  t = e-s
  cat('\n The smoothing process finished in',round(t),units(t))
  close(pb) # close the cluster
  snow::stopCluster(cl)

  # return
  return(spresc %>% select(idp,dat,dbaixa,nd,th,drugs = sm.drug,everything()))

}

