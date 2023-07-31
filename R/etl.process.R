

# etl.process: Process to transform prescription or dispensation --------

#' @param data: Input data.frame
#' @param id.var: Name of identifier column
#' @param dat.var: Name of start date column
#' @param end,dat.var: Name of end date column
#' @param cod.var: Name of cod column
#' @param agr.var: Name of agr column
#' @param select.drugs: Character vector with the names of drugs or agr to transform
#' @param start: Initial date of the study
#' @param end: End date of study
#' @param n.cores: Number of cores tu use during parallelization
#' @param cod: which one to analyze, atc codes or drug aggregation?

etl.process = function(data,id.var = 'idp',dat.var = 'dat',end.dat.var = 'dbaixa',
                       cod.var = 'cod',agr.var='agr',select.drugs,start,end,
                       cod=FALSE,
                       n.cores=parallel::detectCores() - 1){

  options(warn=-1)

  # std. dataset_
  aux = data.frame(idp=data[,id.var],
                    dat=data[,dat.var],dbaixa=data[,end.dat.var],
                    cod = data[,cod.var],agr=data[,agr.var])

  # - Individuals to process_
  id = unique(aux$idp)

  # - Drugs to align_
  select.drugs=select.drugs

  # - Configure parallelization_
  n.cores = n.cores
  niter <- n_distinct(id)

  cl <- snow::makeSOCKcluster(n.cores) # make cluster
  doSNOW::registerDoSNOW(cl)           # register into cluster

  # - Progress bar_
  pb <- utils::txtProgressBar(min=1, max=niter, style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  comb = 'rbind.data.frame'


  # - alignment parallelized by individual_
  s = Sys.time()
  pETL = foreach(i = 1:niter,
                 .packages = c("Kendall",'UEMR','data.table','anytime','dplyr'),
                 .options.snow = opts,
                 .combine = comb,
                 .multicombine = F) %dopar% {

                   gl = expand.grid(idp = id[i], dat = seq(from = start,
                                                           to = end, by = "day"))
                   a = aux %>% filter(idp == id[i]) %>%
                     mutate(dat = ifelse(dat < start, start, dat),
                            dat = anytime::anydate(dat),
                            dif = dbaixa - dat) %>% filter(dif > 0) %>% left_join(gl,.) %>%
                     select(idp, dat, dbaixa, agr, cod)

                   a.etl = alignment.ETL(a, select.drugs = select.drugs,cod = cod)

                   # Exists one column for each agr/cod in select.drugs? If not create the column (makes foreach .combine easier)
                   setd = setdiff(select.drugs,colnames(a.etl))
                   if(length(setd)>0){
                     m = matrix(nrow=nrow(a.etl),ncol=length(setd)) %>% data.frame
                     colnames(m)=setd

                     a.etl = cbind.data.frame(a.etl,m)
                   }
                   return(a.etl %>% select(idp,dat,dbaixa,nd,drugs,th,select.drugs))

                 }

  e = Sys.time()
  t = e-s
  cat('\n The etl finished in',round(t),units(t))
  close(pb) # close the cluster
  snow::stopCluster(cl)

  # return
  pETL[is.na(pETL)] = FALSE
  return(pETL %>% select(idp,dat,dbaixa,nd,th,drugs,everything()))

}

