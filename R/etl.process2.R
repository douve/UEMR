

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
#' @param cod: which one to analyze, atc codes or drug aggregation?
#' @param n.cores: Number of cores tu use during parallelization
#' @param chunks: Number of chunks during parallelization. More or less nrow/#ind-per-chunk

etl.process2 = function(data,id.var = 'idp',dat.var = 'dat',end.dat.var = 'dbaixa',
                       cod.var = 'cod',agr.var='agr',select.drugs,start,end,
                       cod=FALSE,
                       n.cores=parallel::detectCores() - 1,
                       chunks=NULL){

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

  if(!is.null(chunks)){

    inds <- split(seq_len(niter), sort(rep_len(seq_len(chunks), niter)))   # - Create chunks

    s0 = Sys.time()

    for(i in 1:chunks){
      cat('chunk:',i,'\n')

      # select individuals from first batch
      chunk.id = unique(aux$idp)[inds[[i]]]
      chunk.data = aux %>% filter(idp%in%chunk.id)
      chunk.niter <- length(chunk.id)

      # - alignment parallelized by individual_
      cl <- snow::makeSOCKcluster(n.cores) # make cluster
      doSNOW::registerDoSNOW(cl)           # register into cluster

      # - Progress bar_
      pb <- utils::txtProgressBar(min=1, max=(chunk.niter+1), style=3)
      progress <- function(n) utils::setTxtProgressBar(pb, n)
      opts <- list(progress=progress)
      comb = 'rbind.data.frame'

      # - Parallelization using foreach_
      s = Sys.time()
      echunk = foreach(c = 1:chunk.niter,
                       .packages = c("Kendall",'UEMR','data.table','anytime','dplyr'),
                       .options.snow = opts,
                       .combine = comb,
                       .multicombine = F) %dopar% {

                         alignment.ETL = function(x,select.drugs=select.drugs,cod=FALSE){

                           # Return unique and non-overlapped dates:
                           uniqueDate = function(dat0,dat){
                             sapply(1:length(dat0),function(i){
                               seq(dat0[i],dat[i],by='days')
                             }) %>% unlist %>% unique %>%
                               as.Date(origin='1970-01-01')
                           }

                           # string dicotomous variables changed to logical:
                           stringToLogical = function(x){ifelse(is.na(x),FALSE,TRUE)}

                           # v is the list of cod/agr to process:
                           if(cod) {
                             v = grep(paste0(select.drugs,collapse = "|"),unique(x$cod) %>% as.character,value=T)
                             if(length(v)==0) stop('select.drugs is not a set of ATC codes')
                             x = x %>% mutate(study.drug=cod) %>% select(idp,dat,dbaixa,study.drug)
                           }else {
                             v = grep(paste0(select.drugs,collapse = "|"),unique(x$agr) %>% as.character,value=T)
                             if(length(v)==0) stop('select.drugs is not a set of ATC aggregators')
                             x = x %>% mutate(study.drug=agr) %>% select(idp,dat,dbaixa,study.drug)
                           }

                           # Add one column per each drug and fill it with the days prescribed (dbaixa to dat)_
                           # if(length(v)>0){

                           v = sort(v)
                           for(vi in 1:length(v)){

                             t = x %>% filter(study.drug==v[vi]) %>%
                               mutate(dat = ifelse(dat<start,start,dat),
                                      dat = anydate(dat))
                             au = data.frame(dat = uniqueDate(t$dat,t$dbaixa) %>% unique() %>% sort(),
                                             var=v[vi])
                             au = dcast(au,dat~var)
                             # colnames(au)[length(au)] = v[i]
                             x = left_join(x,au,by="dat")

                           }

                           agr.cols.x = grep(paste0(select.drugs,collapse = "|"),colnames(x))

                           # Calculate the number of drugs per day, and the type of therapy
                           aux = x %>% select_at(agr.cols.x) %>% mutate(nd = rowSums(!is.na(.)),
                                                                        drugs = apply(.,1,function(x) paste0(na.omit(x),collapse="+")))
                           aux$drugs = ifelse(aux$drugs=='',0,aux$drugs)
                           agr.cols.aux = grep(paste0(select.drugs,collapse = "|"),colnames(aux))
                           aux = aux %>% mutate_at(agr.cols.aux,stringToLogical)

                           x = bind_cols(x %>% select(-agr.cols.x,-study.drug),aux)

                           x = x %>% mutate(g.drugs = rleid(drugs),
                                            th = ifelse(nd>1,'Polytherapy',ifelse(nd==1,'Monotherapy',0))) %>%
                             group_by(g.drugs) %>%
                             mutate(dbaixa.drugs=last(dat)) %>%
                             summarise_all(funs(first)) %>%
                             mutate(dbaixa=dbaixa.drugs) %>%
                             select(-dbaixa.drugs,-g.drugs) %>% data.frame

                           return(x %>% arrange(dat))

                         }

                         gl = expand.grid(idp = chunk.id[c], dat = seq(from = start,
                                                                       to = end, by = "day"))
                         a = chunk.data %>% filter(idp == chunk.id[c]) %>%
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
      cat('\n The chunk finished in',round(t),units(t),'\n')
      close(pb) # close the cluster
      snow::stopCluster(cl)

      uniexport(echunk,'Robject',tempdir(),paste0(project_name,'_epresc_chunk_',i),date=TRUE)
      uem.gc()

      Sys.sleep(5)

    }

    pETL = lapply(1:chunks,function(i){
      unimport('Robject',tempdir(),paste0(project_name,'_epresc_chunk_',i,'_'))
    }) %>% bind_rows()

    e0 = Sys.time()
    t0 = e0-s0
    cat('\n The ETL finished in',round(t0),units(t0))
    uem.gc()

  }
  else{

    cl <- snow::makeSOCKcluster(n.cores) # make cluster
    doSNOW::registerDoSNOW(cl)           # register into cluster

    # - Progress bar_
    pb <- utils::txtProgressBar(min=1, max=(niter+1), style=3)
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

                     alignment.ETL = function(x,select.drugs=select.drugs,cod=FALSE){

                       # Return unique and non-overlapped dates:
                       uniqueDate = function(dat0,dat){
                         sapply(1:length(dat0),function(i){
                           seq(dat0[i],dat[i],by='days')
                         }) %>% unlist %>% unique %>%
                           as.Date(origin='1970-01-01')
                       }

                       # string dicotomous variables changed to logical:
                       stringToLogical = function(x){ifelse(is.na(x),FALSE,TRUE)}

                       # v is the list of cod/agr to process:
                       if(cod) {
                         v = grep(paste0(select.drugs,collapse = "|"),unique(x$cod) %>% as.character,value=T)
                         if(length(v)==0) stop('select.drugs is not a set of ATC codes')
                         x = x %>% mutate(study.drug=cod) %>% select(idp,dat,dbaixa,study.drug)
                       }else {
                         v = grep(paste0(select.drugs,collapse = "|"),unique(x$agr) %>% as.character,value=T)
                         if(length(v)==0) stop('select.drugs is not a set of ATC aggregators')
                         x = x %>% mutate(study.drug=agr) %>% select(idp,dat,dbaixa,study.drug)
                       }

                       # Add one column per each drug and fill it with the days prescribed (dbaixa to dat)_
                       # if(length(v)>0){

                       v = sort(v)
                       for(vi in 1:length(v)){

                         t = x %>% filter(study.drug==v[vi]) %>%
                           mutate(dat = ifelse(dat<start,start,dat),
                                  dat = anydate(dat))
                         au = data.frame(dat = uniqueDate(t$dat,t$dbaixa) %>% unique() %>% sort(),
                                         var=v[vi])
                         au = dcast(au,dat~var)
                         # colnames(au)[length(au)] = v[i]
                         x = left_join(x,au,by="dat")

                       }

                       agr.cols.x = grep(paste0(select.drugs,collapse = "|"),colnames(x))

                       # Calculate the number of drugs per day, and the type of therapy
                       aux = x %>% select_at(agr.cols.x) %>% mutate(nd = rowSums(!is.na(.)),
                                                                    drugs = apply(.,1,function(x) paste0(na.omit(x),collapse="+")))
                       aux$drugs = ifelse(aux$drugs=='',0,aux$drugs)
                       agr.cols.aux = grep(paste0(select.drugs,collapse = "|"),colnames(aux))
                       aux = aux %>% mutate_at(agr.cols.aux,stringToLogical)

                       x = bind_cols(x %>% select(-agr.cols.x,-study.drug),aux)

                       x = x %>% mutate(g.drugs = rleid(drugs),
                                        th = ifelse(nd>1,'Polytherapy',ifelse(nd==1,'Monotherapy',0))) %>%
                         group_by(g.drugs) %>%
                         mutate(dbaixa.drugs=last(dat)) %>%
                         summarise_all(funs(first)) %>%
                         mutate(dbaixa=dbaixa.drugs) %>%
                         select(-dbaixa.drugs,-g.drugs) %>% data.frame

                       return(x %>% arrange(dat))

                     }

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

  }

  # return
  pETL[is.na(pETL)] = FALSE
  return(pETL %>% select(idp,dat,dbaixa,nd,th,drugs,everything()))

}

