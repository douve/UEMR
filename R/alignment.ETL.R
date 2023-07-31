


# Concomitance algorithm: Process to transform prescription or dispensation --------

#' @param x: Input data.frame
#' @param select.drugs: Character vector with the names of drugs or agr to transform
#' @param cod: which one to analyze, atc codes or drug aggregation?


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
