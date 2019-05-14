

# smooth.process2: smooth process for prescription or dispensations --------

#' @param data: Input data.frame
#' @param n.cores: Number of cores tu use during parallelization 
#' @param Wt: smooth window in days
#' @param st: minimum splitting days between prescriptions or dispensations

smooth.process2 = function(data,n.cores=1,Wt=30,st=60){
  
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
                     
                     smooth.algorithm2 = function(x,select.drugs,Wt,st,id){
                       
                       MaxTable <- function(x){
                         dd <- unique(x) %>% na.omit()
                         dd[which.max(tabulate(match(x,dd)))]
                       }

                       select.drugs = select.drugs
                       Wt = Wt
                       st = st
                       id = id
                       
                       # Calculem els subsets on suavitzarem:
                        # - Smooth comença i acaba en els períodes registrats de prescripció/facturació, no es tenen en compte el '0' inicial ni final
                        # - Es subdivideix l'individu per tractaments a partir del paràmetre st
                        # - A cada tractament es suavitza sota el paràmetre Wt
                        # - Si dins d'una finestra, el pattern guanyador és '0' es queda l'observat
                       
                       aux = x %>% mutate(long = dbaixa-dat+1,
                                          n=1:nrow(.),
                                          s = cumsum(ifelse((drugs=='0' & long>=st)|(drugs=='0' & n==nrow(.)),T,F)),
                                          n=as.numeric(ifelse(drugs=='0' & n==1,F,T)),
                                          s=n+s) %>% select(-n)   

                       v = data.frame(seq=seq,
                                      t=rep(aux$drugs, aux$long),
                                      l=rep(aux$long, aux$long),
                                      s=rep(aux$s, aux$long))
                       
                       
                       # split dataframe i suavizar en cada subset:
                       S = split(v,v$s)
                       
                       # smooth:
                       df = lapply(1:length(S),function(i){
                         
                         a = S[[i]] %>% filter(!(t=='0' & l>st)) %>% mutate(t=as.character(t))
                         
                         if(nrow(a)!=0){
                           
                           # Most frequent pattern as we move the window:
                           mcin = lapply(1:length(a$t), function(j) {
                             res = MaxTable(a[j:(Wt + j),'t'])
                             if(res=='0'){
                               res = a[j:(Wt + j),'t']
                             } else{
                               res = rep(res, Wt)
                             }
                             res = rev(res)
                           }) %>% do.call(rbind, .)
                           
                           # most frequent drug each day:
                           # - create an indicator for all diagonals in the matrix
                           d <- row(mcin) - col(mcin)
                           
                           # - use split to group on these values
                           mcin = split(mcin, d)
                           
                           # - max value at each diagonal
                           sm.v = lapply(1:length(mcin),function(k){
                             MaxTable(mcin[[k]])
                           }) %>% do.call(c,.)
                           
                           return(data.frame(seq = a$seq, v = a$t, sm.v = sm.v[1:length(a$t)]))
                           
                         }
                         
                       }) %>% bind_rows()
                       
                       # post-process:
                       # - add entire study temporal window
                       df = data.frame(idp = id, seq = seq) %>% left_join(df)   
                       df = df %>% mutate_if(is.factor,as.character)
                       df[is.na(df)] = '0'
                       
                       df = df %>% mutate(v = v, sm.v = sm.v,g.sm.drug = rleid(sm.v))
                       # - aggregate by treatments (including No treatment)
                       df = df %>% mutate(pchange = 100 - perc(sum(sm.v == v), nrow(df))) %>% 
                         group_by(g.sm.drug) %>% 
                         summarise(idp = first(idp), 
                                   dat = first(seq), dbaixa = last(seq), sm.drug = first(sm.v), 
                                   pchange = first(pchange)) %>% select(-g.sm.drug) %>% 
                         ungroup
                       #- columns per drug
                       df = lapply(1:length(select.drugs), function(s) {
                         return(data.frame(n = 1:nrow(df),
                                           drug = select.drugs[s],
                                           value = stringr::str_detect(df$sm.drug,
                                                                       paste0('^',select.drugs[s],'$','|','\\+',
                                                                              select.drugs[s],'|',select.drugs[s],'\\+'))))
                       }) %>% bind_rows() %>% dcast(., n ~ drug, value.var = "value") %>% 
                         select(-n) %>% mutate(nd = rowSums(.),
                                               th = ifelse(nd > 1, "Polytherapy",
                                                           ifelse(nd == 1, "Monotherapy",0))) %>% 
                         bind_cols(df, .)
                       # - % of change per drug
                       t.agr = lapply(1:length(select.drugs),function(i){
                         
                         nvar = select.drugs[i]
                         
                         a = df %>% mutate(long = dbaixa-dat+1) %>% data.frame
                         b = aux %>% mutate(long = dbaixa-dat+1)
                         
                         gplot = data.frame(idp = first(aux$idp),agr = nvar,
                                            fae = rep(as.numeric(b[,nvar]),b$long),
                                            fas= rep(as.numeric(a[,nvar]),a$long))
                         gplot = gplot %>% mutate(pchange =  100 - perc(sum(fae == fas),nrow(gplot)))
                         gplot$pchange = ifelse(is.nan(gplot$pchange),0,gplot$pchange)
                         gplot = gplot %>% group_by(idp) %>% summarise_all(funs(first)) %>% select(idp,agr,pchange)
                         return(gplot)
                         
                         
                       }) %>% bind_rows()
                       t.agr = t.agr %>% dcast(.,idp~agr,value.var='pchange')
                       colnames(t.agr)[-1] = paste0('pc.',colnames(t.agr)[-1])
                       
                       # Return smoothed and processed individual:
                       df = left_join(df,t.agr) %>% arrange(dat) %>% select(idp,dat,dbaixa,sm.drug,select.drugs,everything())
                       return(df)
                       
                     }
                     
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
