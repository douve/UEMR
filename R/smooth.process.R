

smooth.process = function(data,select.drugs,start,end,n.cores=1,Wt=30,new=TRUE){
  
  options(warn=-1)
  
  # - Individuals to process and Window parameter_
  id = unique(data$idp)
  Wt = Wt
  start = start
  end = end
  seq = seq(start,end,'day')  
  new = new
  
  # - Drugs to align_
  select.drugs=select.drugs
  
  # - Configure parallelization_
  niter <- n_distinct(id)             # iterations
  n.cores = n.cores
  cl <- snow::makeSOCKcluster(n.cores) # make cluster
  doSNOW::registerDoSNOW(cl)           # register into cluster
  
  # - Progress bar_
  pb <- utils::txtProgressBar(min=1, max=niter, style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  
  # - alignment parallelized by individual_
  s = Sys.time()
  spresc = foreach(i = 1:niter,
                   .packages = c('Kendall','UEMR','tidyverse','data.table'),
                   .options.snow = opts,
                   .combine = 'bind_rows',
                   .multicombine = T) %dopar% {
                     
                     # which algorithm
                     if(new){
                       smooth.algorithm = function(x,select.drugs,Wt,id){
                         MaxTable <- function(x){
                           dd <- unique(x) %>% na.omit()
                           dd[which.max(tabulate(match(x,dd)))]
                         }

                         aux = x
                         select.drugs = select.drugs
                         Wt = Wt
                         id = id
                         v = rep(aux$drugs, aux$long)
                         
                         # Most frequent pattern as we move the window:
                         mcin = lapply(1:length(v), function(j) {
                           res = MaxTable(v[j:(Wt + j)])
                           res = rep(res, Wt)
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
                         
                         df = data.frame(idp = id, seq = seq, v = v, sm.v = sm.v[1:length(v)])
                         df = df %>% mutate(v = as.character(v), sm.v = as.character(sm.v), 
                                            g.sm.drug = rleid(sm.v))
                         df = df %>% mutate(pchange = 100 - perc(sum(sm.v == 
                                                                       v), nrow(df))) %>% 
                           group_by(g.sm.drug) %>% 
                           summarise(idp = first(idp), 
                                     dat = first(seq), dbaixa = last(seq), sm.drug = first(sm.v), 
                                     pchange = first(pchange)) %>% select(-g.sm.drug) %>% 
                           ungroup
                         
                         df = lapply(1:length(select.drugs),function(s){
                           
                           return(data.frame(n = 1:nrow(df), drug = select.drugs[s], 
                                             value = grepl(select.drugs[s], df$sm.drug)))
                           
                           }) %>% 
                           bind_rows() %>% dcast(., n ~ drug, value.var = "value") %>% 
                           select(-n) %>% mutate(nd = rowSums(.), th = ifelse(nd > 1, "Polytherapy",
                                                                              ifelse(nd == 1, "Monotherapy",0))) %>% 
                           bind_cols(df, .)
                         
                         return(df)
                       }
                       }
                     else{
                       smooth.algorithm = function(x,select.drugs,Wt,id){
                           
                           # look at diagonals of the matrix:
                           diags <- function(m, type = c("sub", "super"), offset = 1) {
                             type <- match.arg(type)
                             FUN <-
                               if(isTRUE(all.equal(type, "sub")))
                                 `+`
                             else
                               `-`
                             m[row(m) == FUN(col(m), offset)] 
                           }
                           
                           # Variables:
                           aux = x
                           select.drugs = select.drugs
                           Wt = Wt
                           id = id
                           
                           v = rep(aux$drugs,aux$long)
                           
                           mcin = lapply(1:length(v),function(j){
                             
                             t = table(v[j:(Wt+j-1)])
                             res = which(t==max(t)) %>% names
                             
                             if(length(res)>1){
                               res = v[j:(Wt+j-1)]
                             }else{
                               res = rep(res,Wt)
                             }
                             
                           }) %>% do.call(rbind,.)
                           
                           mcin_rev = mcin[nrow(mcin):1,]
                           
                           sm.v = lapply(1:nrow(mcin_rev)-1,function(k){
                             
                             d = diags(mcin_rev,offset=k)
                             t = table(d)
                             res = which(t==max(t)) %>% names
                             
                             if(length(res)>1){
                               res = last(d)
                             }
                             return(res)
                             
                           }) %>% do.call(c,.)
                           
                           df = data.frame(idp=id ,seq=seq,v=v,sm.v = rev(sm.v))
                           df = df %>% mutate(v = as.character(v),
                                              sm.v = as.character(sm.v),
                                              g.sm.drug = rleid(sm.v))
                           
                           df = df %>% 
                             mutate(pchange =  100 - perc(sum(sm.v == v),nrow(df))) %>%
                             group_by(g.sm.drug) %>% summarise(idp=first(idp),
                                                               dat=first(seq),
                                                               dbaixa = last(seq),
                                                               sm.drug = first(sm.v),
                                                               pchange=first(pchange)
                             ) %>% select(-g.sm.drug) %>% ungroup
                           
                           
                           # Create logical variables per drug_
                           df = lapply(1:length(select.drugs),function(s){
                             
                             return(data.frame(n = 1:nrow(df),
                                               drug = select.drugs[s],
                                               value = grepl(paste0("^",select.drugs[s],"$"),df$sm.drug)))
                             
                           }) %>% bind_rows() %>% 
                             dcast(.,n~drug,value.var='value') %>% 
                             select(-n) %>% mutate(nd = rowSums(.),
                                                   th = ifelse(nd>1,'Polytherapy',ifelse(nd==1,'Monotherapy',0))) %>% 
                             bind_cols(df,.)
                           
                           
                           return(df)
                           
                         }
                     }

                     aux = data %>% filter(idp==id[i]) %>% mutate(long = dbaixa-dat+1)
                     
                     saux = smooth.algorithm(x=aux,select.drugs = select.drugs,Wt=Wt,id=id[i])
                     
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
