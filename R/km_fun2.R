km_fun2 = function(data,time,event,group,plot=TRUE,conf.int = TRUE,palette,xmax=NULL,breaks=30,facet=NULL,
                   pval.coord=c(1,0.25),axis.text.size=11,...){

  aux = data.frame(data)
  if(is.null(xmax)) xmax = max(aux[,time],na.rm=T)
  form = as.formula(paste0('Surv(',time,',',event,') ~',group))
  km = survminer::surv_fit(form,data=aux)
  if(!is.null(facet)) km.list = survminer::surv_fit(form,data=aux,group.by=facet)


  if(plot){
    if(!is.null(facet)){
      p <- survminer::ggsurvplot(km,data=aux,pval = TRUE, conf.int = conf.int,
                                 palette=palette,xlim=c(0,xmax),facet.by=facet,pval.coord=pval.coord,...)
      p <- p + scale_x_continuous(breaks = seq(0, xmax, breaks), limits=c(0,xmax),labels=1:length( seq(0, xmax, breaks))) +
        theme(axis.text.x = element_text(size = axis.text.size,angle = 45,hjust=1))
    } else{
      p <- survminer::ggsurvplot(km,data=aux,pval = TRUE, conf.int = conf.int,
                                 palette=palette,xlim=c(0,xmax),pval.coord=pval.coord,...)
      p <- p$plot + scale_x_continuous(breaks = seq(0, xmax, breaks), limits=c(0,xmax),labels=1:length( seq(0, xmax, breaks))) +
        theme(axis.text.x = element_text(size = axis.text.size,angle=45,hjust=1))
    }
    print(p)
  }

  if(!is.null(facet)){
    aux = left_join(survminer::surv_median(km.list,combine=T),survminer::surv_pvalue(km.list,combine=T) %>% select(id,pval))
    aux = aux %>%
      group_by(id) %>%
      mutate_at(ncol(.), funs(replace(., duplicated(.), NA)))
    names(aux) = c('Strat','Group','Median Survival','IC 2.5','IC 97.5','P value')
    aux$Strat = aux$Strat %>% stringr::str_remove(group) %>% stringr::str_remove('::')
  }else{
    n=survminer::surv_median(km) %>% nrow
    median = survminer::surv_median(km)
    pval = survminer::surv_pvalue(km)[2]
    aux = cbind.data.frame(median,pval)
    aux$pval[2:n] = NA
    names(aux) = c('Group','Median Survival','IC 2.5','IC 97.5','P value')
  }

  return(data.frame(aux))

}
