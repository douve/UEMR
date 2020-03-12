km_fun = function(data,time,event,group,plot=TRUE,conf.int = TRUE,palette){

  aux = data.frame(data)
  form = as.formula(paste0('Surv(aux[,time],aux[,event]) ~',group))
  km = survminer::surv_fit(form,data=aux)

  if(plot){
    p <- survminer::ggsurvplot(km,data=aux,pval = TRUE, conf.int = conf.int,palette=palette)
    print(p$plot)
  }

}
