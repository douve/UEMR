

# Plot etl and smooth process:
longitudinal.barplot = function(data,idpvar = 'idp',
                               yvar = 'drugs',
                               start.interval='dat',end.interval='dbaixa',order=TRUE,
                               col='#2a3132',date_breaks='6 months',title=''){

  s = data.frame(idp=data[,idpvar],drugs = data[,yvar],
                 dat=data[,start.interval],dbaixa = data[,end.interval])
  s$drugs = as.character(s$drugs)

  if(order) yorder = unique(s$drugs)[order(nchar(unique(s$drugs)),unique(s$drugs))]

  p = ggplot(s,aes(x=dat,y=drugs))+
    geom_segment(aes(x=dat,xend=dbaixa,y=drugs,yend=drugs),size=2,alpha=0.85,col=col)+
    theme_bw()+
    scale_x_date(date_breaks=date_breaks)+
    scale_y_discrete(limits=yorder)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          legend.position="none")+
    ylab("")+ xlab("")+
    ggtitle(paste0(title))

  p

}
