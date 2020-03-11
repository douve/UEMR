
smooth.chart = function(data,idpvar = 'idp',yvar = 'drugs',start.interval='dat',end.interval='dbaixa',order=TRUE,
                        col='#336b87',date_breaks='6 months',title=''){

  s = data.frame(idp=data[,idpvar],drugs = data[,yvar],dat=data[,xvar],edat = data[,exvar])
  s$drugs = as.character(s$drugs)

  if(order) yorder = unique(s$drugs)[order(nchar(unique(s$drugs)),unique(s$drugs))]

  ggplot(s,aes(x=dat,y=drugs))+
    facet_grid(idp~.)+
    scale_x_date(date_breaks=date_breaks)+
    scale_y_discrete(limits=yorder)+
    geom_segment(aes(x=dat,xend=edat,y=drugs,yend=drugs),size=2,alpha=0.85,col=col)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 50, hjust = 1),
          axis.text.y = element_text(size=6))+
    ylab("")+ xlab("")+
    labs(col="")+
    ggtitle(title)+
    theme(legend.position="none")
}
