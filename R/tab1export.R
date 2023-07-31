


tab1export = function(vars,data,test=F,addOverall=T,missing=TRUE,nonnormal=NULL,
                      showAllLevels=T,printToggle=F,format='fp',smd=F,
                      save=F,path.output,filename='table1',date=TRUE,...){

  tab1 = CreateTableOne(vars=vars,data=data,test=test,addOverall=addOverall,...)
  tab1 = print(tab1,showAllLevels=showAllLevels, quote = F, noSpaces = TRUE,
               printToggle=printToggle,missing=missing,nonnormal=nonnormal,format=format,smd=smd)

  if(save){

    if(date) date=Sys.Date()

    tab1 %>%
      ## Save to a CSV file
      write.csv2(file = paste0(path.output,"/",filename,'_',date,".csv"))

  }else{
    return(tab1)
  }
}
