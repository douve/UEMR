

idp.finder = function(x,sidp){
  if(is.data.frame(x)) return(x %>% filter(idp%in%grep(paste0('^',sidp),.$idp,value=TRUE)))
  if(!is.data.frame(x)) grep(paste0('^',sidp),x,value=FALSE)
}
