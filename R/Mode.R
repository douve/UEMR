Mode = function(x,na.rm=FALSE){
  if(na.rm) useNA='no' else useNA='always'
  table(x,useNA=useNA) %>% data.frame %>% arrange(desc(Freq)) %>% .[1,1] %>% as.character() %>% as.numeric()
}
