mfint = function(x){
  table(x) %>% data.frame %>% arrange(desc(Freq)) %>% .[1,1] %>% as.character() %>% as.numeric()
}
