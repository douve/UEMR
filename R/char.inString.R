char.inString = function(string,chars){
  stringr::str_detect(string,chars) %>% all(.==TRUE)
}
