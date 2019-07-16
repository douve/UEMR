isDate = function(x){
  stringr::str_extract(x,'[[:digit:]][[:digit:]]') %in% c("19","20") %>% all(na.rm=T)
}
