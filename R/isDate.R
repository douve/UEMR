isDate = function(x){
  stringr::str_extract(na.omit(x),'[[:digit:]][[:digit:]]') %in% c("19","20") %>% all()
}

