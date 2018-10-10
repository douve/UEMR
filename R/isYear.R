isYear = function(x){
  stringr::str_extract(x,'[[:digit:]][[:digit:]]') %in% c("19","20",NA) %>%
    all(na.rm=T)
}
