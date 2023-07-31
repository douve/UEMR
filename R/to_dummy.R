

# Change columns to values 0,1
to_dummy = function(x) ifelse(x==0 | is.na(x),0,1) %>% as.factor(.)
