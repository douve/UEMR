

## sidiap.import function

mfint = function(x){
  table(x) %>% data.frame %>% arrange(desc(Freq)) %>% .[1,1] %>% as.character() %>% as.numeric()
}

Mode <- function(x) {
  ux <- unique(x)
  ux <- ux[!is.na(ux)]
  ux[which.max(tabulate(match(x, ux)))]
}

isYear = function(x){
  stringr::str_extract(x,'[[:digit:]][[:digit:]]') %in% c("19","20",NA) %>%
    all(na.rm=T)
}

uem.anydate = function(x){
  slength = stringr::str_length(x)
  iY = isYear(x)
  if(Mode(slength)==8 & iY) x = anydate(as.character(x))
  if(Mode(slength)==6 & iY) x = x %>% paste0(.,"01") %>% anydate
  return(x)
}

# Use fread configured to SIDIAP databases:
sidiap.import = function(x,sep="|",...){

  data.table::fread(x,sep=sep,...) %>%
    mutate_if(.,is.integer,uem.anydate)

}
