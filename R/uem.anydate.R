
uem.anydate = function(x){
  slength = stringr::str_length(x)
  iY = isYear(x)
  if(Mode(slength)==8 & iY) x = anydate(as.character(x))
  if(Mode(slength)==6 & iY) x = x %>% paste0(.,"01") %>% anydate
  return(x)
}
