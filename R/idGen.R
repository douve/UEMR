##- idGen
idGen = function(data,project.name,N=NULL,prefix){

  # unique seed created from project.name_
  pnn = digest::digest(project.name) %>%
    stringr::str_replace_all(.,'[[:alpha:]]','') %>%
    stringr::str_sub(start = 1,end = 8) %>% as.numeric()

  set.seed(pnn)

  # # set prefix of ids_
  # if(is.null(prefix))

  if(is.null(N)) N=nrow(data)

  aux = data %>% sample_n(size=N) %>%
    mutate(id = paste0(prefix,"_",formatC(seq(1:n()), width = 5, flag = '0'))) %>%
    select(id,everything())
  # write_csv(aux,path = paste0(cessions_path,project.name,'/Code GCAT/',
  #                             project.name,'_ids_conversor_INTERN.csv'))

  return(aux)

}
