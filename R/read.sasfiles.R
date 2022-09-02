
# load sas files based on a pattern name into R environment:
# Add sas labels to each variable:
.do_fmt <- function(x, fmt, encoding='utf-8') {
  lbl <- if (!missing(fmt))
    unlist(unname(fmt)) else attr(x, 'labels')

  if (!is.null(lbl)){
    x = tryCatch(names(lbl[match(unlist(x), lbl)]),
                 error = function(e) {
                   message(sprintf('formatting failed for %s', attr(x, 'label')),
                           domain = NA)
                   x
                 })
    Encoding(x) = encoding
    x
  } else x
}

read.sasfiles = function(pattern=''){
  lf = list.files(data.path,file.pattern,full.names = T)
  sas.list = lapply(1:length(lf),function(i){

    aux = read_sas((lf[i])) %>%
      lapply(.do_fmt) %>% bind_cols()

  })
  names(sas.list) = lf %>% basename %>% str_remove_all('.sas7bdat|a_')
  list2env(sas.list,envir = .GlobalEnv)
  rm(sas.list);uem.gc()

}
