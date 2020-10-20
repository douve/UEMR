


uem.rm <- function() {
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  UEMR::uem.gc()
}
