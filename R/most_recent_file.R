most_recent_file <- function(path,pattern){

  files <- list.files(path = path, pattern = pattern, full.names = TRUE, recursive = FALSE)

  if (length(files) == 0) {
    return(NULL)
  }

  ordered_files <- files[order(file.info(files)$mtime, decreasing = TRUE)]
  most_recent_file <- ordered_files[1]

  return(most_recent_file)

}
