

# This functions assumes that the input data has four columns: id, start date, end date, drug/agr:

smoothy_xl <- function(data,start.date,end.date,size = NULL, ncores = parallel::detectCores() - 1, diff = FALSE){

  if(is.null(size)) stop("Please provide a value for 'size' argument.")
  if(dim(data)[2]!=4) stop("Please provide a valid input data")

  # rename:
  names(data) = c('id','dat','edat','drug')

  # starting date:
  s0 = Sys.time()

  # create chunks:
  niter <- n_distinct(data$id)
  chunks <- ceiling(niter/size)
  inds <- split(seq_len(niter), sort(rep_len(seq_len(chunks),
                                             niter)))

  # parallel - cores and socket:
  n.cores <- parallel::detectCores() - 1
  cl <- snow::makeSOCKcluster(n.cores)
  doSNOW::registerDoSNOW(cl)

  # progress bar:
  pb <- utils::txtProgressBar(min = 1, max = chunks, style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb,n)
  opts <- list(progress = progress)

  # path to temporal directory:
  tmp.path = tempdir()

  l <- foreach(c = 1:chunks,
               .packages = c("Kendall", "smoothy", "data.table", "anytime", "dplyr"),
               .options.snow = opts,
               .multicombine = F) %dopar% {

                 chunk.id <- unique(data$id)[inds[[c]]]

                 # run the workflow in each individual from the chunk:
                 df <- filter(data, id %in% chunk.id)

                 # 1) parse data:
                 structured_df <- smooth_parse(
                   id = df$id,
                   start_date = df$dat,
                   end_date = df$edat,
                   drug = df$drug,
                   study_from = start.date,
                   study_to = end.date
                 )

                 # 2) smooth algorithm:
                 width <- 61
                 smoothed <- smooth_algorithm(
                   id = structured_df$id,
                   treatment = structured_df$treatment,
                   day = structured_df$day,
                   N = structured_df$N,
                   width = width
                   )

                 # 3) deparse data (original format):
                 deparsed_smoothed <- smooth_deparse(
                   smoothed$id,
                   smoothed$day,
                   smoothed$smoothed_treatment
                   )

                 # 4) Per patient changes due to smooth algorithm:
                 if(diff){

                   # Calculate differences by patient mapping with the group_map function:
                   aux <- smoothed %>%
                     group_by(id) %>%
                     group_map(~ smooth_diff(.$treatment,.$smoothed_treatment)) %>%
                     bind_rows(.id = "group_id") %>%
                     data.frame

                   # Format output and filter global, exposure period:
                   aux <- aux %>%
                     mutate(percentage_of_change = round(proportion_of_change*100,2)) %>%
                     filter(type%in%c('Global','Exposure_period')) %>%
                     mutate(type = factor(type,levels=c('Global','Exposure_period'),
                                          labels=c('total_change','exposure_change')))
                   # add 'id' and reshape:
                   aux <- aux %>%
                     left_join(data.frame(id=unique(smoothed$id),group_id = as.character(seq(1,n_distinct(smoothed$id))))) %>%
                     reshape2::dcast(id~type,value.var='percentage_of_change')

                   # attach to deparsed_smoothed dataframe:
                   deparsed_smoothed <- left_join(
                     deparsed_smoothed,
                     aux
                   )

                   rm(aux)

                 }

                 # Save chunk output to a temporary folder:
                 saveRDS(deparsed_smoothed,paste0(tmp.path,"/chunk_",c,".rds"))

                 rm(df,structured_df,smoothed,deparsed_smoothed);gc()

               }


  # close sockets:
  close(pb)
  snow::stopCluster(cl)

  # Time to finish the process:
  t0 = Sys.time() - s0
  cat("\n The process finished in", round(t0), units(t0)) # The process finished in 16 hours

  # Import and combine all chunks into a single data.frame:
  rds_files <- list.files(tmp.path, pattern = "chunk_", full.names = TRUE)
  all_chunks <- bind_rows(lapply(rds_files, readRDS))

  return(all_chunks)

}
