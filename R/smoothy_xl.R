


smoothy_xl <- function(data,size = NULL, ncores = parallel::detectCores() - 1, diff = TRUE){
  
  if(is.null(size)) stop("Please provide a value for 'size' argument.")
  
  if(diff){
    
    my_data <- data
    
    n.cores <- ncores
    niter <- n_distinct(my_data$id)
    chunks <- ceiling(niter/size)
    
    inds <- split(seq_len(niter), sort(rep_len(seq_len(chunks),
                                               niter)))
    for (i in 1:chunks) {
      
      cat("chunk:", i, "\n")
      chunk.id <- unique(my_data$id)[inds[[i]]]
      chunk.data <- my_data %>% filter(id %in% chunk.id)
      chunk.niter <- length(chunk.id)
      
      doMC::registerDoMC(n.cores)
      getDoParWorkers()
      
      comb <- "rbind.data.frame"
      echunk <- foreach(c = 1:chunk.niter,
                        .packages = c("Kendall", "smoothy", "data.table", "anytime", "dplyr"),
                        # .options.snow = opts,
                        .combine = comb, .multicombine = F) %dopar% {
                          
                          
                          # run the workflow in each individual from the chunk:
                          df <- filter(my_data, id == chunk.id[c])
                          
                          # 1) parse data:
                          structured_df <- smooth_parse(
                            id = df$id,
                            start_date = df$start_date,
                            end_date = df$end_date,
                            drug = df$drug,
                            study_from = "1970-01-01",
                            study_to = "1975-01-01"
                          )
                          
                          # 2) smooth algorithm:
                          id <- structured_df$id
                          treatment <- structured_df$treatment
                          day <- structured_df$day
                          N <- structured_df$N
                          width <- 61
                          
                          smoothed <- smooth_algorithm(id = id, treatment = treatment, day = day, N = N, width = width)
                          
                          # 3) calculate how algorithm is changing the raw data:
                          diff <- smooth_diff(treatment = smoothed$treatment, smoothed_treatment = smoothed$smoothed_treatment)
                          
                          # 4) deparse data (original format):
                          deparsed_smoothed <- smooth_deparse(smoothed$id, smoothed$day, smoothed$smoothed_treatment)
                          
                          # 4.1) add differences:
                          diff$proportion_of_change <- round(diff$proportion_of_change*100,2)
                          diff$id <- deparsed_smoothed$id[1]
                          diff <- diff %>% filter(type%in%c('Global','Expousure period'))
                          
                          res <- left_join(
                            deparsed_smoothed,
                            reshape2::dcast(diff,id~type,value.var='proportion_of_change')
                          )
                          
                          return(res)
                          
                        }
      
      saveRDS(echunk, paste0(tempdir(),"/chunk_", i,".rds"))
    }
    
    # Combine all chunks into one single data.frame:
    rds_files <- list.files(tempdir(), pattern = "chunk_", full.names = TRUE)
    all_df <- bind_rows(lapply(rds_files, readRDS))
    
    return(all_df)
    
  }else{
    
    
    
  }

  
}