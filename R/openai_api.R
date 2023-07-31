


library(httr)
library(jsonlite)

openai_api <- function(prompt, temperature = 0.5, max_tokens = 1024, n = 1, stop = NULL, echo = FALSE, api_key) {

  headers <- c("Content-Type"="application/json",
               "Authorization"=paste("Bearer", api_key))

  body <- list(
    # model = 'davinci',
    temperature = temperature,
    n=n,
    max_tokens = as.integer(max_tokens),
    prompt = prompt)

  if (!is.null(stop)) {
    body$stop <- list(stop = stop)
  }

  url <- "https://api.openai.com/v1/engines/davinci/completions"

  response <- POST(url,
                   add_headers(.headers=headers),
                   body = (body),
                   encode = "json")

  response_content <- content(response, as = "text", encoding = "UTF-8")

  result <- fromJSON(response_content)

  # if (n == 1) {
  #   return(result$choices[[1]]$text)
  # } else {
  #   return(sapply(result$choices, function(x) x$text))
  # }

  return(result)
}

