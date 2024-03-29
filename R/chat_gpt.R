

#' Generate a response to a natural language prompt using the OpenAI GPT-3.5 Turbo language model
#'
#' This function sends a natural language prompt to the OpenAI API and returns a response generated
#' by the specified language model. The response can be used for a variety of tasks, such as generating
#' text, code snippets, or even conversation.
#'
#' @param prompt A character string containing the natural language prompt to send to the OpenAI API.
#' @param model A character string specifying the ID of the OpenAI language model to use for generating
#' the response. Default is "gpt-3.5-turbo", which is a high-performance model optimized for generating
#' text.
#' @param temperature A numeric value specifying the "creativity" of the response generated by the
#' language model. Higher values lead to more diverse and unpredictable responses, while lower values
#' lead to more predictable and "safe" responses. Default is 0.5.
#' @param max_tokens An integer value specifying the maximum number of tokens to include in the response
#' generated by the language model. Increasing this value can lead to longer and more detailed responses,
#' but may also increase the risk of generating nonsensical or irrelevant content. Default is 2048.
#' @param n An integer value controlling the number of responses to generate.
#' @param openai_api_key An optional character string containing the OpenAI API key to use for
#' authentication. If not specified, the function will attempt to retrieve the API key from the system
#' environment variable OPENAI_API_KEY.
#'
#' @return A character string containing the response generated by the OpenAI language model in
#' response to the specified prompt.
#'
#' @export
chat_gpt <- function(prompt, model = "gpt-3.5-turbo", temperature = 0.5,
                     max_tokens = 2048, n=1,openai_api_key = NULL,verbose=TRUE) {

  api_key <- if(!is.null(openai_api_key)) openai_api_key else Sys.getenv("OPENAI_API_KEY")

  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    httr::content_type_json(),
    encode = "json",
    body = list(
      model = model,
      temperature = temperature,
      n=n,
      max_tokens = as.integer(max_tokens),
      messages = list(list(
        role = "user",
        content = prompt
      ))
    )
  )

  # Check for errors or warnings
  if (response$status_code != 200) {
    message("Error: Failed to get a response from OpenAI. Status code: ", response$status_code)
    stop("OpenAI API request failed.")
  } else if (!is.null(response$headers$warning)) {
    message("Warning from OpenAI API: ", response$headers$warning)
  }

  # Parse response JSON
  response_json <- suppressMessages(jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = TRUE))

  # Extract text from response
  response_text <- response_json$choices$message$content

  # Return response
  if (verbose) cat(response_text) else return(response_text)

}


# api_key <- "your_openai_api_key"
# Sys.setenv(OPENAI_API_KEY = api_key)

#' How to ask to chatGPT for R code:
# The best way to ask for R code is to provide a clear and specific description of the task you want to accomplish,
# along with any relevant data or variables that are required.
# It is also helpful to specify any parameters or settings you would like to include in the code,
# such as data transformations, visualizations, or statistical analyses.
# Additionally, providing a desired output or format for the results can help ensure that the code meets
# your needs.

#' This is the main sentence to get a good answer from chatGPT:
# main.prompt = 'I want you to act as R programmer. I will ask you to write scripts based on my requests and you will reply the answer.I want you to only reply with the code, and nothing else. Do not write explanations. My request is'
#' ,and an example of how to make a request:
# "Can you provide R code to create a scatterplot of two variables, x and y, from a dataset called 'data.csv'? Please label the x-axis 'X-axis' and the y-axis 'Y-axis', and include a title for the plot. Thank you."
# chat_gpt(prompt =paste0(main.prompt,"'Can you provide R code to create a scatterplot of two variables, x and y, from a dataset called 'data.csv'? Please label the x-axis 'X-axis' and the y-axis 'Y-axis', and include a title for the plot. Thank you.'"))

# output:
# ```r
# library(ggplot2)
# data <- read.csv("data.csv")
# ggplot(data, aes(x = x, y = y)) +
#   geom_point() +
#   labs(x = "X-axis", y = "Y-axis", title = "Scatterplot of X and Y")
# ```

# Specifying non-default values for temperature and max_tokens
# chat_gpt("What is the R code to create a color palette of more than the maximum values that has the Spectral Palette?",
#          temperature = 0.7, max_tokens = 1024)

# Generating multiple responses
# chat_gpt("What is the R code to create a color palette of more than the maximum values that has the Spectral Palette? Provide only the code no explanaitions nor any other information", n = 5)

# save the answer and use it to get modify, extend or improve them:
# answer = chat_gpt(prompt =paste0(main.prompt,"'Can you provide R code to make webscrapping for the webpage idiapjgol.org?'"),verbose=F,n=1)
# chat_gpt(prompt =paste0(main.prompt,"'Can you add a code to download images from the scrapping you programmed in the last answer?'",answer))

#' Other examples:
# chat_gpt(prompt =paste0(main.prompt,"Can you give a R markdown code which makes a dummy dataset, select all numerical columns and analyses the association between variables using dendrogram and cluster techniques? The values must be normalized before conducting the clusterization. I want to show the results in the markdown using plotly for an interactive plot."))
# chat_gpt(prompt =paste0(main.prompt,"'What is the R code to separate one column into two using ',' as separator?'"),n=1)
