


#' r_gpt() is an R function that allows the user to interact with the chat_gpt function in a more streamlined way.
#'
#' The function takes four arguments:
#' @param prompt - a character string that represents the user's request or question for the chatbot.
#' @param prompt0 - an optional character string that represents the initial prompt given to the chatbot. If this argument is not provided, the default prompt0 will be used.
#' @param verbose - a logical value that determines whether or not the chatbot's response will be printed to the console. If verbose is set to TRUE, the chatbot's response will be printed. If verbose is set to FALSE, the chatbot's response will not be printed.
#' @param n - an integer value that represents the number of responses the chatbot should generate.
#'
#' If prompt0 is not provided, the default prompt0 is "I want you to act as R programmer. I will ask you to write scripts based on my requests and you will reply the answer. I want you to only reply with the code, and nothing else. Do not write explanations. My request is".
#'
#' @return R code based on their requests using the chat_gpt function


r_gpt <- function(prompt,prompt0=NULL,verbose=TRUE,n=1){

  if(is.null(prompt0)) prompt0 = 'I want you to act as R programmer. I will ask you to write scripts based on my requests and you will reply the answer.I want you to only reply with the code, and nothing else. Do not write explanations. My request is '

  chat_gpt(paste0(prompt0,prompt),verbose=verbose,n=n)

}
