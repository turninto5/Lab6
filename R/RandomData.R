#' RandomData
#' @description This file aims to generate input data for knapsack.
#' @name RandomData
#' @param data_length A numeric variable, default NA.
#' @examples
#' knapsack_objects <- randomData(data_length = 8)
#' @return A data frame who contains w for weights and v for values.
#' @export randomData
#' @importFrom stats runif
#'

randomData <- function(data_length = NA){
  stopifnot(is.numeric(data_length))
  RNGversion(min(as.character(getRversion()),"3.5.3"))

  ##old sampler used for backward compatibility
  ## suppressWarnings() can be used so that the above warning is not displayed
  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
  n <- 2000
  knapsack_objects <- data.frame(w=sample(1:3500, size = n, replace = TRUE),
                                 v=runif(n = n, 0, 10000))
  return(knapsack_objects[1:data_length, ])
}


