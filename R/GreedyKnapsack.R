#' greedy_knapsack
#'
#' @description This Function solves the knapsack poblem by the greedy algorithm 
#' @param x data.frame x with two variables v and w
#' @param W The variable is the knapsack size
#' @return returns the maximum knapsack value and which elements
#' @examples
#' output <- greedy_knapsack(list(v <- c(1,2,3), w <- (4,5,6)), W = 10)
#' @export knapsack_brute_force
greedy_knapsack <- function(x = NA, W = NA){
    stopifnot(is.data.frame(x))
    stopifnot("v" %in% colnames(x))
    stopifnot("w" %in% colnames(x))
    stopifnot(is.numeric(W)& W>0)
    ratios <- x$v/x$w
    x <- cbind(x,ratios)
    x <- cbind(x,originalOrder = 1:length(x$ratios))
    x <- x[order(-x$ratios), ]

    maxSubset <- list(value = 0, elements = c())
    remainingCapacity <- W
    for (index in 1:nrow(x))
    {
      if(remainingCapacity == 0){
        break
      }
      if (x$w[index] <= remainingCapacity) {
            maxSubset$value <- maxSubset$value + x$v[index]
            maxSubset$elements <- c(maxSubset$elements, x$originalOrder[index])
            remainingCapacity <- remainingCapacity - x$w[index]  
     } else {
            # fraction <- remainingCapacity / x$w[index]
            # maxSubset$value <- maxSubset$value + x$v[index] * fraction  
            remainingCapacity <- 0 
    }
  }
  return(maxSubset)
}

