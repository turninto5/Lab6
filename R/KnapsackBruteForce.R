#' brute_force_knapsack
#'
#' @description This class uses the Kolada API
#' @param x data.frame x with two variables v and w  
#' @param W The variable is the knapsack size
#' @return returns the maximum knapsack value and which elements
#' @examples
#' instance <- bf()
#' @export knapsack_brute_force
brute_force_knapsack <- function(x = NA, W = NA){
    stopifnot(is.data.frame(x))
    stopifnot("v" %in% colnames(x))
    stopifnot("w" %in% colnames(x))
    stopifnot(is.numeric(W)& W>0)
    n <- length(x$v) 
    subsets <- getAllSubsets(n)
    maxSubset <- list(value = 0, elements = c())
    for(subset in subsets){
      totalWeight <- sum(x$w[subset])
      if(totalWeight>W){
        next
      }
      totalValue <- sum(x$v[subset])
      if(totalValue>=maxSubset$value){
        maxSubset$value <- totalValue
        maxSubset$elements <- subset
      }
    }
    # print("Weight")
    # print(sum(x$w[maxSubset$elements]))
    # print(x$v[maxSubset$elements])
    # optimal solution 1
    # print(x$w[c(5,8)])
    # print(x$v[c(5,8)])
    # optimal solution 2
    # print(x$w[c(3,8)])
    # print(x$v[c(3,8)])
    return(maxSubset)
}


#' @description Helper function getAllSubsets(n), returns all subsets of a value n
#' @param n integer
#' @return list of all subsets
getAllSubsets <- function(n) {  
  subsets <- list()
  for (i in 0:(2^n - 1)) {
    bits <- as.integer(intToBits(i))[1:n]
    subset <- which(bits == 1)
    subsets[[i + 1]] <- subset
  }
  return(subsets)
}