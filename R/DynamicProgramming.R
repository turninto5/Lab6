#' Dynamic Programming
#' @description
#' This functions provides a solution to solve knapsack via dynamic programming
#' @param x Parameter x is input, should be in type of data frame, which contains 2 vector weights and values of items.
#' @param W Parameter W is the capacity of the bag, should be numeric.
#' @param fast Parameter fast is in type of boolean and aims to improve performance via C++.
#' @return This function returns a list, which contains elements which are selected and the maximum value the bag contain under its capacity.
#' @examples
#' v <-c(2,2,6,5,4)
#' w <- c(6,3,5,4,6)
#' knapsack_objects <- randomData(data_length = 8)
#' outcome1 <- dynamicProgramming(x = as.data.frame(cbind(w, v)), W = 10,  fast = FALSE)
#' outcome2 <- dynamicProgramming(x = knapsack_objects, W = 3500, fast = FALSE)
#' @export dynamicProgramming
#' @name dynamicProgramming
#' @import Rcpp



dynamicProgramming <- function(x = NA, W = NA, fast = NA){
    stopifnot(is.logical(fast))
    stopifnot(is.data.frame(x))
    stopifnot("v" %in% colnames(x))
    stopifnot("w" %in% colnames(x))
    stopifnot(is.numeric(W))
    weight <- x$w
    value <- x$v
    ssq <- matrix(data = 0, nrow = length(weight) + 1, ncol = W + 1) # solution of sub-question

    for (i in 1:length(weight)){
      for (j in seq(from = W + 1, to = weight[i] + 1)){
        # we used profvis to analyse the lowest efficient part, and found this part can be replaced by C++
        if(fast){
          ssq[i + 1, j] <- updateStatus(ssq[i,j], ssq[i, j - weight[i]], value[i])
        }
        else{
          ssq[i + 1, j] <- max(ssq[i, j], ssq[i, j - weight[i]] + value[i])
        }

      }
    }
    elements <- vector()
    for (i in seq(from = nrow(ssq), to = 2)){
      if (ssq[i, W+1] > ssq[i-1, W+1]){
        elements <- append(elements, i-1)
        W <- W - weight[i-1]
      }
    }
    outcome <- list(value = max(ssq), elements = elements)
    return (outcome)
}




#' updateStatus
#' @description
#' This function aims to improve the performance in R implemented by C++.
#' @param not_load A numeric variable.
#' @param load A numeric variable.
#' @param value A numeric variable.
#' @return A numeric element.
#' @export

updateStatus <- cppFunction({
  'int updateStatus(double not_load, double load, double value){
                return std::max(not_load, load + value);
            }'
})


