#' Dynamic Programming
#' @description
#' This functions provides a solution to solve knapsack via dynamic programming
#' @return This function returns the maximum value that can fit in a defined backpack capacity.
#' @param
#' x Parameter x is input, should be in type of data frame, which contains 2 vector weights and values of items.
#' W Parameter W is the capacity of the bag, should be numeric.
#' fast Parameter fast is in type of boolean and aims to improve performance via C++.
#' @return This function returns a list, which contains elements which are selected and the maximum value the bag contain under its capacity.
#' @examples
#' v <-c(2,2,6,5,4)
#' w <- c(6,3,5,4,6)
#' outcome1 <- dynamicProgramming(x = as.data.frame(cbind(w, v)), W = 10)
#' outcome2 <- dynamicProgramming(x = knapsack_objects[1:8,], W = 3500)
#' @export
#' @name dynamicProgramming

source("./R/RandomData.R")
library(Rccp)

dynamicProgramming <- function(x = NA, W = NA, fast = NA){
    stopifnot(is.logical(fast))
    stopifnot(is.data.frame(x))
    stopifnot(is.numeric(W))
    weight <- x$w
    value <- x$v
    ssq <- matrix(data = 0, nrow = length(weight) + 1, ncol = W + 1) # solution of sub-question

    if(fast){
        cppFunction('void knapsack(std::vector<double> weight, std::vector<double> value, int w){
          int n = weight.size();
          int m = value.size();
          double ssp[n][w];
          for (int i = 0; i <= n; i++){
            for (int j = 0; j < m; j++){
              ssp[i][j] = 0;
            }
          }
          # initialize ssp
          
          for(int i = 1; i <= n; i++){
            for(int j = w; j > 0; j--){
              ssq[i][j] = max(ssq[i-1][j], ssq[i-1][j-weight[i]] + values[i])
            }
          }
          vector<int> outcome;
          for(int i = 0; i <= n; i++){
            if(ssq[i][w+1] > ssq[i-1][w+1]){
              outcome.push_back(i);
              w -= weight[i];
            }
          }
        }')
    }



    for (i in 1:length(weight)){
        for (j in seq(from = W + 1, to = weight[i])){
           ssq[i + 1, j] <- max(ssq[i, j], ssq[i, j - weight[i]] + value[i])
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




