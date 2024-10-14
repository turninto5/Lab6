 RNGversion(min(as.character(getRversion()),"3.5.3"))

 ##old sampler used for backward compatibility
 ## suppressWarnings() can be used so that the above warning is not displayed
 set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
 n <- 2000
 knapsack_objects <- data.frame(w=sample(1:3500, size = n, replace = TRUE),
                                v=runif(n = n, 0, 10000))


# print(knapsack_objects[1:8,])



dynamicProgramming <- function(x = NA, W = NA){
    stopifnot(is.data.frame(x))
    stopifnot(is.numeric(W))
    weight <- x$w
    value <- x$v
    ssq <- matrix(data = 0, nrow = length(weight) + 1, ncol = W + 1) # solution of sub-question

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


outcome <- dynamicProgramming(x = knapsack_objects[1:8,], W = 3500)
print(outcome)

