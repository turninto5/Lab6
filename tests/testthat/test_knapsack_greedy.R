
test_that("Greedy: Execution time is large", {
     knapsack_objects <- randomData(1000000)
    execution_time <- system.time({greedy_knapsack(x = knapsack_objects, W = 3500)})
    print("This is the speed of the greedy knapsack with N=1000000")
    print(execution_time)
    expect_true(execution_time["elapsed"]>0.05)
})
