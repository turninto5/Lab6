
test_that("Execution time is large", {
    knapsack_objects <- randomData()
    execution_time <- system.time({greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500)})
    print("This is the speed of the greedy knapsack with N=1000000")
    print(execution_time)
    expect_true(execution_time["elapsed"]>0.05)
})
