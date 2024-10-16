test_that("Detects wrong input", {
    expect_error(brute_force_knapsack("Faulty", W = 3500))
})

test_that("Detects wrong input#2", {
    expect_error(brute_force_knapsack(list(vs <- 1, w <- 2), W = 3500))
})

test_that("Detects wrong input#3", {
    expect_error(brute_force_knapsack(list(v <- 1, ws <- 2), W = 3500))
})

test_that("Detects wrong input#4", {
    expect_error(brute_force_knapsack(list(v <- 1, w <- 2), W = "3500"))
})

test_that("Optimization is correct", {
    knapsack_objects <- randomData(8)
    output <- brute_force_knapsack(x = knapsack_objects, W = 3500)
    expect_equal(output$value, 22772.03, tolerance = 1e-2)
    expect_equal(output$elements, c(3,6,8))
})

test_that("Execution time is large", {
    knapsack_objects <- randomData(16)
    execution_time <- system.time({brute_force_knapsack(x = knapsack_objects, W = 3500)})
    print("This is the speed of the brute force with N=16")
    print(execution_time)
    expect_true(execution_time["elapsed"]>0.05)
})

