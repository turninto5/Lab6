test_that("Detects wrong input", {
    expect_error(knapsack_brute_force("Faulty", W = 3500))
})

test_that("Detects wrong input#2", {
    expect_error(knapsack_brute_force(list(vs <- 1, w <- 2), W = 3500))
})

test_that("Detects wrong input#3", {
    expect_error(knapsack_brute_force(list(v <- 1, ws <- 2), W = 3500))
})

test_that("Detects wrong input#4", {
    expect_error(knapsack_brute_force(list(v <- 1, w <- 2), W = "3500"))
})

test_that("Optimization is correct", {
    knapsack_objects <- randomData()
    output <- knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500)
    expect_equal(output$value, 22772.03, tolerance = 1e-2)
    expect_equal(output$elements, c(3,6,8))
})

test_that("Execution time is large", {
    knapsack_objects <- randomData()
    execution_time <- system.time({knapsack_brute_force(x = knapsack_objects[1:16,], W = 3500)})
    print("This is the speed of the brute force with N=16")
    print(execution_time)
    expect_true(execution_time["elapsed"]>0.05)
})

