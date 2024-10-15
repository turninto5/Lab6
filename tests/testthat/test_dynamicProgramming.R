#context("dynamicProgramming")

knapsack_objects <- data.frame(w = c(3202, 3280, 1002, 2907, 2247, 1817, 2579, 472),
                               v = c(9899.656, 4384.936, 6999.032, 8890.770, 8341.595, 7344.215, 6469.033, 8428.783))
wrong_input <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))

test_that("Detects wrong input, x should be dataframe.",{
  expect_error(dynamicProgramming(x = list(w = c(1, 2, 3), v = c(3, 6, 4)), W = 3500, fast = FALSE))
  expect_error(dynamicProgramming(x = 100, W = 3500, fast = FALSE))
  expect_error(dynamicProgramming(x = "100", W = 3500, fast = FALSE))
  expect_error(dynamicProgramming(x = c(1 ,2 ,4), W = 3500, fast = FALSE))
})

test_that("Detects wrong input, x should contain w and v.",{
  expect_error(dynamicProgramming(x = wrong_input, W = 3500, fast = FALSE))
})


test_that("Detects wrong input, W should be numeric.",{
  expect_error(dynamicProgramming(x = knapsack_objects, W = "3500", fast = FALSE))

})

test_that("Detects wrong input, fast should be boolean.",{
  expect_error(dynamicProgramming(x = knapsack_objects, W = 3500, fast = 0))
  expect_error(dynamicProgramming(x = knapsack_objects, W = 3500, fast = "0"))
})



test_that("Output correct!",{
  output <- dynamicProgramming(x = knapsack_objects, W = 5000, fast = FALSE)
  expect_equal(output$value, 25327.47, tolerance = 1e-2)
  expect_equal(output$elements, c(8, 3, 1))
})



