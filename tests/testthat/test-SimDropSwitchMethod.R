test_that("SimDrop works for 4 item lists", {
  x1 <- c("cow", "dog", "fish", "lizard")
  x2 <- c(0, 1, 2, 1)
  y <- c(2, 0, 2, 2)
  expect_equal(sim_drop_switch(x1, x2), y)
})
test_that("SimDrop works for generic example", {
  x1 <- c("cat", "dog", "mouse", "rat", "giraffe", "lion")
  x2 <- c(0, 2, 1, 2, 1, 1)
  y <- c(2, 0, 1, 0, 2, 2)
  expect_equal(sim_drop_switch(x1, x2), y)
})