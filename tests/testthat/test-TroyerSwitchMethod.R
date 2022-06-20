test_that("Troyer works for 2 item lists", {
  x1 <- c("cow", "dog")
  y <- c(2,1)
  expect_equal(troyer_switch(x1), y)
})
test_that("Troyer works for generic example", {
  x1 <- c("cat", "dog", "mouse", "rat", "giraffe", "lion")
  y <- c(2,0, 1, 0, 1, 0)
  expect_equal(troyer_switch(x1), y)
})