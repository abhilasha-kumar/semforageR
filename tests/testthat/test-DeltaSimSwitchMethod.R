test_that("DeltaSim works for 3 item lists", {
  x1 <- c("cow", "dog", "fish")
  x2 <- c(0, 0.1, 0.2)
  y <- c(2,1,0)
  expect_equal(delta_similarity_switch(x1, x2, 0,0), y)
})
test_that("DeltaSim works for generic example", {
  x1 <- c("cat", "dog", "mouse", "rat", "giraffe", "lion")
  x2 <- c(0, 2, 1, 2, 1, 1.5)
  y <- c(2, 0, 1, 0, 1, 0)
  expect_equal(delta_similarity_switch(x1, x2, 0.5,0.5), y)
})
