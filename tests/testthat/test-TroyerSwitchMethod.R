test_that("DeltaSim works for 3 item lists", {
  x1 <- c("cow", "dog", "fish")
  x2 <- c(0, 0.1, 0.2)
  y <- c(2,2)
  expect_equal(delta_similarity_switch(x1, x2), y)
})