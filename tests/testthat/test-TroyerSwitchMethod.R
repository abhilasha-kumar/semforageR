test_that("Troyer works for 2 item lists", {
  x1 <- c("cow", "dog")
  y <- c(2,0)
  expect_equal(troyer_switch(x1), y)
})