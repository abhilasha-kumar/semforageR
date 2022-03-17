test_that("hello works", {
  x <- as.data.frame(c("apple-mango", "x-223"))
  colnames(x) = "c1"
  y <- data.frame(c("apple", "x"), c("mango", "223"))
  colnames(y) = c("c1", "c2")
  expect_equal(hello(x, "c1", separator = "-"), y)
})
