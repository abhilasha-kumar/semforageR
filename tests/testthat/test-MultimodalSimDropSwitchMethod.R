test_that("MultimodalSimDrop works for generic example", {
  x1 <- c("cat", "dog", "mouse", "rat", "giraffe", "lion")
  x2 <- c(0, 2, 1, 2, 1, 1)
  x3 <- c(0,0,0,0,0,0)
  expect_equal(multimodal_switch(x1, x2, x3, 1), sim_drop_switch(x1,x2))
})
