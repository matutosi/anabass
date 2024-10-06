## add_x_pos()
  # Test case 1: Basic functionality
test_that("add_x_pos correctly adds x_start and x_end columns", {
  df <- data.frame(
    sentence = c(1, 1, 1, 2, 2),
    word = c("the", "quick", "brown", "the", "lazy"),
    stringsAsFactors = FALSE
  )
  df <- add_x_pos(df, group = "sentence", base = "word")
  expect_true("x_start" %in% colnames(df))
  expect_true("x_end" %in% colnames(df))
  expect_equal(df$x_start, c(0, 3, 8, 0, 3))
  expect_equal(df$x_end, c(3, 8, 13, 3, 7))
})

  # Test case 2: Different group and base names
test_that("add_x_pos works with different group and base names", {
  df <- data.frame(
    group_id = c(1, 1, 1, 2, 2),
    item = c("the", "quick", "brown", "the", "lazy"),
    stringsAsFactors = FALSE
  )
  df <- add_x_pos(df, group = "group_id", base = "item")
  expect_true("x_start" %in% colnames(df))
  expect_true("x_end" %in% colnames(df))
  expect_equal(df$x_start, c(0, 3, 8, 0, 3))
  expect_equal(df$x_end, c(3, 8, 13, 3, 7))
})
