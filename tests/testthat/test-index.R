test_that("can generate search index", {
  expect_equal(search_index(1), NULL)
  expect_equal(search_index(2), 1)
  expect_equal(search_index(3), c(1, 2))
  expect_equal(search_index(4), c(1, 3, 2))
})
