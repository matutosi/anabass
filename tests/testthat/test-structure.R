## list2df()
  # Test case 1: Simple list of sentences
test_that("list2df correctly converts a simple list of sentences", {
  sentences <- list(
    c("the", "quick", "brown", "fox"),
    c("the", "lazy", "dog")
  )
  df <- list2df(sentences)
  expect_identical(names(df), c("word", "sentence"))
  expect_equal(nrow(df), 7)
  expect_equal(df$word[1:4], c("the", "quick", "brown", "fox"))
  expect_equal(df$sentence[1:4], c(1, 1, 1, 1))
  expect_equal(df$word[5:7], c("the", "lazy", "dog"))
  expect_equal(df$sentence[5:7], c(2, 2, 2))
})
  # Test case 2: Empty list
test_that("list2df returns an empty data frame for an empty list", {
  sentences <- list()
  df <- list2df(sentences)
  expect_identical(names(df), character(0))
  expect_equal(nrow(df), 0)
})
  # Test case 3: List with a single sentence
test_that("list2df correctly handles a list with a single sentence", {
  sentences <- list(
    c("this", "is", "a", "single", "sentence")
  )
  df <- list2df(sentences)
  expect_identical(names(df), c("word", "sentence"))
  expect_equal(nrow(df), 5)
  expect_equal(df$word, c("this", "is", "a", "single", "sentence"))
  expect_equal(df$sentence, c(1, 1, 1, 1, 1))
})
  # Test case 4: List with different sentence lengths
test_that("list2df correctly handles sentences with different lengths", {
  sentences <- list(
    c("short", "sentence"),
    c("longer", "sentence", "with", "more", "words"),
    c("single", "word")
  )
  df <- list2df(sentences)
  expect_identical(names(df), c("word", "sentence"))
  expect_equal(nrow(df), 9)
  expect_equal(df$word[1:2], c("short", "sentence"))
  expect_equal(df$sentence[1:2], c(1, 1))
})


## search_common_word()
  # Test case 1: Common word found
test_that("search_common_word finds a common word", {
  sentence1 <- c("yellow", "quick", "the", "fox")
  sentence2 <- c("yellow", "lazy", "dog")
  result <- search_common_word(sentence1, sentence2)
  expect_equal(result$common_i, 1)
  expect_equal(result$common_j, 1)
})
  # Test case 2: No common word found
test_that("search_common_word returns NA when no common word is found", {
  sentence1 <- c("apple", "banana", "cherry")
  sentence2 <- c("avocado", "pear", "grape")
  result <- search_common_word(sentence1, sentence2)
  expect_equal(result, NULL)
})
  # Test case 3: Multiple common words
test_that("search_common_word finds yellow first common word", {
  sentence1 <- c("yellow", "quick", "the", "fox", "yellow")
  sentence2 <- c("a", "lazy", "dog", "yellow", "fox")
  result <- search_common_word(sentence1, sentence2)
  expect_equal(result$common_i, 1)
  expect_equal(result$common_j, 4)
})

## split_sentence()
  # Test case 1: Basic functionality
test_that("split_sentence splits a data frame into a list of data frames", {
  df <- data.frame(
    sentence = c(1, 1, 1, 2, 2),
    word = c("the", "quick", "brown", "the", "lazy"),
    stringsAsFactors = FALSE
  )
  split_dfs <- split_sentence(df, group = "sentence")
  expect_equal(length(split_dfs), 2)
})
  # Test case 2: Different group name
test_that("split_sentence works with different group names", {
  df <- data.frame(
    group_id = c(1, 1, 1, 2, 2),
    item = c("the", "quick", "brown", "the", "lazy"),
    stringsAsFactors = FALSE
  )
  split_dfs <- split_sentence(df, group = "group_id")
  expect_equal(length(split_dfs), 2)
})

## connect_with()
  # Test case 1: Find a connection
test_that("connect_with finds a connection", {
  sentences <- list(
    c("the", "quick", "brown", "fox"),
    c("the", "lazy", "dog"),
    c("jumped", "over", "the", "moon")
  )
  connection <- connect_with(sentences, 2)
  expect_equal(connection$sentence_i, 2)
  expect_equal(connection$sentence_j, 1)
  expect_equal(connection$word_i, 1)
  expect_equal(connection$word_j, 1)
})
  # Test case 2: No connection found
test_that("connect_with returns NULL if no connection is found", {
  sentences <- list(
    c("apple", "banana", "cherry"),
    c("orange", "pear", "grape"),
    c("strawberry", "kiwi", "mango")
  )
  connection <- connect_with(sentences, 1)
  expect_null(connection$sentence_j)
})
  # Test case 3: Single sentence
test_that("connect_with returns NULL for a single sentence", {
  sentence <- c("this", "is", "a", "single", "sentence")
  connection <- connect_with(list(sentence), 1)
  expect_null(connection$sentence_j)
})


## compute_diff_x()
  # Test case 1: Basic functionality
test_that("compute_diff_x correctly computes the difference", {
  sentences <- list(
    c("there", "is", "a", "quick", "brown", "fox"),
    c("the", "fox", "chase", "a", "lazy", "dog"),
    c("dog", "jump", "over", "the", "moon")
  )
  df <-
    sentences |>
    list2df() |>
    add_x_pos() |>
    add_index()
  connection <- connect_with(sentences, 2)
  diff <- compute_diff_x(df, connection)
  expect_equal(diff$difference, 15)
  expect_equal(diff$sentence_i, 2)

  connection <- connect_with(sentences, 3)
  diff <- compute_diff_x(df, connection)
  expect_equal(diff$difference, 16)
  expect_equal(diff$sentence_i, 3)
})

## update_x_pos()
  # Test case 1: Basic functionality
test_that("update_x_pos correctly updates x positions", {
  sentences <- list(
    c("there", "is", "a", "quick", "brown", "fox"),
    c("the", "fox", "chase", "a", "lazy", "dog"),
    c("dog", "jump", "over", "the", "moon")
  )
  df <-
    sentences |>
    list2df() |>
    add_x_pos() |>
    add_index()

  for(i in seq_along(sentences)){
    con <- connect_with(sentences, i)
    if(is.null(con$sentence_j)) next
    diff <- compute_diff_x(df, con)
    df <- update_x_pos(df, diff)
  }
  expect <- c(0, 5, 7, 8, 13, 18,15,18,21, 26, 27, 31,31,34,38, 42, 45)
  testthat::expect_equal(df$x_start, expect)
})
