#' @title Find Connections Between Sentences
#' @description Finds connections between sentences 
#'              in a list based on shared common words.
#' @param sentences A list of sentences (character vectors).
#' @return A list of connections, where each element represents 
#'          a connection between two sentences.
#' @examples
#' sentences <- list(
#'   c("the", "quick", "brown", "fox"),
#'   c("the", "lazy", "dog"),
#'   c("jumped", "over", "the", "moon")
#' )
#' connections <- connection(sentences)
connection <- function(sentences){
  seq <- seq_along(sentences)
  connect <-
    seq |>
    purrr::map(connect_with, sentences)
  return(connect)
}

#' @title Connect a Sentence with Others
#' @description Connects a specific sentence with other sentences 
#'              in the list based on shared common words.
#' @param df A data frame containing sentence information.
#' @param i The index of the target sentence.
#' @param connect A list of connections between sentences.
#' @return The modified data frame with updated `x` positions.
#' @examples
#' sentences <- list(
#'   c("the", "quick", "brown", "fox"),
#'   c("the", "lazy", "dog"),
#'   c("jumped", "over", "the", "moon")
#' )
#' df <- sentences2df(sentences)
#' connect <- connection(sentences)
#' df <- connect_sentence_i(df, 1, connect)
connect_sentence_i <- function(df, i, connect){
  diff <- compute_diff_x(df, connect[[i]])
  df <- update_x_pos(df, diff)
  return(df)
}

#' @title Connect Sentences Based on Common Words
#' @description Connects sentences in a list based on shared common words, 
#'             adjusting their `x` positions accordingly.
#' @param sentences A list of sentences (character vectors).
#' @return A data frame containing the sentences 
#'         with updated `x` positions to reflect the connections.
#'
#' @examples
#' sentences <- list(
#'   c("the", "quick", "brown", "fox"),
#'   c("the", "lazy", "dog"),
#'   c("jumped", "over", "the", "moon")
#' )
#' connected_df <- connect_sentences(sentences)
#'
#' @export
connect_sentences <- function(sentences){
  df <- sentences2df(sentences)
  connect <- connection(sentences)
  df <-
    seq_along(sentences) |>
    purrr::reduce(connect_sentence_i,
                  .init = df,
                  connect = connect)
  df <- highlight_connect_word(df, connect)
  df <- align_zero(df)
  return(df)
}

#' @title Create a Data Frame from Sentences
#' @description Converts a list of sentences into a data frame with 
#'              `word`, `sentence`, `x_start`, and `x_end` columns.
#' @param sentences A list of sentences (character vectors).
#' @return A data frame representing the sentences.
#'
#' @examples
#' sentences <- list(
#'   c("the", "quick", "brown", "fox"),
#'   c("the", "lazy", "dog"),
#'   c("jumped", "over", "the", "moon")
#' )
#' df <- sentences2df(sentences)
#'
#' @export
sentences2df <- function(sentences){
  df <-
    sentences |>
    list2df() |>
    add_x_pos() |>
    add_index()
  return(df)
}

#' @title Highlight Connected Words in a Data Frame
#' @description Adds a `highlight` column to a data frame indicating 
#'              which words are connected.
#' @param connect A data frame or list representing connections between sentences.
#' @return The modified data frame with the `highlight` column.
#'
#' @examples
#' connect <- data.frame(
#'   sentence_i = c(1, 1),
#'   sentence_j = c(2, 3),
#'   word_i = c(1, 3),
#'   word_j = c(1, 3)
#' )
#' df <- highlight_df(connect)
#'
#' @export
highlight_df <- function(connect){
  connect <-
    connect |>
    dplyr::bind_rows() |>
    na.omit()
  df <-
    tibble::tibble(
      sentence = c(connect$sentence_i, connect$sentence_j),
      index = c(connect$word_i, connect$word_j),
      highlight = TRUE) |>
    dplyr::distinct()
  return(df)
}

#' @title Highlight Connected Words in a Data Frame
#' @description Adds a `highlight` column to a data frame indicating 
#'              which words are connected.
#' @param df A data frame containing sentence information.
#' @param connect A data frame or list representing connections between sentences.
#' @return The modified data frame with the `highlight` column.
#'
#' @examples
#' df <- data.frame(
#'   sentence = c(1, 1, 1, 2, 2),
#'   index = c(1, 2, 3, 1, 2),
#'   x_start = c(0, 3, 8, 0, 3),
#'   x_end = c(3, 8, 14, 3, 7),
#'   stringsAsFactors = FALSE
#' )
#' connect <- data.frame(
#'   sentence_i = 1,
#'   sentence_j = 2,
#'   word_i = 3,
#'   word_j = 1
#' )
#' df <- highlight_connect_word(df, connect)
#'
#' @export
highlight_connect_word <- function(df, connect){
  high_df <- highlight_df(connect)
  df <-
    df |>
    dplyr::left_join(high_df,
                     by = join_by("sentence", "index"))
  return(df)
}


#' Update `x` Positions Based on a Computed Difference
#' Updates the `x_start` and `x_end` positions in a data frame based on a computed difference.
#' @param df A data frame containing sentence information.
#' @param diff A list representing the difference in `x` values and
#'        the index of the first sentence.
#' @return The modified data frame with updated `x_start` and `x_end` positions.
#' @name update_x_pos
#' @examples
#' sentences <- list(
#'   c("there", "is", "a", "quick", "brown", "fox"),
#'   c("the", "fox", "chase", "a", "lazy", "dog"),
#'   c("dog", "jump", "over", "the", "moon"),
#'   c("apple", "apricot", "avocado", "banana")
#' )
#'
#' sentences <- list(
#'   c("これ", "は", "文章", "です"),
#'   c("文章", "は", "短い", "方", "が", "よい"),
#'   c("短い", "文章", "は", "読み", "やすい")
#' )
#'
#' df <- connect_sentences(sentences)
#'
#' df |>
#'   ggplot2::ggplot(ggplot2::aes(x = x_start, y = sentence, label = word)) +
#'   ggplot2::geom_point() +
#'   gghighlight::gghighlight(highlight == TRUE, label_key = word) +
#'   ggplot2::scale_y_reverse() +
#'   ggplot2::theme_bw()
#'
#' df |>
#'   ggplot2::ggplot(ggplot2::aes(x = x_start, y = sentence,
#'                                label = word,
#'                                fill = highlight)) +
#'   ggplot2::geom_label() +
#'   ggplot2::scale_y_reverse() +
#'   ggplot2::theme_bw()
#'
#' df |>
#'   ggplot2::ggplot(ggplot2::aes(x = x_start, y = sentence, label = word)) +
#'   ggplot2::geom_text(hjust = 0) +
#'   ggplot2::scale_y_reverse() +
#'   ggplot2::theme_bw()
#'
#' @export
update_x_pos <- function(df, diff){
  offset <- diff$difference
  s_i <- diff$sentence_i
  df <-
   df |>
   dplyr::mutate(
     "x_start" := dplyr::case_when(
       sentence == s_i ~ .data[["x_start"]] + offset,
       .default = .data[["x_start"]]
       ),
     "x_end" := dplyr::case_when(
       sentence == s_i ~ .data[["x_end"]] + offset,
       .default = .data[["x_end"]]
     )
   )
  return(df)
}


#' Compute the Difference in `x` Values Between Two Sentences
#' Calculates the difference in `x` values between two sentences based on the specified connection.
#' @param con A data frame or list representing the connection between two sentences.
#' @return A list containing the difference in `x` values and the index of the first sentence.
#' @inheritParams update_x_pos
#' @examples
#' sentences <- list(
#'   c("there", "is", "a", "quick", "brown", "fox"),
#'   c("the", "fox", "chase", "a", "lazy", "dog"),
#'   c("dog", "jump", "over", "the", "moon"),
#'   c("apple", "apricot", "avocado", "banana")
#' )
#' df <-
#'   sentences |>
#'   list2df() |>
#'   add_x_pos() |>
#'   add_index()
#'
#' con <- connect_with(1, sentences)
#' compute_diff_x(df, con)
#'
#' con <- connect_with(2, sentences)
#' compute_diff_x(df, con)
#'
#' con <- connect_with(3, sentences)
#' compute_diff_x(df, con)
#'
#' con <- connect_with(4, sentences)
#' compute_diff_x(df, con)
#'
#'
#' @export
compute_diff_x <- function(df, con){
  s_i <- con$sentence_i
  s_j <- con$sentence_j
  w_i <- con$word_i
  w_j <- con$word_j
  x_start_i <-
    dplyr::filter(df, .data[["sentence"]] == s_i, .data[["index"]] == w_i) |>
    `$`(_, "x_start")
  if(is.null(s_j)){ # when no match
    x_start_j <- max(df$x_end)
  }else{
    x_start_j <-
      dplyr::filter(df, .data[["sentence"]] == s_j, .data[["index"]] == w_j) |>
      `$`(_, "x_start")
  }
  x_start_diff <- x_start_j - x_start_i
  diff <- list(difference = x_start_diff,
               sentence_i = s_i)
  return(diff)
}

#' Convert List of Sentences to Data Frame
#' Converts a list of character vectors (sentences) into a data frame with two columns: `word` and `sentence`.
#' @param sentences A list of character vectors representing sentences.
#' @return A data frame with two columns:
#'   - **word:** The individual words from the sentences.
#'   - **sentence:** The corresponding sentence index for each word.
#' @examples
#' sentences <- list(
#'   c("the", "quick", "brown", "fox"),
#'   c("the", "lazy", "dog"),
#'   c("jumped", "over", "the", "moon")
#' )
#' list2df(sentences)
#'
#' @export
list2df <- function(sentences){
  index <- seq_along(sentences)
  each_len <- purrr::map_int(sentences, length)
  sentence <-
    purrr::map2(index, each_len, \(.x, .y) rep(x = .x, each = .y)) |>
    unlist()
  tibble::tibble(
    word = unlist(sentences),
    sentence = sentence
  )
}



#' Find the First Common Word in Two Sentences
#'
#' Finds the first common word in two sentences and returns its indices in each sentence.
#'
#' @param sentence_i,sentence_j  A character vector of sentence.
#' @return A list with two elements or NULL when no common word was found:
#'   - **common_i:** The index of the common word in the sentence of i.
#'   - **common_j:** The index of the common word in the sentence of j.
#' @examples
#' sentence1 <- c("the", "quick", "brown", "fox")
#' sentence2 <- c("the", "lazy", "dog")
#' search_common_word(sentence1, sentence2)
#'
#' @export
search_common_word <- function(sentence_i, sentence_j){
  common <- intersect(sentence_i, sentence_j)
  if(length(common) == 0){
    return(NULL)
  }
  common_i <- which(sentence_i == common[1])[1]
  common_j <- which(sentence_j == common[1])[1]
  return(list(common_i = common_i, common_j = common_j))
}



#' Find the First Connection for a Given Sentence
#' Finds the first connection for a given sentence in a list of sentences.
#' @param sentences A list of sentences (character vectors).
#' @param i         An index of the target sentence
#' @return A list representing the connection
#'         (with elements `sentence_i`, `sentence_j`, `word_i`, and `word_j`),
#'         or only `sentence_i` when no connection found.
#' @examples
#' sentences <- list(
#'   c("there", "is", "a", "quick", "brown", "fox"),
#'   c("the", "fox", "chase", "a", "lazy", "dog"),
#'   c("dog", "jump", "over", "the", "moon")
#' )
#' connect_with(1, sentences)
#' connect_with(2, sentences)
#' connect_with(3, sentences)
#'
#' @export
connect_with <- function(i, sentences){
  len <- length(sentences)
  if(len == 1) return(list(sentence_i = i))
  for(j in search_index(i)){
    common <- search_common_word(sentences[[i]], sentences[[j]])
    if(is.null(common)) next
    con <- list(sentence_i = i,
                sentence_j = j,
                word_i = common$common_i,
                word_j = common$common_j)
    return(con)
  }
  return(list(sentence_i = i, word_i = 1))
}

#' Split sentence into list
#' @param df A dataframe
#' @param group A string specifying group
#' @return A list of split dataframe
#' @examples
#' sentences <- list(
#'   c("the", "quick", "brown", "fox"),
#'   c("the", "lazy", "dog"),
#'   c("jumped", "over", "the", "moon")
#' )
#' list2df(sentences) |>
#'   split_sentence()
#'
#' @export
split_sentence <- function(df, group = "sentence"){
  dplyr::group_split(df, .data[[group]])
}
