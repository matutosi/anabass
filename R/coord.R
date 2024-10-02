align_zero <- function(df){
  offset <- min(df$x_start)
  df |>
    dplyr::mutate("x_start" := .data[["x_start"]] - offset, 
                  "x_end"   := .data[["x_end"]]   - offset)
}

#' Add Cumulative Character Lengths to a Data Frame
#' Adds a new column `x_start` and `x_end` to a data frame, representing the cumulative sum of character lengths within specified groups.
#' @param df A data frame containing sentences.
#' @param group   A string of the column name containing sentence identifiers.
#' @param base    A string of the column name containing the base words for character length calculations.
#' @return The modified data frame with the added `x_start` and `x_end` columns.
#' @name add_x_pos
#' @examples
#' sentences <- list(
#'   c("the", "quick", "brown", "fox"),
#'   c("the", "lazy", "dog"),
#'   c("jumped", "over", "the", "moon")
#' )
#' df <- list2df(sentences)
#' df <- add_x_pos(df, group = "sentence", base = "word")
#' df <- add_index(df)
#'
#' @export
add_x_pos <- function(df, group = "sentence", base = "word"){
  x_1 <- "x_start"
  x_2 <- "x_end"
  cols <- c(group, base, x_1, x_2)
  df |>
    dplyr::group_by(.data[[group]]) |>
    dplyr::mutate(`:=`({{ x_2 }}, string_width(.data[[base]])), 0.5, 0.8)
    stringr::str_width() |>
    dplyr::mutate(`:=`({{ x_2 }}, purrr::accumulate(.data[[x_2]], sum))) |>
    dplyr::mutate(`:=`({{ x_1 }},
                       dplyr::lag(.data[[x_2]], n = 1, default = 0))) |>
    dplyr::relocate(dplyr::all_of(cols)) |>
    dplyr::ungroup()
}

#' Wrappaer function for stringr::str_width
#' 
string_width <- function(string, intercept, slope){
  intercept + stringr::str_width(string) * slope
}

#' Add index to a Data Frame
#' Adds a new column `index` to a data frame, representing the cumulative sum of character lengths within specified groups.
#' @rdname add_x_pos
#' @return The modified data frame with the added `index` column.
#'
#' @export
add_index <- function(df, group = "sentence"){
  df |>
    dplyr::group_by(.data[[group]]) |>
    dplyr::mutate(index = dplyr::row_number()) |>
    dplyr::ungroup()
}
