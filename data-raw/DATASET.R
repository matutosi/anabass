## code to prepare `sentences` dataset goes here
sentences <-
  stringr::sentences |>
  stringr::str_remove("\\.") |>
  stringr::str_split(" ")

usethis::use_data(sentences, overwrite = TRUE)


## code to prepare `neko_sentences` dataset goes here
neko_sentences <-
  moranajp::neko_chamame |> 
  moranajp::unescape_utf() |>
  moranajp::add_sentence_no() |>
  moranajp::clean_up(use_common_data = FALSE, add_stop_words = moranajp::stop_words) |>
  dplyr::select(dplyr::all_of(c("\u539f\u5f62", "sentence"))) |>
  `colnames<-`(c("word", "sentence"))

usethis::use_data(neko_sentences, overwrite = TRUE)

