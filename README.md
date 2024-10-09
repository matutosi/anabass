
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pivotea

<!-- badges: start -->
<!-- badges: end -->

The goal of anabass is to analyse basic structure of sentences for
better writeings.

anabass is from a name of fish ginus anabas. anabas was is thought to
climb tree, but not in actual.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("matutosi/anabass")
```

<!--
You can also install from CRAN:

``` r
install.packages("anabass")
```
 -->

## Example

You can use `fun()` to analyse basic structure.

``` r
library(anabass)
#> 
#> Attaching package: 'anabass'
#> The following object is masked from 'package:stringr':
#> 
#>     sentences
library(dplyr)
library(ggplot2)

devtools::load_all(".")
#> ℹ Loading anabass
library(moranajp)
review_sentences <- 
  review_chamame |>
  unescape_utf() |>
  add_sentence_no() |>
  clean_up() |>
  dplyr::filter(chap == 5) |>
  dplyr::select(sentence, word = "\u539f\u5f62") |>
  dplyr::filter(word != "") |>
  dplyr::group_by(sentence) |>
  dplyr::summarise(word = reduce(word, paste, sep = ";")) |>
  `$`(_, "word") |>
  stringr::str_split(";")

df <-
  review_sentences |>
  list2df() |>
  add_x_pos() |>
  add_index()

for(i in seq_along(review_sentences)){
  con <- connect_with(review_sentences, i)
  diff <- compute_diff_x(df, con)
  df <- update_x_pos(df, diff)
}
print(df)
#> # A tibble: 426 × 5
#>    sentence word         x_start x_end index
#>       <int> <chr>          <dbl> <dbl> <int>
#>  1        1 草原            184   188.     1
#>  2        1 おく            188.  192.     2
#>  3        1 単位            192.  196.     3
#>  4        1 面積            196.  200.     4
#>  5        1 世界            200.  204.     5
#>  6        1 最高            204.  209.     6
#>  7        1 植物            209.  213.     7
#>  8        1 種数            213.  217.     8
#>  9        1 チェコ          217.  223.     9
#> 10        1 アルゼンチン    223.  234     10
#> # ℹ 416 more rows


df |>
  ggplot2::ggplot(ggplot2::aes(x = x_start, y = sentence, label = word)) +
  ggplot2::geom_text() +
  ggplot2::scale_y_reverse() +
  ggplot2::theme_bw()
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r

df <- 
  review_chamame |>
  unescape_utf() |>
  dplyr::filter(chap == 5) |>
  add_sentence_no() |>
  dplyr::distinct(sect, para, sentence) |> 
  dplyr::right_join(df)
#> Joining with `by = join_by(sentence)`

df |>
  ggplot2::ggplot(ggplot2::aes(x = x_start, y = sentence, label = word)) +
  ggplot2::geom_text() +
  ggplot2::scale_y_reverse() +
  ggplot2::theme_bw() + 
  ggplot2::facet_wrap(vars(para))
```

<img src="man/figures/README-example-2.png" width="100%" />

## Citation

Toshikazu Matsumura (2024) anabass: Analyse Basic Structure of Sentences
for Better Writing. <https://github.com/matutosi/anabass/>.
