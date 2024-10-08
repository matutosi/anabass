% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/structure.R
\name{compute_diff_x}
\alias{compute_diff_x}
\title{Compute the Difference in \code{x} Values Between Two Sentences
Calculates the difference in \code{x} values between two sentences based on the specified connection.}
\usage{
compute_diff_x(df, con)
}
\arguments{
\item{df}{A data frame containing sentence information.}

\item{con}{A data frame or list representing the connection between two sentences.}
}
\value{
A list containing the difference in \code{x} values and the index of the first sentence.
}
\description{
Compute the Difference in \code{x} Values Between Two Sentences
Calculates the difference in \code{x} values between two sentences based on the specified connection.
}
\examples{
sentences <- list(
  c("there", "is", "a", "quick", "brown", "fox"),
  c("the", "fox", "chase", "a", "lazy", "dog"),
  c("dog", "jump", "over", "the", "moon"),
  c("apple", "apricot", "avocado", "banana")
)
df <-
  sentences |>
  list2df() |>
  add_x_pos() |>
  add_index()

con <- connect_with(sentences, 1)
compute_diff_x(df, con)

con <- connect_with(sentences, 2)
compute_diff_x(df, con)

con <- connect_with(sentences, 3)
compute_diff_x(df, con)

con <- connect_with(sentences, 4)
compute_diff_x(df, con)


}
