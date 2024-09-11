#' Generate search index for multi sentences
#'
#' @param   i An Integer
#' @return  A numeric vector
#' 
#' @examples
#' search_index(1)
#' search_index(2)
#' search_index(3)
#' search_index(4)
#' 
#' @export
search_index <- function(i){
  if(i < 2) return(NULL)
  if(i == 2) return(1)
  c(1, seq(from = i - 1, to = 2))
}
