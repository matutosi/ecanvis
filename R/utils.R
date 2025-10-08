#' Cut continiuous scales
#' 
#' @param x        A numeric vector.
#' @return         A factor vector.
#' @examples
#' x <- runif(100)
#' cut_conti(x)
#' 
#' @export
cut_conti <- function(x){
  brks <- graphics::hist(x, plot = FALSE)$breaks
  cut(x, brks)
}

#' Convert inputs into a list.
#' @param ... Vectors or a list.
#' @export
dots2list <- function(...){
  res <- list(...)
  if(length(res) == 0) return(NULL)
  res[sapply(res, is.null)] <- NULL # remove NULL
  if (length(res) == 1) res <- res[[1]]
  return(res)
}

#' Transfer when true
#' 
#' @param x        A community data matrix.
#' @param cond     A logical.
#' 
#' @export
t_if_true <- function(x, cond){
  if(cond) t(x) else x
}
