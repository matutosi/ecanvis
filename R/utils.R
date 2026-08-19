#' Cut continiuous scales
#'
#' Bins a numeric vector using the breaks that [graphics::hist()] would use.
#' The lowest break is included so that the minimum value is not dropped
#' (`cut()` alone returns `NA` for a value equal to the first break).
#'
#' @param x        A numeric vector.
#' @return         A factor vector of the same length as `x`.
#' @examples
#' x <- runif(100)
#' cut_conti(x)
#'
#' @export
cut_conti <- function(x){
  brks <- graphics::hist(x, plot = FALSE)$breaks
  cut(x, brks, include.lowest = TRUE)
}

#' Is a vector continuous?
#'
#' Used to decide whether a grouping column has to be binned before plotting.
#' Integer columns are continuous too, so [is.numeric()] is used rather than
#' [is.double()].  A missing column (`NULL`) is not continuous.
#'
#' @param x        A vector, or `NULL`.
#' @return         A logical of length 1.
#' @examples
#' is_conti(1:5)
#' is_conti(letters)
#'
#' @export
is_conti <- function(x){
  if(is.null(x)) return(FALSE)
  is.numeric(x) && !is.factor(x)
}

#' Bin a column when it is continuous
#'
#' Returns `df` unchanged when `col` is absent or not continuous.
#'
#' @param df       A data frame.
#' @param col      A string: column name to be binned.
#' @return         A data frame.
#' @examples
#' cut_conti_col(data.frame(x = 1:10), "x")
#'
#' @export
cut_conti_col <- function(df, col){
  if(is.null(col) || !(col %in% colnames(df))) return(df)
  if(!is_conti(df[[col]]))                     return(df)
  df[[col]] <- cut_conti(df[[col]])
  df
}

#' Choose a column for individuals
#'
#' Returns the item (species) column when `use_item` is `TRUE`, and the unit
#' (stand) column otherwise.
#'
#' @param use_item A logical of length 1.  `NULL` and `NA` are treated as `FALSE`.
#' @param st       A string: unit (stand) column name.
#' @param sp       A string: item (species) column name.
#' @return         A string.
#' @examples
#' pick_indiv(TRUE,  "stand", "species")
#' pick_indiv(FALSE, "stand", "species")
#'
#' @export
pick_indiv <- function(use_item, st, sp){
  if(is.null(use_item) || is.na(use_item) || !use_item) st else sp
}

#' Are unit, item and abundance columns usable?
#'
#' Unit and item must differ from each other and from abundance, all three
#' must exist in `df`, and abundance must be numeric.
#'
#' @param df       A data frame.
#' @param st       A string: unit (stand) column name.
#' @param sp       A string: item (species) column name.
#' @param ab       A string: abundance column name.
#' @return         A logical of length 1.
#' @examples
#' df <- data.frame(stand = "a", species = "b", cover = 1)
#' has_valid_cols(df, "stand", "species", "cover")
#'
#' @export
has_valid_cols <- function(df, st, sp, ab){
  cols <- c(st, sp, ab)
  if(length(cols) != 3)          return(FALSE)
  if(any(sapply(cols, is.null))) return(FALSE)
  if(any(is.na(cols)))           return(FALSE)
  if(any(cols == ""))            return(FALSE)
  if(anyDuplicated(cols) != 0)   return(FALSE)
  if(!all(cols %in% colnames(df))) return(FALSE)
  is.numeric(df[[ab]])
}

#' Filter the result of an indicator species analysis
#'
#' Both bounds are inclusive, so the widest settings of the app
#' (`p_max = 1`, `range = c(0, 1)`) keep every row.
#'
#' @param df       A data frame with columns `p.value` and `ind.val`.
#' @param p_max    A number: maximum p value.  A string is coerced.
#' @param range    A numeric vector of length 2: range of ind.val.
#' @return         A data frame.
#' @examples
#' df <- data.frame(p.value = c(0.01, 0.5), ind.val = c(0.9, 0.2))
#' filter_ind_val(df, 0.05, c(0, 1))
#'
#' @export
filter_ind_val <- function(df, p_max, range = c(0, 1)){
  p_max <- as.numeric(p_max)
  keep <- 
    df[["p.value"]] <= p_max &
    df[["ind.val"]] >= range[1] &
    df[["ind.val"]] <= range[2]
  df[!is.na(keep) & keep, , drop = FALSE]
}

#' Convert inputs into a list.
#'
#' `NULL` elements are dropped.  A single remaining element is unwrapped.
#'
#' @param ... Vectors or a list.
#' @return    A list, a single element, or `NULL` when nothing is left.
#' @examples
#' dots2list(1, NULL, 2)
#'
#' @export
dots2list <- function(...){
  res <- list(...)
  res <- res[!vapply(res, is.null, logical(1))] # remove NULL
  if(length(res) == 0) return(NULL)
  if(length(res) == 1) res <- res[[1]]
  return(res)
}

#' Transfer when true
#'
#' @param x        A community data matrix.
#' @param cond     A logical.
#' @return         `t(x)` when `cond` is `TRUE`, `x` otherwise.
#' @examples
#' t_if_true(matrix(1:6, nrow = 2), TRUE)
#'
#' @export
t_if_true <- function(x, cond){
  if(isTRUE(cond)) t(x) else x
}

#' Force a value that may be a reactive
#'
#' Shiny modules receive either a plain value or a reactive.  Arguments of an
#' R function are promises evaluated only once, so a reactive passed as a value
#' would freeze at its first use; call it through this helper instead.
#'
#' @param x        A value, or a function / reactive returning one.
#' @return         `x()` when `x` is callable, `x` otherwise.
#' @examples
#' as_value(1)
#' as_value(function() 1)
#'
#' @export
as_value <- function(x){
  if(is.function(x)) x() else x
}

#' Caution shown when unit, item and abundance are not usable
#'
#' @return         A string.
#' @examples
#' msg_invalid_cols()
#'
#' @export
msg_invalid_cols <- function(){
  paste("Select correct set of unit, item and abundance.",
        "Unit and item must not be duplicated.",
        "Abundance must be numeric.")
}

#' Are unit, item and abundance columns duplicated?
#'
#' @param st       A string: unit (stand) column name.
#' @param sp       A string: item (species) column name.
#' @param ab       A string: abundance column name.
#' @return         A logical of length 1.
#' @examples
#' has_duplicated_cols("stand", "stand", "cover")
#'
#' @export
has_duplicated_cols <- function(st, sp, ab){
  cols <- c(st, sp, ab)
  if(length(cols) != 3) return(FALSE)
  anyDuplicated(cols) != 0
}
