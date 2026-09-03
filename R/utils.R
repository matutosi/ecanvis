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

#' Read the pseudospecies cut levels typed by the user
#'
#' The text box takes the levels as free text ("0, 2, 5, 10, 20").  Anything
#' that is not a number is dropped, and `default` is used when nothing is left,
#' so a half typed entry never stops the app.
#'
#' @param txt      A string, or `NULL`.
#' @param default  A numeric vector used when `txt` holds no number.
#' @return         A sorted numeric vector without duplicates.
#' @examples
#' parse_cut_levels("0, 2, 5, 10, 20")
#' parse_cut_levels("")
#'
#' @export
parse_cut_levels <- function(txt, default = c(0, 2, 5, 10, 20)){
  if(is.null(txt)) return(default)
  levels <- suppressWarnings(as.numeric(strsplit(txt, "[,[:space:]]+")[[1]]))
  levels <- levels[!is.na(levels)]
  if(length(levels) == 0) return(default)
  sort(unique(levels))
}

#' Read the number of groups typed by the user
#'
#' `ecan::twinspan()` takes `NULL` for "no limit" and needs at least 1.
#' The numeric input cannot hold `NULL`, so 0 (and an empty box) mean no limit.
#'
#' @param x        A number, or `NULL`.
#' @return         An integer, or `NULL` for no limit.
#' @examples
#' as_n_clusters(4)
#' as_n_clusters(0)
#'
#' @export
as_n_clusters <- function(x){
  if(is.null(x) || length(x) != 1 || is.na(x)) return(NULL)
  if(x < 1) return(NULL)
  as.integer(x)
}

#' Cluster a community table
#'
#' Wraps the two ways of clustering the app offers.  TWINSPAN is not one of the
#' `c_method` of [ecan::cluster()]: it divides the stands itself and uses no
#' distance, so it is run through [ecan::twinspan()] and converted with
#' [stats::as.hclust()].  The result carries `$clustering_method` and
#' `$distance_method` in either case, as the rest of the app expects.
#'
#' @param tbl        A community data matrix.
#'                   rownames: stands, colnames: species.
#' @param c_method   A string of clustering method, or "twinspan".
#' @param d_method   A string of distance method.  Unused by TWINSPAN.
#' @param modified   A logical: modified TWINSPAN (Roleček et al. 2009).
#' @param n_clusters An integer of the number of groups, or `NULL` for no limit.
#' @param cut_levels A numeric vector of pseudospecies cut levels.
#' @return           An "hclust" object.
#'
#' @export
compute_cluster <- function(tbl, c_method, d_method,
                            modified   = FALSE,
                            n_clusters = NULL,
                            cut_levels = c(0, 2, 5, 10, 20)){
  if(!identical(c_method, "twinspan")){
    return(ecan::cluster(tbl, c_method = c_method, d_method = d_method))
  }
  tw <- ecan::twinspan(tbl,
                       cut_levels = cut_levels,
                       modified   = isTRUE(modified),
                       n_clusters = n_clusters)
  cls <- stats::as.hclust(tw)
  cls$clustering_method <- "twinspan"
  cls$distance_method   <- NULL  # TWINSPAN uses no distance
  cls$twinspan          <- tw
  cls
}

#' Find a column name that is not taken yet
#'
#' Used before a helper column is added to the user's data, so that a column
#' of the same name is never overwritten.
#'
#' @param name     A string: the wanted name.
#' @param taken    A character vector of the names already in use.
#' @return         `name`, or `name` with a number appended.
#' @examples
#' unique_col_name("twinspan", c("stand", "species"))
#' unique_col_name("twinspan", c("twinspan", "twinspan_2"))
#'
#' @export
unique_col_name <- function(name, taken){
  if(!(name %in% taken)) return(name)
  i <- 2
  while(paste0(name, "_", i) %in% taken) i <- i + 1
  paste0(name, "_", i)
}

#' Add the groups found by TWINSPAN as a column
#'
#' The panel lets a group be chosen from the columns of the data.  TWINSPAN
#' makes groups of its own, so they are joined onto the data as one more
#' column and become selectable in the same way.
#'
#' `tw$classification` names its units `stand` whichever way round the table
#' was clustered: when items (species) are clustered the table is transposed
#' first, so those names are the items.  They are matched against `indiv`.
#'
#' @param df       A data frame.
#' @param tw       A "twinspan" object, or `NULL` for the other methods.
#' @param indiv    A string: the column of `df` holding the units.
#' @param col      A string: the wanted name of the new column.
#' @return         `df`, with one column added when `tw` is a TWINSPAN result.
#'
#' @export
add_tw_group <- function(df, tw, indiv, col = "twinspan"){
  if(is.null(tw) || !inherits(tw, "twinspan"))     return(df)
  if(is.null(indiv) || !(indiv %in% colnames(df))) return(df)

  cls <- tw$classification
  if(is.null(cls) || !all(c("stand", "group") %in% colnames(cls))) return(df)

  col <- unique_col_name(col, colnames(df))
  group <- stats::setNames(as.character(cls$group), as.character(cls$stand))
  df[[col]] <- unname(group[as.character(df[[indiv]])])
  df
}

#' Ordered two-way table of TWINSPAN as a data frame
#'
#' [ecan::tw_two_way()] returns a character matrix with the division paths in
#' its attributes, which `print()` lays out as the two-way table of the
#' original TWINSPAN.  `reactable()` needs a data frame, so the path of each
#' row is put in a column of its own, and the dichotomy of each column is put
#' in the rows at the bottom, as the digits printed below the original table.
#'
#' @param tw       A "twinspan" object made with `species = TRUE`.
#' @param cells    A string: "level" (pseudospecies cut level) or "abundance".
#' @param row_name A string: name of the first column.
#' @return         A data frame.
#'
#' @export
tw_two_way_df <- function(tw, cells = c("level", "abundance"),
                          row_name = "species"){
  if(is.null(cells) || is.na(cells[1])) cells <- "level"
  cells <- match.arg(cells[1], c("level", "abundance"))
  tab   <- ecan::tw_two_way(tw, cells = cells)

  path_col <- unique_col_name("path", c(row_name, colnames(tab)))
  head_cols <- c(row_name, path_col)

  df <- data.frame(rownames(tab), attr(tab, "species_path"),
                   stringsAsFactors = FALSE, check.names = FALSE)
  colnames(df) <- head_cols
  df <- cbind(df, as.data.frame(unclass(tab),
                                stringsAsFactors = FALSE, check.names = FALSE))
  colnames(df) <- c(head_cols, colnames(tab))
  rownames(df) <- NULL

    # the dichotomy of each column, one row per digit
  st_path <- attr(tab, "stand_path")
  depth   <- if(length(st_path) == 0) 0 else max(nchar(st_path))
  for(i in seq_len(depth)){
    digit <- substr(st_path, i, i)
    digit[digit == ""] <- " "
    row <- as.data.frame(as.list(c("", "", digit)),
                         stringsAsFactors = FALSE, check.names = FALSE)
    colnames(row) <- colnames(df)
    df <- rbind(df, row)
  }
  df
}
