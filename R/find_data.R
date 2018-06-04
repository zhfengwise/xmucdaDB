#' Find a dataset by dataset description title or dataset variable name
#'
#' Find the required data sets quickly when the dataset name is unknown
#' but some description about the dataset or some variable name is known.
#'
#' @param pattern,ignore.case,perl,fixed We use \code{grep} to match
#'   the pattern with dataset description or with variable name.
#'   All those parameters that would be passed to \code{grep},
#'   see \code{\link{grep}} for more information.
#'
#'   Note that the default value of \code{ignore.case} in \code{grep} is FALSE, but
#'   in this two functions, the default value is TRUE.
#'
#' @param package a character vector giving the package(s) to look in for data sets,
#'   or NULL (for looking for dataset in all data sets).
#'
#'   By default, this two functions would find dataset in \code{xmucdaDB} package.
#'
#' @param simplify a logical. If TRUE (default), function would return
#'   the dataset name only. If FALSE, it would return a \code{data.frame}
#'   which includes the package, dataset name, dataset description and
#'   variable name that match the pattern (only for \code{find_data_by_var}).
#'
#' @examples
#' # 描述中包含"癌"的数据集
#' find_data_by_title("癌")
#' find_data_by_title("癌", simplify = FALSE)
#'
#' # 描述中包含"癌"且有变量名包含"treatment"的数据集
#' intersect(
#'   find_data_by_title("癌"),
#'   find_data_by_var("treatment", ignore.case = TRUE)
#' )
#'
#' @export
find_data_by_title <- function(pattern, package = "xmucdaDB",
                               ignore.case = TRUE, perl = FALSE,
                               fixed = FALSE, simplify = TRUE) {
  all_dataset <- utils::data(package = package)$result
  dataset_index <- grep(
    pattern, all_dataset[, 4],
    ignore.case = ignore.case, perl = perl, fixed = fixed
  )
  result <- all_dataset[dataset_index, -2, drop = FALSE]
  if (simplify) {
    return(unname(result[, 2]))
  }
  as.data.frame(result)
}



#' @rdname find_data_by_title
#' @export
find_data_by_var <- function(pattern, package = "xmucdaDB",
                             ignore.case = TRUE, perl = FALSE,
                             fixed = FALSE, simplify = TRUE) {
  all_dataset <- utils::data(package = package)$result
  result <- apply(all_dataset, 1, function(dataset) {
    package_name <- dataset[1]
    dataset_name <- dataset[3]
    pos_package <- paste0("package:", package_name)
    data_object <- get(dataset_name, pos = pos_package)
    all_var_name <- names(as.data.frame(data_object))
    all_var_name <- setdiff(all_var_name, "Freq")
    match_name <-grep(
      pattern, all_var_name,
      ignore.case = ignore.case, perl = perl, fixed = fixed
    )
    match_name <- all_var_name[match_name]
    if (length(match_name)) {
      match_name <- paste0(match_name, collapse = ", ")
      return(c(
        dataset_name,
        match_name = match_name
      ))
    }
  })
  if (!is.list(result)) {
    return(NULL)
  }
  result <- do.call(rbind, result)
  if (simplify) {
    return(unname(result[, 1]))
  }
  merge(all_dataset[, -2], result)
}
