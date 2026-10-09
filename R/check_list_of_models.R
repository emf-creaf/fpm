#' Check list of regression models.
#'
#' @description
#' \code{check_list_of_models} checks out the validity of the elements in input list of models.
#'
#'
#' @param lst_models \code{list} object containing 7 regression objects.
#'
#' @returns
#' If the regression objects are correct, the output is the same list of models.
#'
#' @export
#'
#' @details
#' Checks are done on being a list, having 7 named elements and being regression objects.
#'
#'
#' @examples
#' x <- 1:100
#' set.seed(100)
#' y <- rnorm(100)
#' r <- lm(y ~ x)
#' lst <- list("seedlings" = r, "saplings" = r, "ingrowth" = r,
#'    "lambda" = r, "survival" = r, "growth" = r, "variance" = r)
#' lst2 <- check_list_of_models(lst)
#' identical(lst, lst2)
#'
check_list_of_models <- function(lst_models) {


  # It must be a list with 7 elements.
  if (!is.list(lst_models) || length(lst_models) != 7) {
    cli::cli_abort("Input 'lst_models' must be a list object with 7 named elements")
  }


  # All elements must be named.
  required <- c(
    "seedlings",
    "saplings",
    "ingrowth",
    "lambda",
    "survival",
    "growth",
    "variance"
  )

  missing <- setdiff(required, tolower(names(lst_models)))

  if (length(missing) > 0) {
    cli::cli_abort(paste0("Missing models: ", paste(missing, collapse = ", ")))
  }


  # Some of the elements must be a regression object.
  reg_classes <- c("lm", "glm", "glmerMod", "lmerMod", "gam", "nls",
                   "rlm", "coxph", "survreg", "randomForest", "gbm", "rq")
  if (!all(sapply(lst_models, inherits, what = reg_classes))) {
    cli::cli_abort("Elements in input list 'lst_models' have incorrect classes")
  }


  return(lst_models)

}
