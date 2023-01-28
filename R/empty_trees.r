#' Set trees to an empty \code{data.frame}
#'
#' @param a
#' @param idplot
#'
#' @return
#' @export
#'
#' @examples
empty_trees <- function(a, idplot, include_date = NA) {

  mf <- match.call()
  m <- match(c("a", "idplot"), tolower(names(mf)[-1]))
  if (any(is.na(m))) stop("Missing input")

  i <- match(idplot, a$idplot)
  if (any(is.na(i))) stop(cat(paste0("Could not find ",sum(is.na(i))," plots\n")))

  # Erase date or not?
  if (is.na(include_data)) stop("You must set 'include_date' to TRUE or FALSE")
  stopifnot(is.logical(include_date))
  if (include_date) z$stand_type <- NA

  # Erase content.
  a$trees[[1]] <- list()
  a$tree_species[[1]] <- list()
  a$basal_area <- data.frame()

  return(a)

}
