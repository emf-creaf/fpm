#' Set trees to an empty \code{data.frame}
#'
#' @param a
#' @param idplot
#' @param criterion
#' @param threshold
#' @param per_species
#' @param update
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
clear_trees <- function(a, idplot, criterion = "number_trees", threshold = .01, per_species = T, update = T, verbose = F) {

  mf <- match.call()
  m <- match(c("a", "idplot"), tolower(names(mf)[-1]))


  # Does 'idplot' exist?
  id <- if (is.na(m[2])) 1:length(a$idplot) else match(idplot, a$idplot)
  stopifnot("Could not find some 'idplot' in 'a'" = all(!is.na(id)))


  # Plots to evaluate must be of stand_type = "ipm".
  stand_type <- sapply(id, function(i) a$plot[[i]]$stand_type == "ipm")
  stopifnot("Plots to clear must be of 'ipm' type" = all(stand_type))


  # Check which criterion to use.
  criterion <- match.arg(criterion, c("basal_area", "number_trees"))
  criterion <- paste0("total_", criterion)
  if (per_species) criterion <- paste0(criterion, "_species")


  # Shall we update beforehand?
  if (update) a <- update_stands(a, verbose = verbose)


  # Erase tree content if condition is met.
  for (i in id) {

    b <- a$plot[[i]]
    if (per_species) {
      j <- which(b[[criterion]] < threshold)
      if (length(j) > 0) b[[criterion]][j] <- vector("list", length(j))
    } else {
      if (b[[criterion]] < threshold) b$trees <- list()
    }
    a$plot[[i]] <- b
  }


  # Update.
  if (update) a <- update_stands(a, verbose = verbose)


  return(a)
}
