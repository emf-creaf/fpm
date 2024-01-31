#' Title
#'
#' @param a
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
check_stand <- function(a, verbose = T) {

  # 'a' must be an "sf" object with one row.
  stopifnot("Input 'a' must be an 'sf' object" = any(class(a) == "sf"))
  stopifnot("Input sf object must have one row only" = nrow(a) == 1)

  # Little function to avoid repeating things.
  flist <- function(b, lab = lab, colnam = c("species", "n")) {
    flag <- TRUE
    b <- b[[lab]]
    if (!(inherits(b, "list") | inherits(a, "data.frame"))) {
      flag <- FALSE
      if (verbose) warning(paste("Column", lab," must be a data.frame or a list"))
    } else {
      b <- b[[1]]
      if (!is.null(b)) {
        if (!inherits(b, "data.frame")) {
          flag <- FALSE
          if (verbose) warning(paste("Elements in list element", lab," must be a data.frame"))
        } else {
          if (!all(colnames(b) %in% colnam)) {
            flag <- FALSE
            if (verbose) warning(paste0("Column names in ", lab, " are wrong"))
          }
        }
      }
    }
    return(flag)
  }


  flag <- TRUE

  if (get_parameters(a, "country") == "spain") {
    if (!all(names(a) %in% c("geometry", "idplot", "date", "stand_type", "seedlings", "saplings", "trees"))) {
      flag <- FALSE
      if (verbose) warning("Wrong column names in 'a' sf object")
    } else {

      # Check date column.
      if (!(inherits(a$date, "character") | inherits(a$date, "Date"))) {
        flag <- FALSE
        if (verbose) warning("'date' column must be a character or a Date object")
      }

      # Stand type.
      if (!(is.character(a$stand_type) & length(a$stand_type))) {
        flag <- FALSE
        if (verbose) warning("'stand_type' column must be a single character string")
      }

      # Check lists.
      flag <- flist(a, "saplings")
      flag <- flist(a, "seedlings")

      flag <- flist(a, "trees", c("species", "dbh"))

    }


    # If 'stand_type' is 'individual' we check contents of 'trees' list elements
    if (flag) {
      for (i in a$idplot) {
        if (a$stand_type != "") {
          if (!(a$stand_type %in% c("individual", "ipm", "mpm"))) {
            flag <- FALSE
            if (verbose) warning("Wrong 'stand_type' value")
          } else {
            if (a$stand_type == "individual") {
              if (!all(colnames(a$trees) %in% c("species", "dbh"))) {
                flag <- FALSE
                if (verbose) warning("Wrong column names in 'trees'")
              }
            }
          }
        }
      }
    }
  }

  return(flag)

}
