#' Simulated Spanish Inventario Forestal Nacional sapling data
#'
#' Simulated sapling data from 100 random plots of the Spanish Inventario Forestal Nacional.
#' Tree saplings are defined as any plant heigher than 130cm and dbh within the interval
#' 2.5-7.5 cm.
#' Only three species have been included: Pinus nigra, Pinus halepensis and Quercus ilex.
#'
#' @docType data
#'
#' @usage data(IFNsaplings)
#'
#' @format A data.frame with three columns containing a plot identifier "idplot",
#' a "n" column with the number of saplings present (from 0 to infinite)
#' and a "species" column.
#'
#'
#' @keywords datasets
#'
#' @references
#'
#' @source
#'
#' @examples
#' load("IFNsaplings")
#'

"IFNsaplings"
