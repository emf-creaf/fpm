#' Maximum diameter at breast height per tree species.
#'
#' @description
#' Maximum diameter at breast height (dbh) for twenty tree pecies present in the Spanish
#' Inventario Forestal Nacional. Fifteen tree species are referred to by their full
#' latin name, whereas five others represent functional groups.
#'
#' @docType data
#'
#' @usage data(maxdbh)
#'
#' @format
#' A named vector, where elements are named by the species.
#'
#' @keywords datasets
#'
#' @details
#' Data have been obtained mostly from English Wikipedia (WK),
#' the "Arbres Monumentals. Tresors Naturals dels Ports" poster (AM),
#' the IFN2 and IFN3 National Inventories (IFN),
#' the http://www.monumentaltrees.com web (MT) or
#' the "Boscos singulars de Catalunya" database (BS).
#' We give here the largest of the five.
#'
#' Not all IFN2/IFN3 species have been included. There are 15 individual
#' tree species with a major presence in the Inventories whose maximum dbh are provided.
#' The remaining species have been
#' grouped as "Plantation", "Sclerophyll", "Conifer", "Deciduous" and "Riparian" and their
#' maximum dbh's correspond to
#'
#' When data is not available because species have been grouped under a generic name (e.g. "Conifer")
#' maximum dbh is calculated from the Spanish Inventories IFN2 or IFN3.
#'
#' @references
#' * https://www.wikipedia.org/
#' * https://parcsnaturals.gencat.cat/ca/detalls/Article/06-Arbres-monumentals
#' * https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional.html
#' * https://www.monumentaltrees.com/
#' * http://www.creaf.uab.es/boscossingulars/
#'
#' @examples
#' load("maxdbh")
#'

"maxdbh"
