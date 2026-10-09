#' Simulated Spanish Inventario Forestal Nacional tree data
#'
#' Simulated adult tree data from 100 random plots of the Spanish Inventario Forestal Nacional.
#' Plot coordinates have been modified not to point to the actual plots.
#' Only three species have been selected: Pinus nigra, Pinus halepensis and Quercus ilex.
#'
#' @docType data
#'
#' @usage data(treesIFN)
#'
#' @format A data.frame with five columns containing a plot identifier "idplot",
#' a pair of UTM coordinates for the plot, "utm_x" and "utm_y", the "dbh" of the tree (in cm)
#' and its corresponding "species". EPSG is 4326.
#'
#' @keywords datasets
#'
#' @references
#' Alberdi, I., Sandoval, V., Condes, S., Cañellas, I., & Vallejo, R. (2016).
#' El Inventario Forestal Nacional español, una herramienta para el conocimiento,
#' la gestión y la conservación de los ecosistemas forestales arbolados. Ecosistemas, 25(3), 88-97.
#'
#' @source
#' https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional.html
#'
#' @examples
#' data(treesIFN)
"treesIFN"
