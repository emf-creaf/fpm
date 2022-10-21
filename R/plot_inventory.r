plot_inventory <- function(a, colname = "geometry") {


  if (colname == "geometry") {
    plot(sf::st_geometry(a))
  } else {
    plot(a[, colname])
  }
}
