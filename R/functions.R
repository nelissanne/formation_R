# DEFINITION DE FONCTIONS ---------------

decennie_a_partir_annee <- function(annee) {
  return(annee - annee %%
           10)
}

#' Sortir un indicateur statistique pour une série donnée
#'
#' @param serie A list of numeric values
#' @param indicateur "moyenne", "sd", "ecart-type", "variance"
#' @param ... kwargs
#'
#' @return A numeric value
#'
#' @examples 
#' indicateur_stat(rnorm(10))
#' indicateur_stat(rnorm(10), "ecart-type")
#' indicateur_stat(rnorm(10), "variance")

indicateur_stat <- function(serie, indicateur = "moyenne", ...) {
  if (indicateur == "moyenne") {
    x <- mean(serie, na.rm = TRUE, ...)
  } else if (indicateur == "ecart-type" || indicateur == "sd") {
    x <- sd(serie, na.rm = TRUE, ...)
  } else if (indicateur == "variance") {
    x <- var(serie, na.rm = TRUE, ...)
  }
  return(x)
}