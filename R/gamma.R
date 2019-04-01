#' @details
#' \tabular{ll}{
#'  \strong{Package:} \tab gamma \cr
#'  \strong{Type:} \tab Package \cr
#'  \strong{Version:} \tab 0.0.0.9000 \cr
#'  \strong{Date:} \tab 2019-03-28 \cr
#'  \strong{License:} \tab GPL-3 \cr
#'  \strong{DOI:} \tab \href{https://doi.org/xxx}{xxx}
#' }
#' @author
#' \strong{Full list of authors and contributors} (alphabetic order)
#'
#' \tabular{ll}{
#'  Nicolas Frerebeau \tab \emph{IRAMAT-CRP2A, Université Bordeaux Montaigne, France} \cr
#'  Guillaume Guerrin \tab \emph{IRAMAT-CRP2A, Université Bordeaux Montaigne, France} \cr
#'  Christelle Lahaye \tab \emph{IRAMAT-CRP2A, Université Bordeaux Montaigne, France} \cr
#'  Brice Lebrun \tab \emph{CEREGE, Aix-Marseille Université, France}
#' }
#'
#' \strong{Package maintainer}
#'
#' Nicolas Frerebeau\cr
#' \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#' IRAMAT-CRP2A (UMR 5060)\cr
#' Maison de l'Archéologie\cr
#' Universite Bordeaux Montaigne\cr
#' F-33607 Pessac cedex\cr
#' France
#' @note
#' This work received a state financial support managed by the Agence Nationale
#' de la Recherche (France) throught the program \emph{Investissements d'avenir}
#' (ref. ANR-10-LABX-52).
#' @name gamma-package
#' @aliases gamma-package gamma
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @importFrom graphics plot
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @import ggplot2
NULL

# Quiets concerns of R CMD check: the .'s that appear in pipelines
# See https://github.com/tidyverse/magrittr/issues/29
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))
