#' @details
#' \tabular{ll}{
#'  \strong{Package:} \tab gamma \cr
#'  \strong{Type:} \tab Package \cr
#'  \strong{Version:} \tab 0.1.0.9000 \cr
#'  \strong{Date:} \tab 2019-04-26 \cr
#'  \strong{License:} \tab GPL-3 \cr
#'  \strong{DOI:} \tab \href{https://doi.org/10.5281/zenodo.2652393}{10.5281/zenodo.2652393}
#' }
#' @author
#' \strong{Full list of authors and contributors} (alphabetic order)
#'
#' \tabular{ll}{
#'  Nicolas Frerebeau \tab \emph{Université Bordeaux Montaigne, France} \cr
#'  Guillaume Guérin \tab \emph{CNRS, France} \cr
#'  Christelle Lahaye \tab \emph{Université Bordeaux Montaigne, France} \cr
#'  Brice Lebrun \tab \emph{Aix-Marseille Université, France} \cr
#'  Guilhem Paradol \tab \emph{Université Bordeaux Montaigne, France}
#' }
#'
#' \strong{Package maintainer}
#'
#' Nicolas Frerebeau\cr
#' \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#' IRAMAT-CRP2A (UMR 5060)\cr
#' Maison de l'Archéologie\cr
#' Université Bordeaux Montaigne\cr
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

#' @importFrom methods new
#' @importFrom graphics plot
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

# Quiets concerns of R CMD check: the .'s that appear in pipelines
# See https://github.com/tidyverse/magrittr/issues/29
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))
