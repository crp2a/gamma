% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/operators.R
\docType{methods}
\name{operator}
\alias{operator}
\alias{Arith,GammaSpectrum,GammaSpectrum-method}
\alias{Arith,GammaSpectrum,numeric-method}
\alias{Compare,GammaSpectrum,GammaSpectrum-method}
\alias{Compare,GammaSpectrum,numeric-method}
\alias{Logic,GammaSpectrum,GammaSpectrum-method}
\alias{Logic,GammaSpectrum,numeric-method}
\alias{Logic,GammaSpectrum,logical-method}
\alias{Math,GammaSpectrum-method}
\alias{Math2,GammaSpectrum-method}
\alias{Summary,GammaSpectrum-method}
\title{Common Operations on GammaSpectrum Objects}
\usage{
\S4method{Arith}{GammaSpectrum,GammaSpectrum}(e1, e2)

\S4method{Arith}{GammaSpectrum,numeric}(e1, e2)

\S4method{Compare}{GammaSpectrum,GammaSpectrum}(e1, e2)

\S4method{Compare}{GammaSpectrum,numeric}(e1, e2)

\S4method{Logic}{GammaSpectrum,GammaSpectrum}(e1, e2)

\S4method{Logic}{GammaSpectrum,numeric}(e1, e2)

\S4method{Logic}{GammaSpectrum,logical}(e1, e2)

\S4method{Math}{GammaSpectrum}(x)

\S4method{Math2}{GammaSpectrum}(x, digits)

\S4method{Summary}{GammaSpectrum}(x, ..., na.rm = FALSE)
}
\arguments{
\item{x, e1, e2}{An object (typically a \linkS4class{GammaSpectrum} object).}

\item{digits}{A length-one \code{\link{numeric}} vector giving the number of digits to
be used in \code{\link[=round]{round()}} or \code{\link[=signif]{signif()}}.}

\item{...}{Further arguments passed to or from methods.}

\item{na.rm}{A \code{\link{logical}} scalar: should missing values (including \code{NaN}) be
omitted from the calculations?}
}
\description{
Performs common operations on \code{GammaSpectrum} objects.
}
\section{Group Generics}{

\linkS4class{GammaSpectrum} objects have support for S4 group generic
functionality to operate within elements across objects:
\describe{
\item{\code{Arith}}{"\code{+}", "\code{-}", "\code{*}", "\code{^}", "\verb{\\\%\\\%}", "\verb{\\\%/\\\%}", "\code{/}"}
\item{\code{Compare}}{"\code{==}", "\code{>}", "\code{<}", "\code{!=}", "\code{<=}", "\code{>=}"}
\item{\code{Logic}}{"\code{&}", "\code{|}"}
\item{\code{Math}}{"\code{abs}", "\code{sign}", "\code{sqrt}", "\code{ceiling}", "\code{floor}",
"\code{trunc}", "\code{cummax}", "\code{cummin}", "\code{cumprod}", "\code{cumsum}", "\code{log}",
"\code{log10}", "\code{log2}", "\code{log1p}", "\code{acos}", "\code{acosh}", "\code{asin}", "\code{asinh}",
"\code{atan}", "\code{atanh}", "\code{exp}", "\code{expm1}", "\code{cos}", "\code{cosh}", "\code{cospi}",
"\code{sin}", "\code{sinh}", "\code{sinpi}", "\code{tan}", "\code{tanh}", "\code{tanpi}", "\code{gamma}",
"\code{lgamma}", "\code{digamma}", "\code{trigamma}"}
\item{\code{Math2}}{"\code{round}", "\code{signif}"}
\item{\code{Ops}}{"\code{Arith}", "\code{Compare}", "\code{Logic}"}
\item{\code{Summary}}{"\code{min}", "\code{max}", "\code{range}", "\code{prod}", "\code{sum}", "\code{any}",
"\code{all}"}
}
}

\examples{
## No examples
}
\author{
N. Frerebeau
}
\concept{operator}
