% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/smooth.R,
%   R/smooth_rectangular.R, R/smooth_savitzky.R, R/smooth_triangular.R
\docType{methods}
\name{smooth}
\alias{smooth}
\alias{signal_smooth}
\alias{signal_smooth-method}
\alias{smooth_rectangular}
\alias{smooth_rectangular-method}
\alias{smooth_triangular}
\alias{smooth_triangular-method}
\alias{smooth_savitzky}
\alias{smooth_savitzky-method}
\alias{signal_smooth,GammaSpectrum-method}
\alias{signal_smooth,GammaSpectra-method}
\alias{smooth_rectangular,GammaSpectrum-method}
\alias{smooth_rectangular,GammaSpectra-method}
\alias{smooth_savitzky,GammaSpectrum-method}
\alias{smooth_savitzky,GammaSpectra-method}
\alias{smooth_triangular,GammaSpectrum-method}
\alias{smooth_triangular,GammaSpectra-method}
\title{Smooth}
\usage{
signal_smooth(object, ...)

smooth_rectangular(object, ...)

smooth_triangular(object, ...)

smooth_savitzky(object, ...)

\S4method{signal_smooth}{GammaSpectrum}(object, method = c("rectangular", "triangular", "savitzky"), ...)

\S4method{signal_smooth}{GammaSpectra}(object, method = c("rectangular", "triangular", "savitzky"), ...)

\S4method{smooth_rectangular}{GammaSpectrum}(object, m = 3, ...)

\S4method{smooth_rectangular}{GammaSpectra}(object, m = 3, ...)

\S4method{smooth_savitzky}{GammaSpectrum}(object, m = 3, p = 2, ...)

\S4method{smooth_savitzky}{GammaSpectra}(object, m = 3, p = 2, ...)

\S4method{smooth_triangular}{GammaSpectrum}(object, m = 3, ...)

\S4method{smooth_triangular}{GammaSpectra}(object, m = 3, ...)
}
\arguments{
\item{object}{A \linkS4class{GammaSpectrum} or \linkS4class{GammaSpectra}
object.}

\item{...}{Extra parameters to be passed to further methods.}

\item{method}{A \code{\link{character}} string specifying the smoothing method to be
used. It must be one of "\code{unweighted}" (default), "\code{weighted}" or
"\code{savitzky}" (see details). Any unambiguous substring can be given.}

\item{m}{An odd \code{\link{integer}} giving the number of adjacent points to be used.}

\item{p}{An \code{\link{integer}} giving the polynomial degree. Only used if \code{method}
is "\code{savitzky}".}
}
\value{
A \linkS4class{GammaSpectrum} or \linkS4class{GammaSpectra} object.
}
\description{
Smoothes intensities.
}
\details{
The following smoothing methods are available:
\describe{
\item{\code{rectangular}}{Unweighted sliding-average or rectangular smooth.
It replaces each point in the signal with the average of \eqn{m} adjacent
points.}
\item{\code{triangular}}{Weighted sliding-average or triangular smooth.
It replaces each point in the signal with the weighted mean of \eqn{m}
adjacent points.}
\item{\code{savitzky}}{Savitzky-Golay filter. This method is based on the
least-squares fitting of polynomials to segments of \eqn{m} adjacent
points.}
}
There will be \eqn{(m - 1) / 2} points both at the beginning and at the end
of the spectrum for which a complete \eqn{m}-width smooth cannot be
calculated. To prevent data loss, progressively smaller smooths are used at
the ends of the spectrum if \code{method} is \code{unweighted} or \code{weighted}. If the
Savitzky-Golay filter is used, the original \eqn{(m - 1) / 2} points at the
ends of the spectrum are preserved.
}
\examples{
# Import CNF files
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
spc <- read(spc_file)
spc <- signal_slice(spc, -c(1:35))

# Plot raw spectrum
spc_clean <- signal_correct(spc)
plot(spc_clean)

# Rectangular smooth
spc_unweighted <- smooth_rectangular(spc, m = 3)
spc_unweighted_clean <- signal_correct(spc_unweighted)
plot(spc_unweighted_clean)

# Triangular smooth
spc_weighted <- smooth_triangular(spc, m = 5)
spc_weighted_clean <- signal_correct(spc_weighted)
plot(spc_weighted_clean)

# Savitzky–Golay
spc_savitzky <- smooth_savitzky(spc, m = 21, p = 2)
spc_savitzky_clean <- signal_correct(spc_savitzky)
plot(spc_savitzky_clean)
}
\references{
Gorry, P. A. (1990). General Least-Squares Smoothing and Differentiation by
the Convolution (Savitzky-Golay) Method. \emph{Analytical Chemistry}, 62(6),
p. 570-573. \doi{10.1021/ac00205a007}.

Savitzky, A. & Golay, M. J. E. (1964). Smoothing and Differentiation of
Data by Simplified Least Squares Procedures. \emph{Analytical Chemistry},
36(8), p. 1627-1639. \doi{10.1021/ac60214a047}.
}
\seealso{
Other signal processing: 
\code{\link{baseline}},
\code{\link{peaks_find}()},
\code{\link{peaks_search}()},
\code{\link{signal_integrate}()},
\code{\link{signal_slice}()},
\code{\link{signal_split}()},
\code{\link{signal_stabilize}()}
}
\author{
N. Frerebeau
}
\concept{signal processing}
