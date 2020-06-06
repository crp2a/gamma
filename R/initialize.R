# INITIALIZE
#' @include AllClasses.R

# ================================================================ GammaSpectrum
# /!\ initialize() GammaSpectrum retains copy construction
setMethod(
  f = "initialize",
  signature = "GammaSpectrum",
  definition = function(
    .Object, ..., hash = .Object@hash, name = .Object@name,
    date = .Object@date, instrument = .Object@instrument,
    file_format = .Object@file_format, chanel = .Object@chanel,
    energy = .Object@energy, count = .Object@count, rate = .Object@rate,
    live_time = .Object@live_time, real_time = .Object@real_time,
    calibration = .Object@calibration
  ) {

    if (length(date) == 0) date <- Sys.time()
    if (length(chanel) != 0) chanel <- as.integer(chanel)
    if (length(count) > 0 && length(live_time) == 1) rate <- count / live_time

    methods::callNextMethod(
      .Object, ..., hash = hash, name = name, date = date,
      instrument = instrument, file_format = file_format, chanel = chanel,
      energy = energy, count = count, rate = rate, live_time = live_time,
      real_time = real_time, calibration = calibration
    )
  }
)

setMethod(
  f = "initialize",
  signature = "Baseline",
  definition = function(
    .Object, ..., hash = .Object@hash, name = .Object@name,
    date = .Object@date, instrument = .Object@instrument,
    file_format = .Object@file_format, chanel = .Object@chanel,
    energy = .Object@energy, count = .Object@count, rate = .Object@rate,
    live_time = .Object@live_time, real_time = .Object@real_time,
    calibration = .Object@calibration, method = .Object@method
  ) {

    .Object <- methods::callNextMethod(
      .Object, ..., hash = hash, name = name, date = date,
      instrument = instrument, file_format = file_format, chanel = chanel,
      energy = energy, count = count, rate = rate, live_time = live_time,
      real_time = real_time, calibration = calibration
    )
    .Object@method <- method
    .Object
  }
)

# ================================================================= GammaSpectra
setMethod(
  f = "initialize",
  signature = "GammaSpectra",
  definition = function(.Object, ...) {
    .Object <- methods::callNextMethod(.Object, ...)
    # Get spectrum names
    spc_list <- .Object@.Data
    spc_ref <- vapply(X = spc_list, FUN = "[[", FUN.VALUE = character(1),
                      i = "name")
    spc_ref <- make.unique(spc_ref, sep = "_")
    names(spc_list) <- spc_ref
    .Object@.Data <- spc_list

    methods::validObject(.Object)
    return(.Object)
  }
)

# ================================================================= PeakPosition
# /!\ initialize() PeakPosition retains copy construction
setMethod(
  f = "initialize",
  signature = "PeakPosition",
  definition = function(
    .Object, ..., hash = .Object@hash, noise_method = .Object@noise_method,
    noise_threshold = .Object@noise_threshold, window = .Object@window,
    chanel = .Object@chanel, energy = .Object@energy
  ) {
    if (length(chanel) != 0)
      chanel <- as.integer(chanel)

    methods::callNextMethod(
      .Object, ..., hash = hash, noise_method = noise_method,
      noise_threshold = noise_threshold, window = window, chanel = chanel,
      energy = energy
    )
  }
)
