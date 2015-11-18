#' ECG class
#'
#' @docType class
#' @importFrom dygraphs dygraph dyOptions
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#'
ECG <- R6Class("ECG",

  # Superclass
  inherit = PHYR6_BASE,

  public = list(

    # Public fields ------------------------------------------------------------

    #' ibi
    #'
    #' Numeric vector for the sequence of interbeat intervals
    #'
    ibi = NA,

    # Initialize ---------------------------------------------------------------

    #' initialize
    #'
    #' @param data numeric vector containing the ECG signal
    #' @param samplerate numeric indicating the samplerate of the signal in
    #'   \code{data}.
    #' @param marker numeric vector containing the markers for the signal. Must
    #'   have the same length as \code{data}.
    #' @param name character string indicating the name of the new object, e.g.
    #'   the subject identifier.
    #' @param path character string indicating the path where export and import
    #'   functions.
    #'
    initialize = function(data, samplerate, marker, name, path)
    {
      self$data <- data
      self$samplerate <- samplerate
      if (!missing(marker)) self$marker <- marker
      if (!missing(name)) self$name <- name
      if (!missing(path)) self$path <- path
    },

    # Filtering -----------------------------------------------------------------

    #' filter
    #'
    #' Apply a signal filter to the ECG signal, e.g. signal::butter()
    #'
    #' @param filt an object of class ARMA
    #'
    filter = function(filt)
    {
      self$data <- as.numeric(signal::filter(filt, self$data))
      invisible(self)
    },

    # Export / Import -----------------------------------------------------------

    #' export_ecg
    #'
    #' Export the ECG signal to a text file with the name "'name'_ecg.txt" to
    #' the path specified in 'path'
    #'
    export_ecg = function()
    {
      write.table(
        matrix(self$data),
        file = paste0(getwd(), "/", self$path, "/", self$name, "_ecg.txt"),
        row.names = FALSE, col.names = FALSE
      )
    },

    #' import_ibi
    #'
    #' Import interbeat intervals from text file
    #'
    #' @param file character string specifing the filename to read. Defaults to
    #'   "'name'_ecg_ibi.txt" if argument is missing
    #'
    import_ibi = function(file)
    {
      if (missing(file))
      {
        filename <- paste0(getwd(), "/", self$path, "/", self$name,
                           "_ecg_ibi.txt")
      }

      self$ibi <- scan(filename, what = numeric(), sep = "\n", quiet = TRUE)

      private$has_ibi <- TRUE

      invisible(self)
    },

    # HR / HRV Parameters -------------------------------------------------------

    #' hr
    #'
    #' Calculate mean heart rate
    #'
    #' @param segment character string indicating the segment to report the
    #'   heart rate for.
    #'
    hr = function(segment = NULL) { private$get_measure("hr", segment) },

    #' nn50
    #'
    #' Calculate the number of successive interbeat intervals that differ by
    #' more than 50 ms
    #'
    #' @param segment character string indicating the segment to report the
    #'   nn50 for.
    #'
    nn50 = function(segment = NULL) { private$get_measure("nn50", segment) },

    #' pnn50
    #'
    #' Relative nn50 (as percent of total interbeat intervals)
    #'
    #' @param segment character string indicating the segment to report the
    #'   pnn50 for.
    #'
    pnn50 = function(segment = NULL) { private$get_measure("pnn50", segment) },

    #' nn20
    #'
    #' Calculate the number of successive interbeat intervalss that differ by
    #' more than 20 ms
    #'
    #' @param segment character string indicating the segment to report the
    #'   nn20 for.
    #'
    nn20 = function(segment = NULL) { private$get_measure("nn20", segment) },

    #' pnn20
    #'
    #' Relative nn20 (as percent of total interbeat intervals)
    #'
    #' @param segment character string indicating the segment to report the
    #'   pnn20 for.
    #'
    pnn20 = function(segment = NULL) { private$get_measure("pnn20", segment) },

    #' sdnn
    #'
    #' Standard deviation of interbeat intervals
    #'
    #' @param segment character string indicating the segment to report the
    #'   sdnn for.
    #'
    sdnn = function(segment = NULL) { private$get_measure("sdnn", segment) },

    #' rmssd
    #'
    #' Root mean square of successive differences
    #'
    #' @param segment character string indicating the segment to report the
    #'   rmssd for.
    #'
    rmssd = function(segment = NULL) { private$get_measure("rmssd", segment) },

    #' sdsd
    #'
    #' Standard deviation of successive differences
    #'
    #' @param segment character string indicating the segment to report the
    #'   sdsd for.
    #'
    sdsd = function(segment = NULL) { private$get_measure("sdsd", segment) },

    # Plots ---------------------------------------------------------------------

    #' plot_ecg
    #'
    #' Plot the ECG signal using dygraphs.
    #'
    #' @param freq numeric indicating the frequency (in Hz) used to recode the
    #'   sample. The lower the value, the higher the plotting perfomance but the
    #'   lower the signal accuracy.
    #'
    plot_ecg = function(freq)
    {
      super$plot_data(freq)
    },

    plot_data = NULL,

#     plot_ecg = function(freq = 100)
#     {
#       dygraphs::dygraph(private$resample_data(freq))
#     },

    #' plot_ibi
    #'
    #' Plot the interbeat interval sequence using dygraphs.
    #'
    plot_ibi = function()
    {
      if (private$has_ibi)
      {
        dygraph(list(x = seq_along(self$ibi), y = self$ibi)) %>%
          dyOptions(drawGrid = FALSE)
      }
      else
      {
        stop("Load interbeat intervals using import_ibi() first")
      }
    },

    # Print ---------------------------------------------------------------------

    #' print
    #'
    print = function()
    {
      # Character representation of the time length of the signal
      time <-
        (length(self$data) / self$samplerate) %>%
        round(2) %>%
        lubridate::duration() %>%
        as.character()

      cat(
        # Class name
        "<ECG>",
        # Object name (e.g. subject indentifier)
        "\n  Name:", self$name,
        # Samplerate in Hertz
        "\n  Sample rate:", self$samplerate, "Hz",
        # Length of the signal in samples
        "\n  Length:", length(self$data), "samples",
        # Length of the signal (hh:mm:ss)
        "\n         ", time
      )
      if (length(self$marker) > 1)
      {
        m <- unique(self$marker)
        # Sequence of markers in 'marker' field
        cat(paste("\n  Markers:", paste(m[order(m)], collapse = ", ")))
      }
      if (length(self$ibi) > 1)
      {
        cat(
          "\n  Heart rate:", round(self$hr(), 2), "[bpm]",
          "\n  Heart rate variability:",
          "\n    Time domain measures:",
          "\n      NN50:", self$nn50(), "[count]",
          "\n      pNN50:", round(self$pnn50() * 100, 2), "[%]",
          "\n      NN20:", self$nn20(), "[count]",
          "\n      pNN20:", round(self$pnn20() * 100, 2), "[%]",
          "\n      SDNN:", round(self$sdnn(), 2), "[ms]",
          "\n      RMSSD:", round(self$rmssd(), 2), "[ms]",
          "\n      SDSD:", round(self$sdsd(), 2), "[ms]"
        )
      }
    }

  ),

  # Private functions ----------------------------------------------------------

  private = list(

    #' has_ibi
    #'
    #' Logical indicating whether interbeat intervals are present or not
    #'
    has_ibi = FALSE,

    #' get_measure
    #'
    #' @param name character string
    #' @param segment character string
    #'
    get_measure = function(name, segment)
    {
      # Add underscore to function name, because all private parameter functions
      # end with one. This is later used to call the parameter function
      name <- paste0(name, "_")

      # Return NA if no interbeat intervals are present
      if (!private$has_ibi)
      {
        return(NA)
      }

      # If no segment was specified, apply the function to the complete
      # sequence of interbeat intervals
      if (is.null(segment))
      {
        return(private[[name]](self$ibi))
      }

      # Multiple segments
      if (grepl("\\*", segment))
      {
        # Regular expression for finding the requested segments
        pattern <- sub("_?\\*$", "_\\.\\*", segment)

        # Extract the requested segments
        segments <- self$segments[grep(pattern, names(self$segments))]
      }
      # Single segment
      else
      {
        # Extract the requested segment
        segments <- self$segments[[segment]]
      }

      # Calculate the parameter 'name' for all segments
      sapply(segments, function(.x) {

        if (any(is.na(.x)))
        {
          NA
        }
        else
        {
          private[[name]](private$subset_ibi(.x[1], .x[2]))
        }

      })
    },

    # HEART RATE

    #' hr
    #'
    #' @param data numeric vector with sequence of interbeat intervals
    #'
    hr_ = function(data) { 60000 / mean(data) },

    # HEART RATE VARIABILITY: TIME DOMAIN MEASURES

    nn50_  = function(data) { sum(diff(data) > 50)               },
    pnn50_ = function(data) { private$nn50_(data) / length(data) },
    nn20_  = function(data) { sum(diff(data) > 20)               },
    pnn20_ = function(data) { private$nn20_(data) / length(data) },
    sdnn_  = function(data) { sd(data)                           },
    rmssd_ = function(data) { sqrt(mean(diff(data) ^ 2))         },
    sdsd_  = function(data) { sd(diff(data))                     },

    #' subset_ibi
    #'
    #' @param from numeric indicating the start position of the subset in
    #'   samples
    #' @param to numeric indicating the end position of the sumbset in samples
    #'
    subset_ibi = function(from, to)
    {
      from_ibi <- which(cumsum(self$ibi / 1000) > from / self$samplerate)[1]
      to_ibi   <- which(cumsum(self$ibi / 1000) > to   / self$samplerate)[1]

      self$ibi[from_ibi:to_ibi]
    }

  )
)

# # Since the series of interbeat intervals is an unevenly sampled signal,
# # it must be interpolated before computing frequency power bands using fft.
# interp_ibi_ts = function(method = "linear", freq = 4, coerce = NULL)
# {
#   # Sample times in seconds is needed for interpolation
#   x = cumsum(self$ibi) / 1000
#
#   interp_ibi <- approx(x, y = self$ibi,
#                        xout = seq(head(x, 1), tail(x, 1), 1 / freq),
#                        method = method)$y
#
#   if (!is.null(coerce))
#   {
#     do.call(as.character(coerce), list(x = interp_ibi))
#   }
#   else
#   {
#     interp_ibi
#   }
# }
