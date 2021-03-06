#' ECG class
#'
#' @useDynLib phyr6
#' @docType class
#' @importFrom dplyr filter
#' @importFrom dygraphs dygraph dyOptions
#' @importFrom lubridate duration
#' @importFrom magrittr %>%
#' @importFrom pracma trapz
#' @importFrom PythonInR pyCall pyConnect pyGet pyImport pyIsConnected
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#'
ECG <- R6Class("ECG",

  # Superclass
  inherit = PHYR6_BASE,

  public = list(

    # Public fields ------------------------------------------------------------

    ## ibi
    ##
    ## Numeric vector for the sequence of interbeat intervals (in milliseconds)
    ##
    ibi = NA,

    # Initialize ---------------------------------------------------------------

    ## initialize
    ##
    ## @param data numeric vector containing the ECG signal
    ## @param samplerate numeric indicating the samplerate of the signal in
    ##   \code{data}.
    ## @param marker data frame containing the markers for the signal. Column
    ##   \code{name} contains names of the markers as character strings, column
    ##   \code{position} contains the corresponding positions in samples.
    ## @param name character string indicating the name of the new object, e.g.
    ##   the subject identifier.
    ## @param path character string indicating the path where export and import
    ##   functions.
    ##
    initialize = function(data, samplerate, marker, name, path)
    {
      self$data <- data
      self$samplerate <- samplerate
      if (!missing(marker)) self$marker <- marker
      if (!missing(name)) self$name <- name
      if (!missing(path)) self$path <- path
    },

    # Extract ------------------------------------------------------------------

    ## extract_hr
    ##
    ## Extract a segment of the 'hr_interpolated' field.
    ##
    ## @param segment character string; name of the segment to extract
    ## @param latency numeric indicating the number of milliseconds to cut
    ##   before (negative) or after (negative) the segment starts
    ##
    extract_hr = function(segment, latency = 0)
    {
      if (latency != 0)
      {
        latency <- latency / 1000 * self$samplerate
      }

      # TODO: check names for single and for muliple segments
#       if (!private$find_segment(segment))
#       {
#         #stop(paste("Segment", segment, "not found"))
#         private$error_segment_not_found(segment)
#       }

      # Multiple segments
      if (grepl("\\*", segment))
      {
        # Regular expression for finding the requested segments
        pattern <- sub("_?\\*$", "_\\.\\*", segment)

        # Extract the requested segments
        segments <- private$segments[grep(pattern, names(private$segments))]
      }
      # Single segment
      else
      {
        # Extract the requested segment
        segments <- private$segments[segment]
      }

      #private$hr_interpolated[segment$start:segment$end]

      # Extract interpolated hr for all segments
      lst <- sapply(segments, function(.x) {

        if (any(is.na(.x)))
        {
          NA
        }
        else
        {
          private$hr_interpolated[(.x$start + latency):.x$end]
        }

      })

      # TODO: return as list or aggregate
      lst
    },

    # Export / Import ----------------------------------------------------------

    ## export_ecg
    ##
    ## Export the ECG signal to a text file with the name "'name'_ecg.txt" to
    ## the path specified in 'path'
    ##
    export_ecg = function()
    {
      # TODO: returns warning if file does not exist. Needed?
      path <- normalizePath(paste0(getwd(), "/", self$path, "/", self$name,
                                   "_ecg.txt"))

      export_ecg_impl(self$data, path)
    },

    ## import_ibi
    ##
    ## Import interbeat intervals from text file
    ##
    ## @param file character string specifing the filename to read. Defaults to
    ##   "'name'_ecg_ibi.txt" if argument is missing
    ##
    import_ibi = function(file)
    {
      # If no filename was specified, use default one
      if (missing(file))
      {
        filename <- paste0(getwd(), "/", self$path, "/", self$name,
                           "_ecg_ibi.txt")
      }

      # Read file
      self$ibi <- scan(filename, what = numeric(), sep = "\n", quiet = TRUE)

      private$has_ibi <- TRUE

      # TODO: spline interpolation of ibi
      #private$hr_interpolated <-
      #  60000 / private$interpolate_ibi(self$ibi, freq = self$samplerate)
      private$hr_interpolated <-
        spline(cumsum(self$ibi), 60000 / self$ibi, n = length(self$data))$y

      invisible(self)
    },

    # HR / HRV Parameters ------------------------------------------------------

    ## hr
    ##
    ## Calculate mean heart rate
    ##
    ## @param segment character string indicating the segment to report the
    ##   heart rate for.
    ## @param aggregate logical indicating whether to return hear rate for each
    ##   segment separately or an aggregated summary (mean)
    ## @param z logical indicating whether to apply z-standardisation to the
    ##   heart rate signal before calculating the segment mean
    ##
    hr = function(segment = NULL, aggregate = TRUE, z = FALSE)
    {
      private$get_measure("hr", segment, aggregate, z = z)
    },

    ## nn50
    ##
    ## Calculate the number of successive interbeat intervals that differ by
    ## more than 50 ms
    ##
    ## @param segment character string indicating the segment to report the
    ##   nn50 for.
    ##
    nn50 = function(segment = NULL, aggregate = TRUE)
    {
      private$get_measure("nn50", segment, aggregate)
    },

    ## pnn50
    ##
    ## Relative nn50 (as percent of total interbeat intervals)
    ##
    ## @param segment character string indicating the segment to report the
    ##   pnn50 for.
    ##
    pnn50 = function(segment = NULL, aggregate = TRUE)
    {
      private$get_measure("pnn50", segment, aggregate)
    },

    ## nn20
    ##
    ## Calculate the number of successive interbeat intervalss that differ by
    ## more than 20 ms
    ##
    ## @param segment character string indicating the segment to report the
    ##   nn20 for.
    ##
    nn20 = function(segment = NULL, aggregate = TRUE)
    {
      private$get_measure("nn20", segment, aggregate)
    },

    ## pnn20
    ##
    ## Relative nn20 (as percent of total interbeat intervals)
    ##
    ## @param segment character string indicating the segment to report the
    ##   pnn20 for.
    ##
    pnn20 = function(segment = NULL, aggregate = TRUE)
    {
      private$get_measure("pnn20", segment, aggregate)
    },

    ## sdnn
    ##
    ## Standard deviation of interbeat intervals
    ##
    ## @param segment character string indicating the segment to report the
    ##   sdnn for.
    ##
    sdnn = function(segment = NULL, aggregate = TRUE)
    {
      private$get_measure("sdnn", segment, aggregate)
    },

    ## rmssd
    ##
    ## Root mean square of successive differences
    ##
    ## @param segment character string indicating the segment to report the
    ##   rmssd for.
    ##
    rmssd = function(segment = NULL, aggregate = TRUE)
    {
      private$get_measure("rmssd", segment, aggregate)
    },

    ## sdsd
    ##
    ## Standard deviation of successive differences
    ##
    ## @param segment character string indicating the segment to report the
    ##   sdsd for.
    ##
    sdsd = function(segment = NULL, aggregate = TRUE)
    {
      private$get_measure("sdsd", segment, aggregate)
    },

    ## vlf
    ##
    ## Very low frequency bands
    ##
    ## TODO: Arguments for specifying band
    ##
    vlf = function(segment = NULL, lower = .0033, upper = .04, aggregate = TRUE)
    {
      private$get_measure("vlf", segment, aggregate, lower = lower,
                          upper = upper)
    },

    ## lf
    ##
    ## Low frequency bands
    ##
    lf = function(segment = NULL, lower = .04, upper = .15, aggregate = TRUE)
    {
      private$get_measure("lf", segment, aggregate, lower = lower,
                          upper = upper)
    },

    ## hf
    ##
    ## High frequency bands
    ##
    hf = function(segment = NULL, lower = .15, upper = .4, aggregate = TRUE)
    {
      private$get_measure("hf", segment, aggregate, lower = lower,
                        upper = upper)
    },

    # Plots --------------------------------------------------------------------

    ## plot_ecg
    ##
    ## Plot the ECG signal using dygraphs.
    ##
    ## @param freq numeric indicating the frequency (in Hz) used to recode the
    ##   sample. The lower the value, the higher the plotting perfomance but the
    ##   lower the signal accuracy.
    ##
    plot_ecg = function(freq = 100, marker = TRUE, segments = TRUE)
    {
      super$plot_data(freq, marker, segments)
    },

    plot_data = NULL,

    ## plot_hr
    ##
    plot_hr = function(freq = 5, marker = TRUE, segments = TRUE)
    {
      # Interpolate equal spaced time series from sequence of interbeat
      # intervals and convert to heart rate in bpm
      #data <- 60000 / private$interpolate_ibi(self$ibi, freq = freq)
      # TODO: resample data here. Change function to take data as x parameter
      data <- private$resample_data(private$hr_interpolated, freq)

      # Create dygraphs object
      plot <-
        dygraph(list(x = data$x, y = data$y)) %>%
        dyOptions(colors = "#000000", drawGrid = FALSE)

      # Add segments if requested
      if (segments && length(private$segments) > 0)
      {
        plot %<>% private$plot_add_segments()
      }

      # Add marker events if requested
      # TODO: produces an error
      if (marker && !is.na(self$marker))
      {
        plot %<>% private$plot_add_marker()
      }

      plot
    },

    ## plot_ibi
    ##
    ## Plot the interbeat interval sequence using dygraphs.
    ##
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

    # Print --------------------------------------------------------------------

    ## print
    ##
    print = function(marker = TRUE, segments = TRUE)
    {
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
        "\n         ", private$samples_to_string(length(self$data))
      )

      if (marker && !is.na(self$marker))
      {
        m <- unique(self$marker$name)
        # Sequence of names of markers in 'marker' data frame
        cat(paste("\n  Markers:", paste(m[order(m)], collapse = ", ")))
      }

      if (segments && length(private$segments) > 0)
      {
        cat("\n  Segments:")

        for (i in seq_along(private$segments))
        {
          x <- private$segments[[i]]

          cat("\n    ", x$name, " @ ", private$samples_to_hms(x$start),
              ", length: ", private$samples_to_string(x$end - x$start),
              sep = "")
        }
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

    ## has_ibi
    ##
    ## Logical indicating whether interbeat intervals are present or not
    ##
    has_ibi = FALSE,

    ## hr_interpolated
    ##
    ## Evenly sampled sequence of heart rate (in bpm), interpolated to the
    ## same frequency as the original ECG signal
    ##
    hr_interpolated = NA,

    ## py_is_connected
    ##
    ## Logical indicating whether connection to Python is established
    ##
    py_is_connected = FALSE,

    ## get_measure
    ##
    ## @param name character string
    ## @param segment character string
    ##
    get_measure = function(name, segment, aggregate, ...)
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

      # TODO: check if segment exists
      #if (!private$find_segment(name)) private$error_segment_not_found()

      # Multiple segments
      if (grepl("\\*", segment))
      {
        # Regular expression for finding the requested segments
        pattern <- sub("_?\\*$", "_\\.\\*", segment)

        # Extract the requested segments
        segments <- private$segments[grep(pattern, names(private$segments))]
      }
      # Single segment
      else
      {
        # Extract the requested segment
        segments <- private$segments[segment]
      }

      # Calculate the parameter 'name' for all segments
      measure <- sapply(segments, function(.x) {

        if (any(is.na(.x)))
        {
          NA
        }
        else
        {
          # Requested measure is heart rate
          if (name == "hr_")
          {
            private$hr_(.x$start, .x$end, ...)
          }
          # Requested measure is heart rate variability
          else
          {
            private[[name]](private$subset_ibi(.x$start, .x$end), ...)
          }
        }

      })

      # Return either aggregated measure or measures as named vector
      if (aggregate) mean(measure) else measure
    },

    # HEART RATE

    ## hr
    ##
    ## @param data numeric vector with sequence of interbeat intervals
    ##
    hr_ = function(start, end, z)
    {
      if (!z)
      {
        mean(private$hr_interpolated[start:end])
      }
      else
      {
        mean(scale(private$hr_interpolated)[, 1][start:end])
      }

    },

    # HEART RATE VARIABILITY: TIME DOMAIN MEASURES

    nn50_  = function(x) { sum(diff(x) > 50)               },
    pnn50_ = function(x) { private$nn50_(x) / length(x)    },
    nn20_  = function(x) { sum(diff(x) > 20)               },
    pnn20_ = function(x) { private$nn20_(x) / length(x)    },
    sdnn_  = function(x) { sd(x)                           },
    rmssd_ = function(x) { sqrt(mean(diff(x) ^ 2))         },
    sdsd_  = function(x) { sd(diff(x))                     },

    # HEART RATE VARIABILITY: FREQUENCY DOMAIN MEASURES

    vlf_ = function(x, lower, upper) { private$psd(x, lower, upper) },
    lf_  = function(x, lower, upper) { private$psd(x, lower, upper) },
    hf_  = function(x, lower, upper) { private$psd(x, lower, upper) },

    # HELPER FUNCTIONS

    ## subset_ibi
    ##
    ## @param from numeric indicating the start position of the subset in
    ##   samples
    ## @param to numeric indicating the end position of the sumbset in samples
    ##
    subset_ibi = function(from, to)
    {
      from_ibi <- which(cumsum(self$ibi / 1000) > from / self$samplerate)[1]
      to_ibi   <- which(cumsum(self$ibi / 1000) > to   / self$samplerate)[1]

      self$ibi[from_ibi:to_ibi]
    },

    ## interpolate_ibi
    ##
    ## Since the series of interbeat intervals is an unevenly sampled signal,
    ## it must be interpolated before computing frequency power bands using fft.
    ##
    interpolate_ibi = function(ibi, method = "linear", freq = 4)
    {
      # Sample times in seconds is needed for interpolation
      x = cumsum(ibi) / 1000

      approx(x, y = ibi, xout = seq(head(x, 1), tail(x, 1), 1 / freq),
             method = method)$y
    },

    ## py_connect
    ##
    py_connect = function()
    {
      if (!pyIsConnected())
      {
        pyConnect()

        pyImport(import = "signal", from = "scipy")
        pyImport(import = "numpy", as = "np")

        py_started <- TRUE
      }
    },

    ## welch
    ##
    ## Estimate the power spectral density using Welch's method
    ##
    welch = function(x, fs, window = "hanning", nperseg = 256, noverlap = 128,
                     nfft = 256, detrend = "linear", return_onesided = TRUE,
                     scaling = "density")
    {
      # Ensure connection to Python is established
      if (!private$py_is_connected) private$py_connect()

      # Call scipy.signal.welch from Python
      wout <- pyCall("signal.welch", kwargs = list(
        x = x, fs = fs, window = window, nperseg = nperseg, noverlap = noverlap,
        nfft = nfft, detrend = detrend, return_onesided = return_onesided,
        scaling = scaling
      ))

      # Retrieve results from Python
      f <- pyGet(sprintf('list(__R__.namespace[%i])', wout[[1]]$id))
      pxx <- pyGet(sprintf('list(__R__.namespace[%i])', wout[[2]]$id))

      data.frame(f = f, pxx = pxx)
    },

    ## psd
    ##
    psd = function(x, lower = NULL, upper = NULL)
    {
      # Calculates the power spectrum density from the interpolated series of
      # interbeat intervals
      spectrum <- private$welch(private$interpolate_ibi(x))

      # Extract a specific power band if specified
      if (!is.null(lower) && !is.null(upper))
      {
        spectrum <- dplyr::filter(spectrum, f >= lower, f <= upper)
      }

      trapz(spectrum$f, spectrum$pxx)
    }

  )
)
