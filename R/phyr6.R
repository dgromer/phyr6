#' phyr6 base class
#'
#' @docType class
#' @importFrom dplyr bind_rows data_frame
#' @importFrom dygraphs dyEvent dygraph dyOptions dyShading
#' @importFrom magrittr %<>% %>%
#' @importFrom R6 R6Class
#' @importFrom signal butter
#' @format An \code{\link{R6Class}} generator object
#'
PHYR6_BASE <- R6Class("PHYR6_BASE",

  public = list(

    # Public fields ------------------------------------------------------------

    ## data
    ##
    ## Numeric vector for the physiological signal
    ##
    data = NA,

    ## samplerate
    ##
    ## Numeric of length one indicating the samplerate of the signal in 'data'
    ##
    samplerate = NA,

    ## marker
    ##
    ## Data frame with columns "name" (character) and "position" (numeric)
    ## indicating the name and the position (in samples) of markers
    ##
    marker = NA,

    ## name
    ##
    ## Character string indicating the name of the signal (e.g. subject id)
    ##
    name = NA,

    ## path
    ##
    ## Character string indicating the path where export and import functions
    ## act.
    ##
    path = NA,

    # Segmentation -------------------------------------------------------------

    ## segment_marker
    ##
    ## Create a segment for given markers
    ##
    ## @param from numeric indicating the start marker for segment(s)
    ## @param to numeric indicating the end marker for segment(s)
    ## @param name character string indicating the name for the segment
    ## @param check logical indicating whether to stop if a marker was not found
    ##   (\code{TRUE}) or create the segment with content \code{NA}
    ##   (\code{FALSE})
    ##
    segment_marker = function(from = NULL, to = NULL, name, check = TRUE)
    {
      # Check if there is already a segment with this name
      if (private$find_segment(name)) private$error_segment(name)

      # Check if markers 'from' and 'to' are present in marker vector
      if (check)
      {
        # If markers were not found, return an error message
        if (!private$find_marker(from)) private$error_marker(from)
        if (!private$find_marker(to))   private$error_marker(to)
      }
      # Set the return value to NA if one of the markers was not found
      else
      {
        # If check is FALSE and markers are not found, set segment to NA
        if (!private$find_marker(from) || !private$find_marker(to))
        {
          self$segments[[name]] <- NA
          return()
        }
      }

      # Get index/indizes of 'from' marker(s)
      if (!is.null(from))
      {
        #from <- self$marker[self$marker$name == from, "position"]
        from <- self$marker %>% `[.data.frame`(.$name == from, "position")
      }
      else
      {
        from <- 1
      }

      # Get index/indizes of 'to' marker(s)
      if(!is.null(to))
      {
        #to <- self$marker[self$marker$name == to, "position"]
        to <- self$marker %>% `[.data.frame`(.$name == to, "position")
      }
      else
      {
        to <- length(self$data)
      }

      if (length(from) > 1 && length(to) > 1)
      {
        private$create_segments(from, to, name)
      }
      else
      {
        private$create_segment(from, to, name)
      }
    },

    ## segment_samples
    ##
    ## Create a segment based on given samples
    ##
    ## @param from numeric indicating the start position for the segment in
    ##   samples
    ## @param to numeric indicating the end position for the segment in samples
    ## @param name character string indicating the name for the segment
    segment_samples = function(from = 0, to = length(self$data), name)
    {
      private$create_segment(from, to, name)
    },

    add_marker = function(name, position)
    {
      self$marker %<>% bind_rows(data_frame(name = name, position = position))
    },

    delete_marker = function(x)
    {
      for(i in seq_along(x))
      {
        self$marker %<>% filter(name != x[i])
      }
    },

    extract = function(segment)
    {
      if (!private$find_segment(segment))
      {
        stop(paste("Segment", segment, "not found"))
      }

      segment <- private$segments[[segment]]

      self$data[segment$start:segment$end]
    },

    # Filtering ----------------------------------------------------------------

    ## filter
    ##
    ## Apply a signal filter to the ECG signal, e.g. signal::butter()
    ##
    ## @param filt an object of class ARMA
    ##
    filter = function(filt)
    {
      self$data <- as.numeric(signal::filter(filt, self$data))
      invisible(self)
    },

    highpass_filter = function(freq, unit = c("hertz", "nyquist"), order = 2)
    {
      private$filter_template("high", freq, unit, order)
    },

    lowpass_filter = function(freq, unit = c("hertz", "nyquist"), order = 2)
    {
      private$filter_template("low", freq, unit, order)
    },

    bandpass_filter = function(freq, unit = c("hertz", "nyquist"), order = 2)
    {
      private$filter_template("pass", freq, unit, order)
    },

    bandstop_filter = function(freq, unit = c("hertz", "nyquist"), order = 2)
    {
      private$filter_template("stop", freq, unit, order)
    },

    # Plotting -----------------------------------------------------------------

    ## plot_data
    ##
    ## Plot the 'data' field using the dygraphs package.
    ##
    ## @param freq numeric indicating the sample frequency (in Hertz) of the
    ##   signal to be plotted.
    ## @param marker logical. Plot markers?
    ## @param segments logical. Plot segments?
    ##
    plot_data = function(freq = 5, marker = TRUE, segments = TRUE)
    {
      # Downsample data for improved plotting performance
      data <- private$resample_data(freq)

      # Create dygraphs object
      plot <-
        dygraph(list(x = data$x, y = data$y)) %>%
        dyOptions(colors = "#000000", drawGrid = FALSE)

      # Add segments if requested
      if (segments && length(private$segments) != 0)
      {
        plot %<>% private$plot_add_segments()
      }

      # Add marker events if requested
      if (marker && !is.na(self$marker))
      {
        plot %<>% private$plot_add_marker()
      }

      plot
    }

  ),

  private = list(

    ## segments
    ##
    ## List containing the specified segments (of class PHYR6_SEGMENTS)
    ##
    segments = list(),

    ## find_marker
    ##
    ## Searches for a given marker in the marker vector
    ##
    ## @param marker numeric marker to search
    ## @return boolean \code{TRUE} if the marker was found or \code{FALSE} if
    ##   not
    ##
    find_marker = function(marker)
    {
      !is.null(marker) && any(self$marker$name == marker)
    },

    ## error_marker
    ##
    ## Throws an error that a marker could not be found
    ##
    ## @param marker numeric marker to report an error for
    ##
    error_marker = function(marker)
    {
      stop(paste(if (!is.na(self$name)) sprintf("%s:", self$name),
                 "Could not find marker", marker), call. = FALSE)
    },

    ## find_segment
    ##
    ## Searches for a segment with a given name
    ##
    ## # TODO: Support whitespace character
    ##
    ## @param name character string indicating the segment to search for
    ## @return boolean \code{TRUE} if the segment exists or \code{FALSE} if not
    ##
    find_segment = function(name)
    {
      name %in% names(private$segments)
    },

    ## error_segment
    ##
    ## Throws an error that a segment already exists.
    ##
    ## @param character string. The name of a segment
    ##
    error_segment = function(name)
    {
      stop(paste(if (!is.na(self$name)) sprintf("%s:", self$name), "Segment",
                 name, "already exists"), call. = FALSE)
    },

    ## create_segment
    ##
    ## Creates a signle segment
    ##
    ## @param from numeric. The beginning of the segment (in samples)
    ## @param to numeric. The end of the segment (in samples)
    ## @param name character string indicating the name of the segment
    ##
    create_segment = function(from, to, name)
    {
      # If 'from' has more than on element, pick the first one
      if (length(from) > 1) from <- from[1]
      # If 'to' has more than on element, pick the first one occuring after
      # 'from'
      if (length(to) > 1) to <- to[which(to > from)][1]

      private$segments[[name]] <- PHYR6_SEGMENT(from, to, name)
    },

    ## create_segments
    ##
    ## Creates multiple segments
    ##
    ## @param from numeric vector. Positions of "from" markers (in samples)
    ## @param to numeric vector. Positions of "to" markers (in samples)
    ## @param name character string indicating the base name of the segments.
    ##   Segments will be named by "name_1", "name_2", ...
    ##
    create_segments = function(from, to, name)
    {
      # Throw a warning if 'from' and 'to' do not have the same length
      if (length(from) != length(to))
      {
        warning("Arguments 'from' and 'to' do not have the same length")
      }

      # Counter variables
      i <- 1 # Walks through 'from' vector
      j <- 1 # Walks through 'to' vector
      n <- 1 # Number of segments created

      while (i <= length(from))
      {
        # Check if next "from" marker is before next "to" marker
        if (from[i] > to[j])
        {
          # Increase i and repeat
          i <- i + 1
          next
        }

        # Add segment
        private$segments[[paste(name, n, sep = "_")]] <-
          PHYR6_SEGMENT(from[i], to[j], paste(name, n, sep = "_"))

        # Increase counters
        i <- i + 1
        j <- j + 1
        n <- n + 1
      }

      message(paste(if(!is.na(self$name)) sprintf("%s:", self$name), n - 1,
                    "segments created"))
    },

    ## resample_data
    ##
    ## Returns a resampled copy of the 'data' field. This is e.g. used for
    ## downsampling the signal to increase plotting performance.
    ##
    ## @param freq numeric indicating the new sampling frequency in Hertz
    ##
    resample_data = function(freq = 5)
    {
      # Calculate number of data points to plot
      n <- round(length(self$data) / self$samplerate * freq)

      approx(seq_along(self$data) / self$samplerate, self$data, n = n)
    },

    ## filter_template
    ##
    ## Apply a butterworth filter to the 'data' field
    ##
    ## @param type character string indicating the type of the butterworth
    ##   filter. One of "low", "high", "pass" or "stop"
    ## @param freq numeric. The critical frequencies of the filter. Can be
    ##   either in Hertz if argument unit is "hertz" or relative to the nyquist
    ##   frequency if argument unit is "nyquist".
    ## @param unit character string indicating the unit for argument 'freq'.
    ##   Either "hertz" or "nyquist"
    ## @param order numeric indicating the filter order of the butterworth
    ##   filter
    ##
    filter_template = function(type, freq, unit = c("hertz", "nyquist"), order)
    {
      unit <- match.arg(unit)

      if (type == "low" || type == "high" && length(freq) != 1)
      {
        stop("Argument 'freq' must be of length one", call. = FALSE)
      }
      else if (type == "stop" || type == "pass" && length(freq != 2))
      {
        stop("Argument 'freq' must be of length two")
      }

      if (unit == "hertz")
      {
        # Divide the frequency in Hertz by the Nyquiest frequency
        freq <- freq / (self$samplerate / 2)
      }

      self$filter(butter(order, freq, type = type, plane = "z"))
    },

    ## plot_add_marker
    ##
    ## Display markers in a dygraphs plot
    ##
    ## @param plot dygraph to add marker to
    ##
    plot_add_marker = function(plot)
    {
      for (i in 1:nrow(self$marker))
      {
        plot %<>% dyEvent(self$marker[i, "position"] / self$samplerate,
                          label = self$marker[i, "name"], color = "#888888")
      }

      plot
    },

    ## plot_add_segments
    ##
    ## Display segments in a dygraphs plot
    ##
    ## @param plot dygraph to add segments to
    ##
    plot_add_segments = function(plot)
    {
      for (i in seq_along(private$segments))
      {
        plot %<>% dyShading(private$segments[[i]]$start / self$samplerate,
                            private$segments[[i]]$end / self$samplerate,
                            color = "#D8E2EE")
      }

      plot
    },

    samples_to_string = function(x)
    {
        (x / self$samplerate) %>%
        round(2) %>%
        lubridate::duration() %>%
        as.character()
    },

    samples_to_hms = function(x)
    {
      format(as.POSIXct('0001-01-01 00:00:00') + x / self$samplerate,
             "%H:%M:%S")
    }


  )

)
