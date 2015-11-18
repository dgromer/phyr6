#' phyr6 base class
#'
#' @docType class
#' @importFrom dygraphs dygraph
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#'
PHYR6_BASE <- R6Class("PHYR6_BASE",

  public = list(

    # Public fields ------------------------------------------------------------

    #' data
    #'
    #' Numeric vector for the physiological signal
    #'
    data = NA,

    #' samplerate
    #'
    #' Numeric of length one indicating the samplerate of the signal in 'data'
    #'
    samplerate = NA,

    #' marker
    #' Numeric vector with the same length as 'data' indicating the marker
    #' positions
    #'
    marker = NA,

    #' name
    #'
    #' Character string indicating the name of the signal (e.g. subject id)
    #'
    name = NA,

    #' path
    #'
    #' Character string indicating the path where export and import functions
    #' act.
    #'
    path = NA,

    # Segmentation -------------------------------------------------------------

    #' segment_marker
    #'
    #' Create a segment for given markers
    #'
    #' @param from numeric indicating the start marker for segment(s)
    #' @param to numeric indicating the end marker for segment(s)
    #' @param name character string indicating the name for the segment
    #' @param check logical indicating whether to stop if a marker was not found
    #'   (\code{TRUE}) or create the segment with content \code{NA}
    #'   (\code{FALSE})
    #'
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

      # Get indizes of markers
      from <- if (!is.null(from)) which(self$marker == from) else 0
      to <- if(!is.null(to)) which(self$marker == to) else length(self$data)

      # TODO: check if markers appear one or more times

      if (length(from) > 1 && length(to) > 1)
      {
        private$create_segments(from, to, name)
      }
      else
      {
        private$create_segment(from, to, name)
      }
    },

    #' segment_samples
    #'
    #' Create a segment based on given samples
    #'
    #' @param from numeric indicating the start position for the segment in
    #'   samples
    #' @param to numeric indicating the end position for the segment in samples
    #' @param name character string indicating the name for the segment
    segment_samples = function(from = 0, to = length(self$data), name)
    {
      private$create_segment(from, to, name)
    },

    #' plot_data
    #'
    plot_data = function(freq = 5)
    {
      # Downsample data for improved plotting performance
      data <- resample_data(freq)

      # Create dygraphs object
      plot <- dygraph(list(x = seq_along(data) / freq, y = data))

      plot
    }

  ),

  private = list(

    #' segments
    #'
    #' List containing the specified segments (of class PHYR6_SEGMENTS)
    #'
    segments = list(),

    #' find_marker
    #'
    #' Searches for a given marker in the marker vector
    #'
    #' @param marker numeric marker to search
    #' @return boolean \code{TRUE} if the marker was found or \code{FALSE} if
    #'   not
    #'
    find_marker = function(marker)
    {
      !is.null(marker) && any(self$marker == marker)
    },

    #' error_marker
    #'
    #' Throws an error that a marker could not be found
    #'
    #' @param marker numeric marker to report an error for
    #'
    error_marker = function(marker)
    {
      stop(paste("Could not find marker", marker), call. = FALSE)
    },

    #' find_segment
    #'
    #' Searches for a segment with a given name
    #'
    #' # TODO: Support whitespace character
    #'
    #' @param name character string indicating the segment to search for
    #' @return boolean \code{TRUE} if the segment exists or \code{FALSE} if not
    #'
    find_segment = function(name)
    {
      name %in% names(self$segments)
    },

    #' error_segment
    #'
    #' Throws an error that a segment already exists.
    #'
    #' @param character string. The name of a segment
    #'
    error_segment = function(name)
    {
      stop(paste("Segment", name, "already exists"))
    },

    #' create_segment
    #'
    #' Creates a signle segment
    #'
    #' @param from numeric. The beginning of the segment (in samples)
    #' @param to numeric. The end of the segment (in samples)
    #' @param name character string indicating the name of the segment
    #'
    create_segment = function(from, to, name)
    {
      self$segments[[name]] <- PHYR6_SEGMENT(from, to, name)
    },

    #' create_segments
    #'
    #' Creates multiple segments
    #'
    #' @param from numeric vector
    #' @param to numeric vector
    #' @param name character string indicating the base name of the segments.
    #'   Segments will be named by "name_1", "name_2", ...
    #'
    create_segments = function(from, to, name)
    {
      # Throw a warning if 'from' and 'to' do not have the same length
      if (length(from) != length(to))
      {
        warning("Arguments 'from' and 'to' do not have the same length")
      }

      # Counter variables
      k <- length(from)
      i <- 0
      j <- 0

      while (k < length(from))
      {
        # Check if next "from" marker is before next "to" marker
        if (from[i] < to[j])
        {
          # Increase i and repeat
          i <- i + 1
          next
        }

        # Add segment
        self$segments[[paste(name, k, sep = "_")]] <-
          PHYR6_SEGMENT(from[i], to[j], paste(name, k, sep = "_"))

        # Increase counters
        i <- i + 1
        j <- j + 1
        k <- k + 1
      }

      message(paste(k - 1), "segments created")
    },

    #' resample_data
    #'
    #' Returns a resampled copy of the 'data' field. This is e.g. used for
    #' downsampling the signal to increase plotting performance.
    #'
    #' @param freq numeric indicating the new sampling frequency in Hertz
    #'
    resample_data = function(freq = 5)
    {
      # Calculate number of data points to plot
      n <- round(length(self$data) / self$samplerate * freq)

      approx(seq_along(self$data) / self$samplerate, self$data, n = n)
    }

  )
)
