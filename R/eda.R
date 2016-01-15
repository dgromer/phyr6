#' EDA class
#'
#' @docType class
#' @importFrom lubridate duration
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#'
EDA <- R6Class("EDA",

  # Superclass
  inherit = PHYR6_BASE,

  public = list(

    # Initialize ---------------------------------------------------------------

    ## initialize
    ##
    ## @param data numeric vector containing the ECG signal
    ## @param samplerate numeric indicating the samplerate of the signal in
    ##   \code{data}.
    ## @param marker numeric vector containing the markers for the signal. Must
    ##   have the same length as \code{data}.
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

      # Ensure that self$marker is of class data.frame only so we can use
      # `[` instead of `[.data.frame` to extract a single value
      if (inherits(self$marker, "tbl_df")) class(self$marker) <- "data.frame"
    },

    ## scr
    ##
    ## Calculate the skin conductance response to a stimulus
    ##
    ## @param segment character string indicating the segment(s) to calculate
    ##   the SCR for.
    ## @param aggregate logical. If the measure is calculated for multiple
    ##   segments, should the results be aggregated to the mean or returned
    ##   as a vector.
    ## @param method character string indicating the method used to calculate
    ##   the SCR.
    ## @param latency numeric specifying the amout of time (in ms) to shift the
    ##   beginning of the segment. If the beginning of the segment is the
    ##   stimulus onset, then the foot of the SCR is typically 1-3 second later.
    ##
    scr = function(segment = NULL, aggregate = TRUE, method = "start-to-max",
                   latency = 0)
    {
      private$get_measure("scr", segment, aggregate, method = method,
                          latency = latency)
    },

    ## scl
    ##
    ## Calculate the skin conductance level
    ##
    ## @param segment character string indicating the segment(s) to calculate
    ##   the SCR for.
    ## @param aggregate logical. If the measure is calculated for multiple
    ##   segments, should the results be aggregated to the mean or returned
    ##   as a vector.
    ##
    scl = function(segment = NULL, aggregate = TRUE)
    {
      private$get_measure("scl", segment, aggregate)
    },

    # Print --------------------------------------------------------------------

    ## print
    ##
    ## @param marker logical. Print information about marker?
    ## @param segments logical. Print information about segments?
    ##
    print = function(marker = TRUE, segments = TRUE)
    {
      cat(
        # Class name
        "<EDA>",
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
    }

  ),

  private = list(

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

      # If no segment was specified, apply the function to the complete
      # sequence of interbeat intervals
      if (is.null(segment))
      {
        return(private[[name]](self$data))
      }

      # TODO: search if segment(s) exist
      #if (!private$find_segment(name)) private$error_segment_not_found(name)

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
          private[[name]](self$data[.x$start:.x$end], ...)
        }

      })

      # Return either aggregated measure or measures as named vector
      if (aggregate) mean(measure) else measure
    },

    ## scr_
    ##
    scr_ = function(x, method, latency)
    {
      if (latency > 0)
      {
        x <- x[(latency / 1000 * self$samplerate):length(x)]
      }
      else if (latency < 0)
      {
        stop("Negative values for argument 'latency' are not allowed")
      }

      if (method == "start-to-max")
      {
        # Difference between the first element in 'x' and the maximum of 'x'
        max(x) - x[1]
      }
      else if (method == "foot-to-max")
      {
        # Difference between the minimum in the first two seconds in 'x' and
        # the maximum of 'x'
        max(x) - x[min(x[seq_len(2 * self$samplerate)])]
      }
    },

    ## scl_
    ##
    scl_ = function(x)
    {
      mean(x)
    }

  )
)
