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
    },

    scr = function(segment = NULL, aggregate = TRUE, method = "start-to-max")
    {
      private$get_measure("scr", segment, aggregate, method = method)
    },

    scl = function(segment = NULL, aggregate = TRUE)
    {
      private$get_measure("scl", segment, aggregate, method = method)
    },

    # Print --------------------------------------------------------------------

    ## print
    ##
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
        "<EDA>",
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
        m <- unique(self$marker$name)
        # Sequence of names of markers in 'marker' data frame
        cat(paste("\n  Markers:", paste(m[order(m)], collapse = ", ")))
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
      #if (!find_segment)
      #{
      #  stop("Segment not found")
      #}

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

    scr_ = function(x, method)
    {
      if (method == "start-to-max")
      {
        max(x) - x[1]
      }
    },

    scl_ = function(x)
    {
      mean(x)
    }

  )
)
