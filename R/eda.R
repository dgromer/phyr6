#' EDA class
#'
#' @docType class
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
    }

  ),

  private = list(

  )
)
