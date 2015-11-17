#' Create a segment
#'
#' A segment defines a time interval in the signal
#'
#' @param start numeric indicating the beginning of the segment (in samples)
#' @param end numeric indicating the end of the segment (in samples)
#' @param name character string indicating the name of the segment
#'
#' @return An object of class "PHYR6_SEGMENT" with the following elements:
#'   start:
#'   end:
#'   name:
#'
PHYR6_SEGMENT <- function(start, end, name)
{
  structure(list(start = start, end = end, name = name),
            class = "PHYR6_SEGMENT")
}

# # TODO: replace segment completely by event?
# # TODO: or event with duration = segment?
#
# #' Create an event
# #'
# #' An event defines a point in the signal and has an optional duration
# #'
# #' @param start
# #' @param name
# #' @param duration
# #'
# #' @return An object of class "PHYR6_EVENT" with the following elements:
# #'   start:
# #'   name:
# #'   duration:
# #'
# PHYR6_EVENT <- function(start, name, duration = 0)
# {
#   structure(list(start = start, name = name, duration = duration),
#             class = "PHYR6_EVENT")
# }
