#' @export
#' @importFrom ROI as.OP
as.OP.ROI_optimizer <- function(x) {
  stopifnot(inherits(x, "ROI_optimizer"))
  x@ptr$as_ROI_OP()
}
