#' @param basetable A list created by \code{AS.basetable.create}, containing the group
#'assignments and current table structure.
#' @param subset.mask Optional logical vector used to restrict
#' the analysis to a subset of observations.
#' @param p.values Logical value indicating whether to display \emph{p}-values in the added row.
#' @param digits.sig Number of significant figures for \emph{p}-values. Default = 2.
#' @param sig.thresh Threshold below which \emph{p}-values are displayed as
#' \code{"< threshold"}. Default = 0.001.
