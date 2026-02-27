#' @param basetable A list created by `AS.basetable.create`, containing the group
#' assignments and current table structure.
#' @param subset.mask Optional logical vector used to restrict the analysis to a
#' subset of observations. Alternatively, the string `"auto"` restricts the
#' analysis to the non-missing observations.
#' @param p.values Logical value indicating whether to display *p*-values in the
#' added row.
#' @param digits.fixed Number of decimal places for summaries. Default = `2`.
#' @param digits.sig Number of significant figures for *p*-values. Default = `2`.
#' @param sig.thresh Threshold below which *p*-values are displayed as
#' `"< threshold"` and values larger than `1 - threshold` are displayed as
#' `"> 1 - threshold"`. Default = `0.001`.
