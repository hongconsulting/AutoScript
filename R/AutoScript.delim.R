#' Frequency table of delimited string elements (case- and
#' whitespace-insensitive)
#'
#' Splits each string in a string vector by a specified delimiter, trims
#' whitespace, converts to lowercase, and tabulates the frequency of all unique
#' elements.
#' @param x String vector containing delimited strings.
#' @param delimiter String delimiter. Default = `","`.
#' @return A table of element frequencies (case- and whitespace-insensitive).
#' @examples
#' treatment <- c("capecitabine",
#'                "LETROZOLE",
#'                "letrozole, palbociclib",
#'                "Letrozole,Ribociclib",
#'                "anastrozole, ribociclib")
#' print(AS.delim.table(treatment))
#' @export
AS.delim.table <- function(x, delimiter = ",") {
  return(table(tolower(trimws(unlist(strsplit(x, delimiter, fixed = TRUE))))))
}

#' Replace delimited string elements (case- and whitespace-insensitive)
#'
#' Splits each string in a string vector by a specified delimiter, trims
#' whitespace, converts to lowercase, replaces matching elements with a new
#' string element, removes duplicate elements, sorts elements alphabetically,
#' and rejoins the elements using the same delimiter.
#' @param x String vector containing delimited strings.
#' @param match String element to match (case- and whitespace-insensitive).
#' @param replacement String element replacement (case- and
#' whitespace-insensitive).
#' @param delimiter String delimiter. Default = `","`.
#' @return A string vector with replaced elements.
#' @examples
#' treatment <- c("capecitabine",
#'                "LETROZOLE",
#'                "letrozole, palbociclib",
#'                "Letrozole,Ribociclib",
#'                "anastrozole, ribociclib")
#' treatment <- AS.delim.replace(treatment, "anastrozole", "ai")
#' treatment <- AS.delim.replace(treatment, "letrozole", "ai")
#' treatment <- AS.delim.replace(treatment, "palbociclib", "cdk46i")
#' treatment <- AS.delim.replace(treatment, "ribociclib", "cdk46i")
#' print(treatment)
#' @export
AS.delim.replace <- function(x, match, replacement, delimiter = ",") {
  f <- function(t) {
    parts <- tolower(trimws(unlist(strsplit(t, delimiter, fixed = TRUE))))
    parts[parts == tolower(trimws(match))] <- tolower(trimws(replacement))
    parts <- sort(unique(parts))
    return(paste(parts, collapse = paste0(delimiter, " ")))
  }
  return(unname(sapply(x, f)))
}
