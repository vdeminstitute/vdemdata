#' Shows basic information on a specific variable as given in the codebook
#'
#' Provides information on full name as specified in the codebook, variable type,
#' data collection process (questions, clarifications, responses),
#' scale, aggregation, years and sources (plus notes)
#' of a variable in the V-Dem data set
#'
#' @param var_tag A variable's name as used in the V-Dem data set,
#' e.g. "v2x_polyarchy" for the electoral democracy index.
#' Note: use quotation marks vor "var_tag".
#'
#'
#' @return A list of information on the desired variable.
#'
#' @import dplyr
#' @export
#'
#' @examples
#' # Get information on the variable "v2x_polyarchy" (the electoral democracy index)
#' var_info("v2x_polyarchy")
var_info <- function(var_tag){
  tag <- NULL
  vdemdata::codebook %>%
  dplyr::filter(tag %in% var_tag) %>%
    as.list()
}

#' Search variable information by one or several keywords
#'
#' \code{find_var} returns a data.frame with the Codebook
#' information on the indicators. The keywords are searched
#' in the names of variables.
#'
#' @param keywords A character vector of length one with one or
#' several keywords separated by comma to search in
#' the name of the variable. You can also pass regex
#' notation (see \code{examples(find_var)}).
#'
#' @import dplyr
#'
#' @examples
#' # Don't run
#' # df <- find_var("Democracy, ^elect")
#' # View(df)
#'
#' @export
find_var <- function(keywords) {
 . <- name <- tag <- NULL
	pattern <- strsplit(keywords, ",") %>%
		unlist() %>%
		tolower() %>%
		gsub("^ ", "", x = .) %>%
		paste0(collapse = "|")

	df <- vdemdata::codebook %>%
		dplyr::filter(grepl(pattern, tolower(name))) %>%
		dplyr::arrange(tag)

	return(df)
}
