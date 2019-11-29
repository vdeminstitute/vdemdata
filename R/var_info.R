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
  vdemdata::codebook %>%
  dplyr::filter(vdemdata::codebook$tag == var_tag) %>%
    as.list()
}


