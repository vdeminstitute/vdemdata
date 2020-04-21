#' Find the overlap between episodes of democratization and autocratization.
#'
#' @param episodes The outcome of get_eps(), episodes of regime transformation (ERT), to be used for finding potential overlaps (depending on individual parameter setting). By default with standard parameters.
#'
#' @return A data frame showing the country/year of overlaps between democratization and autocratization episodes.
#'
#' @import dplyr
#' @export
#'
#' @examples
#' #Don't run
#' #Find the overlap between democratization and autocratization episodes
#'
#' #overlap <- find_overlap(episodes)
#'
find_overlap <-function(
  episodes = vdemdata::get_eps())
{
  merged <- episodes
  aut <- merged %>% filter(aut_ep == 1) %>% dplyr::select(country_name, year)
  dem <- merged  %>% filter(dem_ep == 1) %>% dplyr::select(country_name, year)
  overlap <- rbind(aut,dem)[duplicated(rbind(aut,dem)),]
  return(overlap)
}
