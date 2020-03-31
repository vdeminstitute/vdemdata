#' Find the overlap between episodes of democratization and autocratization.
#'
#' @param dem_ep The outcome of get_dem(), democratization episodes. By default with standard parameters.
#'
#' @param aut_ep The outcome of get_aut(), autocratization episodes. By default with standard parameters.
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
#' #democratization_episodes <- get_dem()
#' #autocratization_episodes <- get_aut()
#'
#' #overlap <- find_overlap(democratization_episodes, autocratization_episodes)
#'
find_overlap <-function(
  dem_ep = vdemdata::get_dem(),
  aut_ep = vdemdata::get_aut())
{
  merged = dem_ep %>%
    full_join(aut_ep)
  aut <- merged %>% filter(aut_ep == 1) %>% dplyr::select(country_name, year)
  dem <- merged  %>% filter(dem_ep == 1) %>% dplyr::select(country_name, year)
  overlap <- rbind(aut,dem)[duplicated(rbind(aut,dem)),]
  return(overlap)
}
