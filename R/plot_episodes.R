#' Plot Episodes of Regime Transformation (ERT) over time.
#'
#' `plot_episodes` plots Episodes of Regime Transformation (ERT) over time. It shows either
#' the global number/share of countries undergoing democratization and autocratization or
#' episodes for an individual country.
#'
#' This function is a wrapper for [ggplot2:ggplot()] and produces a plot that shows
#' democratization and autocratization episodes over time.
#' Either the global number/share of countries undergoing democratization and autocratization or
#' episodes for an individual country are plotted. The function calls the [vdemdata:get_eps()] funtion that is
#' used to identify episodes.
#'
#' @param abs Logical value: if TRUE, the absolute number of countries in an episode for each year is plotted. If FALSE, the share
#'  of countries (in \%) undergoing democratization or autocratization is plotted.
#'
#' @param years Vector with numeric values indicating the minimum and maximum year to be plotted.
#'
#' @param country Character vector containing the country for which episodes should be shown. Only countries from the
#'  country_name column in the V-Dem data set are accepted. If no countries are specified, global episode numbers
#'  are plotted. Only a single country can be selected.
#'
#' @param start_incl A threshold for detecting the onset of "potential" episodes.
#' By default a change in the EDI (Vdem's Electoral Democracy Index) of at least +/-0.01 from year(t-1) to year(t).
#'
#' @param cum_incl A threshold to identify a "manifest" episodes as a cumulative change of the EDI (Vdem's Electoral Democracy Index)
#' between the start and end of a sequence. By default a cumulative change of +/-0.1 on the EDI.
#'
#' @param year_turn A threshold to identify a sudden "turn" during a year of an ongoing episode (=failed democratization/autocratization).
#' By default a yearly change of +/-0.03 on the EDI (Vdem's Electoral Democracy Index). Note: Advanced users who wish to remove this criteria altogether
#' should set the value of year turn equal to cum turn. Setting this to zero would allow for an episode to terminate when any year of no change is encountered.
#'
#' @param cum_turn A threshold to identify a gradual "turn" during an ongoing episode (=failed democratization/autocratization).
#' By default a cumulative change of -0.1 on the EDI (Vdem's Electoral Democcracy Index) between the start and end of a sequence.
#'
#' @param tolerance A threshold to specify the number of "stasis" observations (\emph{i.e.}, observations neither increasing
#' or decreasing significantly) permitted before stopping a sequence. By default 5 years.
#'
#' @return The output of this function is a [ggplot2:ggplot()] object with global or country-specific episodes.

#' @import ggplot2 tidyr dplyr stringr
#'
#' @examples
#' \dontrun{
#' # Plot autocratization and democratization episodes over time using
#'  default values from [vdemdata:get_eps()].
#'
#'  plot_episodes()
#'
#' # Plot episodes for Belgium between 1910 and 2010.
#'
#'  plot_episodes(country = c("Belgium"),
#'                years = c(1910, 2010))
#' }
#' @export
plot_episodes <- function(abs = T,
                          years = c(1900, 2020),
                          country = c(),
                          start_incl  = 0.01,
                          cum_incl  = 0.1,
                          year_turn = 0.03,
                          cum_turn = 0.1,
                          tolerance = 5) {

  eps <- vdemdata::get_eps(data = vdemdata::vdem,
                      start_incl = start_incl,
                      cum_incl = cum_incl,
                      year_turn = year_turn,
                      cum_turn = cum_turn,
                      tolerance = tolerance)

  if(length(country) > 1)
    stop("Error: More than one country selected")

year <- country_name <- dem_ep <- aut_ep <- overlap_eps <- country_text_id <- v2x_polyarchy <-
  ep_type <- episode <- vdem <- aut_ep_start_year <- aut_ep_end_year <-
  dem_ep_start_year <- dem_ep_end_year <-
  aut_pre_ep_year <- dem_pre_ep_year <- episode_id  <- NULL

  if(length(country) > 0) {
    eps_year <- eps %>%
      filter(country_name == country, between(year, min(years), max(years))) %>%
      filter(dem_ep == 1 | aut_ep == 1) %>%
      mutate(overlap_eps = ifelse(!is.na(aut_ep_id) & !is.na(dem_ep_id), "overlap", NA)) %>%
      pivot_longer(cols = c(aut_ep_id, dem_ep_id, overlap_eps), names_to = "ep_type", values_to = "episode") %>%
      select(country_name, country_text_id, year, v2x_polyarchy, ep_type, episode,
             aut_ep_start_year, aut_ep_end_year, dem_ep_start_year, dem_ep_end_year,
             aut_pre_ep_year, dem_pre_ep_year) %>%
      filter((ep_type == "dem_ep_id" & dem_pre_ep_year == 0) |
             (ep_type == "aut_ep_id" & aut_pre_ep_year == 0) |
              ep_type == "overlap" & aut_pre_ep_year == 0 & dem_pre_ep_year == 0) %>%
      drop_na(episode) %>%
      group_by(year) %>%
      mutate(overlap_eps = n(),
             episode_id = ifelse(ep_type == "aut_ep_id", str_sub(episode, 5, 13), episode),
             episode_id = ifelse(ep_type == "dem_ep_id", str_sub(episode, 5, 13), episode_id),
             episode_id = ifelse(ep_type == "aut_ep_id", paste0("AUT:", episode_id), episode_id),
             episode_id = ifelse(ep_type == "dem_ep_id", paste0("DEM:", episode_id), episode_id)) %>%
      ungroup()

    polyarchy <- eps %>%
      filter(country_name == country, between(year, min(years), max(years))) %>%
      ungroup() %>%
      select(year, v2x_polyarchy)

    if(max(eps_year$overlap_eps) > 1) {
      print("Warning: Some episodes overlap!")
    }

 p <-   ggplot2::ggplot() +
          geom_line(data = eps_year, aes(group = episode_id, color = episode_id, linetype = ep_type,x = year, y = v2x_polyarchy)) +
          geom_line(data = polyarchy, aes(x = year, y = v2x_polyarchy), alpha = 0.3) +
          scale_colour_grey(breaks = levels(factor(eps_year$episode_id[eps_year$episode_id!="overlap"])),
                        name = "Episode", start = 0.01, end = 0.01) +
          scale_linetype_manual(name = "Episode type", breaks = c("aut_ep_id", "dem_ep_id", "overlap"),
                     labels = c("Autocratization", "Democratization", "Overlap"),
                     values = c("dashed", "dotted", "solid")) +
          scale_x_continuous(breaks = seq(round(min(years) / 10) * 10, round(max(years) / 10) * 10, 10)) +
          xlab("Year") +  ylab("Electoral Democracy Index") + ylim(0,1) +
          theme_classic() +
          guides(color = guide_legend(override.aes = list(size = 0))) +
          ggtitle(sprintf("%s", country))

 if (isTRUE(length(which(eps_year$ep_type == "dem_ep_id")) > 0)) {
    p <- p +  geom_point(data = eps_year, aes(x = year, y = ifelse(year == dem_ep_start_year, v2x_polyarchy, NA)), shape = 2, alpha = 0.75) +
        geom_point(data = eps_year, aes(x = year, y = ifelse(year == dem_ep_end_year, v2x_polyarchy, NA)), shape = 17, alpha = 0.75)
 } else {
    p
 }

 if (isTRUE(length(which(eps_year$ep_type == "aut_ep_id")) > 0)) {
    p <- p +  geom_point(data = eps_year, aes(x = year, y = ifelse(year == aut_ep_start_year, v2x_polyarchy, NA)), shape = 1, alpha = 0.75) +
       geom_point(data = eps_year, aes(x = year, y = ifelse(year == aut_ep_end_year, v2x_polyarchy, NA)), shape = 16, alpha = 0.75)
 } else {
    p
 }
    p


  } else{

  if (isTRUE(abs)) {
    eps_year <- eps %>%
      dplyr::filter(between(year, min(years), max(years))) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(dem_eps = sum(dem_ep),
                       aut_eps = sum(aut_ep)) %>%
    pivot_longer(cols = c(dem_eps, aut_eps), names_to = "ep_type", values_to = "countries")

  } else {
    eps_year <- eps %>%
      dplyr::filter(between(year, min(years), max(years))) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(dem_eps = sum(dem_ep) / length(unique(country_id)),
                       aut_eps = sum(aut_ep) / length(unique(country_id))) %>%
      pivot_longer(cols = c(dem_eps, aut_eps), names_to = "ep_type", values_to = "countries")
  }

 p <-  ggplot2::ggplot(data = eps_year, aes(x = year, y = countries, group = ep_type, linetype = ep_type)) +
        geom_line() +
        scale_x_continuous(breaks = seq(round(min(years) / 10) * 10, round(max(years) / 10) * 10, 10)) +
        scale_linetype(name = "", breaks = c("aut_eps", "dem_eps"), labels = c("Autocratization", "Democratization")) +
        xlab("Year") +
        theme_classic() +
        theme(legend.position = "bottom")

 if (isTRUE(abs)) {
   p +  ylab("Number of Countries")
   }  else {
   p +  ylab("Countries (%)")
   }
  }
}
