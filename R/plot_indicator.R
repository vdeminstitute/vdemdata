#' Plot indicator(s) in V-Dem data.
#'
#' `plot_indicator` produces a combined lineplot and dotplot for selected
#' countries and indicators from the V-Dem data set.
#'
#' This function is a wrapper for [ggplot2:ggplot()] and produces a combined
#' lineplot and dotplot for selected indicators
#' from the V-Dem dataset. It allow users to select multiple indicators
#' as well as multiple countries. Users can adjust the time frame to be plotted.
#' The function offers some sensible defaults
#' for plotting. Additional plot options from [ggplot2:ggplot()] can be added
#' outside the function call. Note that this function is mainly for exploratory analyses
#' and might not be useful for all types of indicators in the data.
#'
#' @param indicator Character vector containing the indicators from the V-Dem data that are plotted (see codebook).
#' @param countries Character vector containing the countries that are plotted. Only countries from the
#'  country_name column in the V-Dem data are accepted. If no countries are specified the global averages
#'  are plotted.
#' @param min_year Numeric value representing the lower bound of the time frame (years) to be plotted.
#' @param max_year Numeric value representing the upper bound of the time frame (years) to be plotted.
#' @param uncertainty Logical value: adds uncertainty intervals to point estimates if available.
#'
#' @return The output of this function is a [ggplot2:ggplot()] object with lines and points
#'  for global averages of the selected indicators or point estimates for selected countries.
#'
#' @import dplyr ggplot2 tidyr tidyselect
#'
#' @examples
#' \dontrun{
#' # Plot V-Dem indicators liberal democracy and egalitarian democracy
#' # for Sweden and Germany between 1912 and 2000.
#'
#'  plot_indicator(indicator=c( "v2x_egaldem", "v2x_libdem"), countries = c("Germany", "Sweden"),
#'                      min_year = 1912, max_year = 2000)
#'
#' # Plot the global averages of V-Dem indicators electoral and egalitarian democracy
#' # between 1940 and 2010.
#'
#'  plot_indicator(indicator=c("v2x_polyarchy", "v2x_egaldem"),
#'                      min_year = 1940, max_year = 2010)
#' }
#' @export
plot_indicator <- function(indicator,
                           countries = NULL,
                           min_year = min(vdemdata::vdem$year),
                           max_year = max(vdemdata::vdem$year),
                           uncertainty = T) {

year <- country_name <- value <- mean_indicator <- vdem <- name <- type <- codelow <- codehigh <- NULL

# Specify sensible colors for plotted lines
  colour_palette <- c("#000000", "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
                      "#66A61E", "#E6AB02", "#A6761D", "#666666")

  # If no countries are selected, compute and plot the global average for the selected indicators
  if (is.null(countries) == T) {
    data <- vdemdata::vdem %>%
      dplyr::select(indicator = indicator, year, country_name) %>%
      dplyr::filter(year >= min_year & year <= max_year) %>%
      tidyr::pivot_longer(starts_with("indicator"), names_to = "indicator", values_to = "value") %>%
      dplyr::group_by(year, indicator) %>%
      dplyr::summarize(mean_indicator = mean(value, na.rm = T)) %>%
      tidyr::drop_na()

    ggplot2::ggplot(data,
                    aes(x = year, y = mean_indicator, fill = indicator, shape = indicator)) +
        geom_line() + geom_point(size = 1) + xlab("") +
        ylab(ifelse(length(indicator) == 1, indicator, "")) +
        scale_x_continuous(breaks = seq(round(min_year / 10) * 10, round(max_year / 10) * 10, 10)) +
        scale_shape_manual("Indicator", values = rep(0:length(indicator)), labels = sort(indicator)) +
        scale_fill_manual("Indicator", values = rep("black", length(indicator)), labels = sort(indicator)) +
        theme_bw()
  }
  # If uncertainty is turned off or indicators do not have uncertainty estimates plot those variables
  else if (isTRUE(uncertainty) == F | length(intersect(colnames(vdemdata::vdem), paste(indicator, "_codehigh", sep = ""))) < 1) {
    data <- vdemdata::vdem %>%
      dplyr::select(indicator = all_of(indicator), year, country_name) %>%
      dplyr::filter(year >= min_year & year <= max_year) %>%
      dplyr::filter(country_name %in% countries) %>%
      dplyr::arrange(country_name) %>%
      tidyr::pivot_longer(starts_with("indicator"), names_to = "indicator", values_to = "value") %>%
      tidyr::drop_na()

    ggplot2::ggplot(data,
                    aes(x = year, y = value, shape = indicator, color = country_name)) +
        geom_line(alpha = 0.80) + geom_point(size = 0.75) + xlab("") +
        ylab("") +
        scale_x_continuous(breaks = seq(round(min_year / 10) * 10, round(max_year / 10) * 10, 10)) +
        scale_color_manual(values = colour_palette[seq_len(length(countries))], name = "Country",
                           labels = sort(countries)) +
        scale_shape_manual(values = rep(0:length(indicator)), labels = sort(indicator), name = "Indicator") +
        theme_bw()
  }
  # Plot selected variables for selected countries including uncertainty estimates
  else {
    indicator_high <- paste(indicator, "_codehigh", sep = "")
    indicator_low <- paste(indicator, "_codelow", sep = "")

    data  <-  vdemdata::vdem %>%
      dplyr::select(one_of(indicator), one_of(indicator_high), one_of(indicator_low), year, country_name) %>%
      dplyr::filter(year >= min_year & year <= max_year) %>%
      dplyr::filter(country_name %in% countries) %>%
      dplyr::arrange(country_name) %>%
      tidyr::pivot_longer(-c(country_name, year)) %>%
      dplyr::mutate(indicator = stringr::str_remove(name, "_codelow|_codehigh"),
                    type = ifelse(stringr::str_detect(name, "_codelow"), "codelow", "value"),
                    type = ifelse(stringr::str_detect(name, "_codehigh"), "codehigh", type)) %>%
      tidyr::pivot_wider(names_from = (type),
                         values_from = value,
                         id_cols = c(country_name, year, indicator)) %>%
      tidyr::drop_na()

    ggplot2::ggplot(data,
                    aes(x = year, y = value, shape = indicator, color = country_name)) +
        geom_line(alpha = .80) + geom_point(size = 0.75) +
        geom_ribbon(aes(x = year, ymin = codelow,
                        ymax = codehigh,
                        linetype = NA, fill = country_name), alpha = .25) + xlab("") +
        xlab("") + ylab("") +
        scale_x_continuous(breaks = seq(round(min_year / 10) * 10, round(max_year / 10) * 10, 10)) +
        scale_color_manual(values = colour_palette[seq_len(length(countries))], name = "Country",
                           labels = sort(countries)) +
        scale_fill_manual(values = colour_palette[seq_len(length(countries))], name = "Country",
                          labels = sort(countries)) +
        scale_shape_manual(values = rep(0:length(indicator)), labels = sort(indicator), name = "Indicator") +
        theme_bw()
  }
}
