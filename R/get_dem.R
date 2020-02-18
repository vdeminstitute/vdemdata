#' Get episodes of democratization (autocratic adaption, democratic deepening)
#'
#' Helps to identify episodes of democratization (liberalization, democratic deepening) in the most recent vdem data set.
#' \emph{Democratization} is an umbrella term for any movement towards demcracy - be it in autocracies or democracies.
#' \emph{Autocratic adaptation} is defined as a subtype of democratiztion and specifically focuses on any movement towards democracy
#' which starts in autocracies. \emph{Democratic deepening} is also a subtype of democratization and
#' concerns all those which are already democratic and further improve their democratic traits (cf. Wilson et al., 2020).
#'
#' @param data The data based on which the episodes are identified.
#' By default the most recent vdem data set.
#'
#' @param start_incl A threshold for detecting the onset of "potential" democratization episodes.
#' By default a positive change in the EDI (Vdem's Electoral Democracy Index) of at least +0.01 from year(t-1) to year(t).
#'
#' @param cum_incl A threshold to identify a "manifest" episode of democratization as a cumulative positive change of the EDI (Vdem's Electoral Democracy Index)
#' between the start and end of a sequence. By default a cumulative change of +0.1 on the EDI.
#'
#' @param year_turn A threshold to identify a sudden "turn" during a year of an ongoing episode of democratization (=failed democratization).
#' By default a yearly change of -0.02 on the EDI (Vdem's Electoral Democracy Index).
#'
#' @param cum_turn A threshold to identify a gradual "turn" during an ongoing episode of democratization (=failed democratization).
#' By default a cumulative change of -0.1 on the EDI (Vdem's Electoral Democcracy Index) between the start and end of a sequence.
#'
#' @param tolerance A threshold to specify the number of "stasis" observations (\emph{i.e.}, observations neither increasing
#' or decreasing significantly) permitted before stopping a sequence. By default 10 years.
#'
#' @param output The default output is a data frame. Alternatively, output = "summary" prints the overall observed numbers of each episode types to the console,
#' output = "table" compiles a short table showing each country with start and end year of each episode and episode ID.
#'
#' @return A data frame specifying episodes of democratization in the most recent Vdem data set: democratic deepening for those episodes starting in democracy ("dem_ep_dem") and
#' autocratic adaptation for those episodes starting in autocracy ("dem_ep_aut"), further distinguishing successful opening episodes ("success"), and three types of failure,
#' (1) preempted ("fail_preem"), (2) reverted ("fail_rev"), and (3) stabilized autocracy ("fail_stab").
#'
#' @import dplyr
#' @import magrittr
#' @import Rcpp
#' @import hablar
#' @export
#'
#' @examples
#' Get the episodes with standard parameters:
#' dem_episodes <- get_dem()
#'
#' Get a short table with country, start and end year of episodes, episode ID only
#' dem_table <- get_dem(output="table")
#'
#' Get the overall numbers of episode types printed to the console
#' dem_summary <- get_dem(output="summary")
#'
### set the parameters ###
get_dem <- function(data = vdemdata::vdem,
                             start_incl = 0.01,
                             cum_incl = 0.1,
                             year_turn = -0.02,    # NOTE: year_turn is implemented in the c++ script but still needs to be setted here, otherwise it cannot be changed by user of package´
                             cum_turn = -0.1,
                             tolerance = 10,
                             output = "df")
                             {
  ### data cleaning and preparation ###
  # selecting the variables we need to construct the episodes dataframe

  full.df <- data %>%
    dplyr::select(country_name, country_id, country_text_id, year,
                  v2x_polyarchy, codingend, matches("v2x_polyarchy"),
                  gapstart1, gapstart2, gapstart3, gapend1, gapend2, gapend3,
                  v2x_regime, matches("v2eltype"), v2elasmoff_ord) %>%
    dplyr::filter(year >= 1900) %>%
    dplyr::mutate(gapstart1 = ifelse(is.na(gapstart1), 9, gapstart1),
                  gapstart2 = ifelse(is.na(gapstart2), 9, gapstart2),
                  gapstart3 = ifelse(is.na(gapstart3), 9, gapstart3)) %>%
    dplyr::arrange(country_text_id, year) %>%
    dplyr::group_by(country_text_id) %>%

    # detect and save potential episodes with the help of the c++ function find_seqs
    dplyr::mutate(episode_id = find_seqs_dem(v2x_polyarchy, v2x_regime,
                                             start_incl,
                                             year_turn,
                                             cum_turn,
                                             tolerance),
                  character_id = ifelse(!is.na(episode_id), paste(country_text_id, episode_id, sep = "_"), NA),
                  dem_ep = ifelse(!is.na(episode_id), 1, 0)) %>% # general check: is there a potential democratization episode?
    ungroup() %>%
    group_by(character_id) %>%
    dplyr::mutate(dem_ep_start_year = ifelse(!is.na(episode_id),first(year) +1, NA),
                  dem_ep_end_year = ifelse(!is.na(episode_id), last(year), NA),
                  manifest = ifelse(dem_ep==1 & max(v2x_polyarchy, na.rm = T) - min(v2x_polyarchy, na.rm = T) > cum_incl, 1, 0), # we check whether the cumulated change in each potential episode was substantial (> cum_inc)
                  pre_regime = first(v2x_regime), # we create this column to get information on regime status in the pre-episode year (NOTE: this is the first year of the episodes generated by the c++ function!)
                  pre_ep_year = ifelse(!is.na(character_id), ifelse(year == first(year), 1, 0), 0),
                  dem_ep_aut = ifelse(!is.na(character_id) & manifest == 1, ifelse(pre_regime <=1, 1, 0),0), # identify manifest autocratic adaptation episodes starting in autocracies
                  dem_ep_dem = ifelse(!is.na(character_id) & manifest ==1, ifelse(pre_regime >1, 1, 0),0), # identify manifest democratic deepening episodes starting in democracies
                  dem_ep_id = ifelse(!is.na(episode_id), paste(country_text_id, dem_ep_start_year, dem_ep_end_year, sep = "_"), NA)) %>%
    ungroup() %>%
    dplyr::select(-character_id, -episode_id, -pre_regime)%>%
    dplyr::arrange(country_name, year)%>%
    as.data.frame %>%


    ### autocratic adaptation episodes ###

    # assess the type of episode: success, failure
    group_by(country_id) %>%
    dplyr::mutate(dem_reg = ifelse(v2x_regime > 1,1,0), # this column tells us whether a regime is democratic or not (country/year)
                  reg_trans = (dem_reg - (dplyr::lag(dem_reg, n=1)))) %>% # this column marks any year in which a regime transitioned to democracy (1) or experienced democratic breakdown (-1)
    ungroup() %>%
    group_by(dem_ep_id) %>%
    dplyr::mutate(founding_elec = min(hablar::s(ifelse(v2x_regime > 1 & dem_ep_aut ==1 & v2elasmoff_ord > 1 & (v2eltype_0 == 1 | v2eltype_4 ==1 | v2eltype_6 ==1) & dplyr::lag(v2x_regime > 1, n=1), year, NA))), # let's find the year of the first election after transition (=founding election)
                  last_trans = min(hablar::s(founding_elec-(ifelse(reg_trans == 1 & pre_ep_year != 1 & year < founding_elec, founding_elec - year, NA)))), # let's find the last transition before the founding election
                  success = ifelse(!is.na(founding_elec), 1, 0)) %>% # finally, define successful episodes based on the conditions above with stasis years still included
    ungroup() %>%
    dplyr::mutate(dem_ep_end_year = ifelse(success == 1, last_trans, dem_ep_end_year),  # let's remove stasis years within successful episodes (for all columns below...)
                  dem_ep_id = ifelse(success ==1, paste(country_text_id, dem_ep_start_year, dem_ep_end_year, sep = "_"), dem_ep_id),
                  dem_ep = ifelse(year>dem_ep_end_year & success ==1, 0, dem_ep),
                  dem_ep_aut = ifelse(year>dem_ep_end_year & success ==1, 0, dem_ep_aut),
                  manifest = ifelse(year>dem_ep_end_year & success ==1, 0, manifest),
                  founding_elec = ifelse(year>dem_ep_end_year & success ==1, NA, founding_elec),
                  success = ifelse(!is.na(dem_ep_id), ifelse(year>dem_ep_end_year, 0, success), 0),
                  dem_ep_id = ifelse(year>dem_ep_end_year, NA, dem_ep_id))  %>%

    # extract the pre-empted episodes
    # meaning there was a transition to democracy but no founding election
    group_by(dem_ep_id) %>%
    dplyr:: mutate(max_trans = max(hablar::s(ifelse(reg_trans == 1,year,NA))), # here, we need the last transition within one episode, not followed by a founding election
                   censored_preem = ifelse(dem_ep_aut == 1 & success == 0 & !is.na(max_trans) & (codingend - max_trans < tolerance | (gapstart1 - max_trans > 0 & gapstart1 - max_trans < tolerance) | # here, we need to make sure that preempted is not also coded as censored
                                                                                                   (gapstart2 - max_trans > 0 & gapstart2 - max_trans < tolerance) | (gapstart3 - max_trans > 0 & gapstart3 - max_trans < tolerance)), 1, 0),
                   fail_preem = ifelse(dem_ep_aut == 1 & success == 0 & !is.na(max_trans) & censored_preem !=1, 1, 0),
                   dem_ep_end_year = ifelse(fail_preem == 1, max_trans, dem_ep_end_year)) %>% # let's remove stasis years within preempted episodes (for all columns below...)
    ungroup() %>%
    mutate(dem_ep_id = ifelse(fail_preem ==1, paste(country_text_id, dem_ep_start_year, dem_ep_end_year, sep = "_"), dem_ep_id),
           dem_ep = ifelse(year>dem_ep_end_year & fail_preem ==1, 0, dem_ep),
           dem_ep_aut = ifelse(year>dem_ep_end_year & fail_preem ==1, 0, dem_ep_aut),
           manifest = ifelse(year>dem_ep_end_year & fail_preem ==1, 0, manifest),
           fail_preem = ifelse(!is.na(dem_ep_id), ifelse(year>dem_ep_end_year, 0, fail_preem), 0),
           dem_ep_id = ifelse(year>dem_ep_end_year, NA, dem_ep_id)) %>%

    # extract the failed reverted
    # for this, the following conditions need to be meet: first, a country moves back to closed autocracy
    group_by(country_id) %>%
    dplyr::mutate(back_closed = ifelse(dplyr::lead(v2x_regime, n=1) == 0 & v2x_regime > 0 & year == dem_ep_end_year, 1, 0),

                  # OR second, check if there was a cumulative drop "drop" in v2x_polyarchy after the final epiosde year for the length of tolerance which is bigger than cum_turn
                  # for this, we need to create "cum_drop" with the following:
                  last_ch_year = ifelse(v2x_polyarchy-dplyr::lag(v2x_polyarchy, n=1)>=start_incl , year, NA)) %>%  # let's locate the last year of a positive change before encountering either a drop or stasis
    group_by(dem_ep_id) %>%
    dplyr::mutate(last_ch_year = ifelse(manifest == 1, max(last_ch_year, na.rm = T), NA)) %>%
    group_by(country_id)

  # yearly drop
  year_drop <- list()
  for (i in 1:tolerance) {
    year_drop[[i]] <- ifelse(full.df$year == full.df$last_ch_year & dplyr::lead(full.df$country_id, n=i)==full.df$country_id, dplyr::lead(full.df$v2x_polyarchy, n=i-1)-dplyr::lead(full.df$v2x_polyarchy, n=i), NA)
  }

  df1 <- do.call(cbind, lapply(year_drop, data.frame, stringsAsFactors=FALSE))
  names <- paste0('year', seq(1:tolerance))
  colnames(df1) <- names
  year_drop <- df1 %>%
    dplyr::mutate(year_drop = ifelse(apply(df1, 1, FUN = min) <= year_turn, 1,NA))  %>%
    dplyr::select(year_drop)

  # cum drop
  cum_drop <- list()
  for (i in 1:tolerance) {
    cum_drop[[i]] <- ifelse(full.df$year == full.df$last_ch_year & dplyr::lead(full.df$country_id, n=i)==full.df$country_id, dplyr::lead(full.df$v2x_polyarchy, n=i)-full.df$v2x_polyarchy, NA)
  }

  df <- do.call(cbind, lapply(cum_drop, data.frame, stringsAsFactors=FALSE))
  names <- paste0('cum', seq(1:tolerance))
  colnames(df) <- names
  cum_drop <- df %>%
    dplyr::mutate(cum_drop = ifelse(apply(df, 1, FUN = min) <= cum_turn, 1,NA)) %>%
    dplyr::select(cum_drop)

  # merge this new column "cum_drop" to our full.df
  full.df <- full.df %>%
    tibble::rownames_to_column('newid') %>%
    left_join(tibble::rownames_to_column(year_drop, 'newid'), by = 'newid') %>%
    left_join(tibble::rownames_to_column(cum_drop, 'newid'), by = 'newid') %>%

    # finally, we have all conditions to check for failed liberalization (the two created above combined by logical OR):
    group_by(dem_ep_id) %>%
    # dplyr::mutate(fail_rev = ifelse(success == 0 & fail_preem == 0 & dem_ep_aut == 1 & (max(hablar::s(back_closed == 1)) | max(hablar::s(cum_drop == 1)) | max(hablar::s(year_drop == 1)) | max(hablar::s(v2x_regime)) == 0),1,0))
    dplyr::mutate(fail_rev = max(hablar::s(ifelse(success == 0 & fail_preem == 0 & dem_ep_aut == 1 & (back_closed == 1 | cum_drop == 1 | year_drop == 1),1,0))),
                  fail_rev = ifelse(!is.na(fail_rev) & fail_rev==1, 1, 0),  # line 147 fills in fail_rev values for entire episode, this line then removes NAs and replaces with zeros
                  censored_fail = ifelse(dem_ep_aut == 1 & success == 0 & fail_preem == 0 & fail_rev == 0 & (codingend - last_ch_year < tolerance | (gapstart1 - last_ch_year > 0 & gapstart1 - last_ch_year < tolerance) | # here, we need to make sure that no episode is not also coded as censored
                                                                                                               (gapstart2 - last_ch_year > 0 & gapstart2 - last_ch_year < tolerance) | (gapstart3 - last_ch_year > 0 & gapstart3 - last_ch_year < tolerance)), 1, 0),
                  fail_rev = ifelse(success == 0 & fail_preem == 0 & dem_ep_aut == 1 & max(v2x_regime == 0) & censored_fail != 1, 1, fail_rev),

                  # extract the stabilized autocracy episodes
                  # if episode (dem_ep_aut), but not success, not preempted, not failed_lib, then it is stabilized autocracy (fail_stab)
                  fail_stab = ifelse(dem_ep_aut == 1 & success == 0 & fail_preem == 0 & fail_rev == 0 & censored_fail != 1, 1,0))%>%

    # now we remove any remaining stasis years
    dplyr::ungroup() %>%
    dplyr::mutate(dem_ep_end_year = ifelse((fail_rev == 1 | fail_stab ==1 | dem_ep_dem == 1), last_ch_year, dem_ep_end_year),
                  dem_ep_id = ifelse((fail_rev == 1 | fail_stab ==1| dem_ep_dem == 1), paste(country_text_id, dem_ep_start_year, dem_ep_end_year, sep = "_"), dem_ep_id),
                  dem_ep = ifelse(year>dem_ep_end_year & (fail_rev == 1 | fail_stab ==1| dem_ep_dem == 1), 0, dem_ep),
                  dem_ep_aut = ifelse(year>dem_ep_end_year & (fail_rev == 1 | fail_stab ==1), 0, dem_ep_aut),
                  manifest = ifelse(year>dem_ep_end_year & (fail_rev == 1 | fail_stab ==1| dem_ep_dem == 1), 0, manifest),
                  fail_rev = ifelse(!is.na(dem_ep_id) & year>dem_ep_end_year, 0, fail_rev),
                  fail_stab = ifelse(!is.na(dem_ep_id) & year>dem_ep_end_year, 0, fail_stab),
                  dem_ep_dem = ifelse(!is.na(dem_ep_id) & year>dem_ep_end_year, 0, dem_ep_dem),
                  dem_ep_id = ifelse(year>dem_ep_end_year, NA, dem_ep_id),

                  # we check whether the episode is still ongoing (=censored), for autocratic adaptation episodes
                  censored = ifelse(dem_ep_aut==1 & (censored_preem == 1 | censored_fail == 1), 1,0),

                  # now, we still need to check if democratic deepening episodes are censored
                  censored = ifelse(dem_ep_dem== 1 & (codingend - last_ch_year < tolerance | (gapstart1 - last_ch_year > 0 & gapstart1 - last_ch_year < tolerance) | # here, we need to make sure that no episode is not also coded as censored
                                                        (gapstart2 - last_ch_year > 0 & gapstart2 - last_ch_year < tolerance) | (gapstart3 - last_ch_year > 0 & gapstart3 - last_ch_year < tolerance)), 1, censored)) %>%
    # clean out all columns we don't need
    dplyr::select(country_name, country_id, country_text_id, year, dem_ep = manifest, dem_ep_id, dem_ep_start_year, dem_ep_end_year, censored,
                  pre_ep_year, dem_ep_aut, dem_ep_dem, success, founding_elec, fail_preem, fail_rev, fail_stab) %>%


    # clean up variables for episodes that are not manifest
    dplyr::mutate(dem_ep_end_year = ifelse(dem_ep==0, NA, dem_ep_end_year),
                  dem_ep_start_year = ifelse(dem_ep==0, NA, dem_ep_start_year),
                  dem_ep_id = ifelse(dem_ep==0, NA, dem_ep_id),
                  pre_ep_year = ifelse(dem_ep==0, NA, pre_ep_year),
                  censored = ifelse(dem_ep==0, NA, censored),


                  # get a new column showing the type of each episode
                  dem_ep_type = ifelse(dem_ep_dem==1,"democr_deep", "no_ep"),
                  dem_ep_type = ifelse(success==1,"success", dem_ep_type),
                  dem_ep_type = ifelse(fail_preem == 1, "fail_preem", dem_ep_type),
                  dem_ep_type = ifelse(fail_rev == 1, "fail_rev", dem_ep_type),
                  dem_ep_type = ifelse(fail_stab ==1, "fail_stab", dem_ep_type),
                  dem_ep_type = ifelse(censored == 1, "censored", dem_ep_type))

  # define output options
  if(output == "summary"){
    summary <- full.df %>%
      dplyr::filter(dem_ep_type !="no_ep") %>%
      group_by(dem_ep_type) %>%
      mutate(count_ep = n_distinct(dem_ep_id),
             count_countries = n_distinct(country_id)) %>%
      distinct(dem_ep_type, count_ep, count_countries) %>%
      arrange(dem_ep_type)
    print(summary)}

  if(output == "table"){
    table <- full.df %>%
        dplyr::filter(dem_ep_type !="no_ep") %>%
        dplyr::select(country_name, dem_ep_type, dem_ep_start_year, dem_ep_end_year, dem_ep_id)%>%
        dplyr::distinct(country_name, dem_ep_start_year, dem_ep_end_year, dem_ep_type, dem_ep_id) %>%
        arrange(country_name)
    return(table)}
  {
  return(full.df)
  }
}
  ### done ;-) ###
