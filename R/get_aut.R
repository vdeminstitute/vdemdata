#' Get episodes of autocratization (democratic regression, autocratic regression)
#'
#' Helps to identify episodes of autocratization in the most recent vdem data set.
#' \emph{Autocratization} is defined as any movement towards autocracy which starts within democracies or autocracies (cf. Lührmann and Lindberg, Democratization, 2019).
#' \emph{Democratic regression} is defined as a subtype of autocratization and specifically focuses on any movement towards autocracy
#' which starts in democracies. \emph{Autocratic regression} is also a subtype of autocratization and
#' concerns all those which are already autocratic and further decline (cf. Boese et al., forthcoming in Democratization, 2020).
#'
#' @param data The data based on which the episodes are identified.
#' By default the most recent vdem data set with filled election variables (fill_vdem)
#'
#' @param start_incl A threshold for detecting the onset of "potential" autocratization episodes.
#' By default a negative change in the EDI (Vdem's Electoral Democracy Index) of at least -0.01 from year(t-1) to year(t).
#'
#' @param cum_incl A threshold to identify a "manifest" episode of autocratization as a cumulative negative change of the EDI (Vdem's Electoral Democracy Index)
#' between the start and end of a sequence. By default a cumulative change of -0.1 on the EDI.
#'
#' @param year_turn A threshold to identify a sudden "turn" during a year of an ongoing episode of autocratization (=failed autocratization).
#' By default a yearly change of +0.02 on the EDI (Vdem's Electoral Democracy Index).
#'
#' @param cum_turn A threshold to identify a gradual "turn" during an ongoing episode of autocratization (=failed autocratization).
#' By default a cumulative change of +0.1 on the EDI (Vdem's Electoral Democcracy Index) between the start and end of a sequence.
#'
#' @param tolerance A threshold to specify the number of "stasis" observations (\emph{i.e.}, observations neither increasing
#' or decreasing significantly) permitted before stopping a sequence. By default 5 years.
#'
#' @param output The default output is a data frame. Alternatively, output = "summary" prints the overall observed numbers of each episode types to the console,
#' output = "table" compiles a short table showing each country with start and end year of each episode and episode ID.
#'
#' @return A data frame specifying episodes of autocratization in the most recent Vdem data set: democratic regression for those episodes starting in democracy ("aut_ep_dem") and
#' autocratic regression for those episodes starting in autocracy ("aut_ep_aut"), further distinguishing subtypes of democratic regression into (1) breakdown ("breakdown"), and (2) averted democratic regression ("averted").
#'
#' @import dplyr
#' @import magrittr
#' @import Rcpp
#' @import hablar
#' @export
#'
#' @examples
#' Get the episodes with standard parameters:
#' aut_episodes <- get_aut()
#'
#' Get a short table with country, start and end year of episodes, episode ID only
#' aut_table <- get_aut(output="table")
#'
#' Get the overall numbers of episode types printed to the console
#' aut_summary <- get_aut(output="summary")
#'
#'
get_aut <- function(data = vdemdata::vdem,
                    start_incl = -0.01,
                    cum_incl = -0.1,
                    year_turn = 0.02,
                    cum_turn = 0.1,
                    tolerance = 5,
                    output = "df")
  {
  ### data cleaning and preparation ###
  # selecting the variables we need to construct the episodes dataframe

  full.df <- data %>%
    dplyr::select(country_name, country_id, country_text_id, year, v2x_polyarchy, codingend, matches("v2x_polyarchy"), gapstart1, gapstart2, gapstart3,
                  v2x_regime, matches("v2eltype")) %>%
    dplyr::filter(year >= 1900) %>%
    dplyr::mutate(gapstart1 = ifelse(is.na(gapstart1), 9, gapstart1),
                  gapstart2 = ifelse(is.na(gapstart2), 9, gapstart2),
                  gapstart3 = ifelse(is.na(gapstart3), 9, gapstart3)) %>%
    dplyr::arrange(country_text_id, year) %>%
    dplyr::group_by(country_text_id) %>%
    
    # detect and save potential episodes with the help of the c++ function find_seqs
    dplyr::mutate(episode_id = find_seqs_aut(v2x_polyarchy, v2x_regime,
                                             start_incl,
                                             year_turn,
                                             cum_turn,
                                             tolerance),
                  character_id = ifelse(!is.na(episode_id), paste(country_text_id, episode_id, sep = "_"), NA),
                  aut_ep = ifelse(!is.na(episode_id), 1, 0))%>% # general check: is there a potential autocratization episode?
    ungroup() %>%
    group_by(character_id) %>%
    dplyr::mutate(aut_ep_start_year = ifelse(!is.na(episode_id),first(year) +1, NA),
                  aut_ep_end_year = ifelse(!is.na(episode_id), last(year), NA),
                  aut_ep = ifelse(aut_ep==1 & min(hablar::s(v2x_polyarchy)) - max(hablar::s(v2x_polyarchy)) < cum_incl, 1, 0),# we check whether the cumulated change in each potential episode was substantial (> cum_inc)
                  aut_ep = ifelse(is.na(aut_ep), 0, aut_ep),
                  pre_regime = first(v2x_regime), # we create this column to get information on regime status in the pre-episode year (NOTE: this is the first year of the episodes generated by the c++ function!)
                  pre_ep_year = ifelse(!is.na(character_id), ifelse(year == first(year), 1, 0), 0),
                  aut_ep_aut = ifelse(aut_ep == 1 & pre_regime <=1, 1, 0),
                  aut_ep_aut = ifelse(is.na(aut_ep_aut), 0, aut_ep_aut), # identify manifest autocratization episodes starting in autocracies
                  aut_ep_dem = ifelse(aut_ep ==1 & pre_regime >1, 1, 0),
                  aut_ep_dem = ifelse(is.na(aut_ep_dem), 0, aut_ep_dem), # identify manifest autocratization episodes starting in democracies
                  aut_ep_id = ifelse(!is.na(episode_id), paste(country_text_id, aut_ep_start_year, aut_ep_end_year, sep = "_"), NA),
                  last_ch_year = max(ifelse(v2x_polyarchy-dplyr::lag(v2x_polyarchy, n=1)<=start_incl, year, NA), na.rm=T), # let's locate the last year of a negative change before encountering either a drop or stasis
                  censored = ifelse((codingend - aut_ep_end_year ==0 | (gapstart1 - aut_ep_end_year ==0) | # here, we need to make sure that  is not also coded as censored
                                       (gapstart2 - aut_ep_end_year == 0) | (gapstart3 - aut_ep_end_year == 0)), 1, 0)) %>%
    ungroup() %>%
    dplyr::select(-character_id, -episode_id, -pre_regime) %>%
    dplyr::arrange(country_name, year) %>%
    # dplyr::filter(aut_ep == 1) %>%
    as.data.frame %>%
    
    
    ### episodes of democratic regression ###
    
    # assess the type of democratic regression episode: breakdown or averted
    
    # for breakdown
    group_by(country_id) %>%
    dplyr::mutate(dem_reg = ifelse(v2x_regime > 1,1,0), # this column tells us whether a regime is democratic or not (country/year)
                  reg_trans = (dem_reg - (dplyr::lag(dem_reg, n=1L)))) %>% # this column marks any year in which a regime transitioned to democracy (1) or experienced democratic breakdown (-1)
    ungroup() %>%
    group_by(aut_ep_id) %>%
    # first condition for breakdown: a founding "autocratic" election
    dplyr::mutate(founding_elec = min(hablar::s(ifelse(v2x_regime < 2 & pre_ep_year !=1 & aut_ep_dem ==1 & (v2eltype_0 == 1 | v2eltype_4 ==1 | v2eltype_6 ==1) & lag(pre_ep_year !=1, n=1) & lag(v2x_regime < 2, n=1), year, NA))), # let's find the year of the first election after transition (=founding election)
                  last_trans = min(hablar::s(founding_elec-(ifelse(reg_trans == -1 & pre_ep_year != 1 & year < founding_elec, founding_elec - year, NA)))),  # let's find the last transition before the founding election
                  # second condition: it remains electoral autocracy for tolerance period, even if the episode ends ("remain"):
                  max_trans = max(hablar::s(ifelse(reg_trans==-1 & aut_ep_dem == 1 & pre_ep_year != 1, year, NA)))) %>%
    ungroup() %>%
    group_by(country_id) %>%
    dplyr::mutate(remain = ifelse(aut_ep_dem ==1 & censored==0 & is.na(founding_elec) & !is.na(max_trans) & year==max_trans & lead(v2x_regime==1, n=tolerance), 1, 0)) %>%
    ungroup() %>%
    group_by(aut_ep_id) %>%
    dplyr::mutate(remain = max(remain, na.rm=T),
                  min_reg= min(v2x_regime),  # now we find the minimum regime type within the episode of democratic regression
                  # third condition: it becomes closed autocracy
                  breakdown = ifelse(aut_ep_dem ==1 & !is.na(founding_elec), 1, 0),
                  breakdown = ifelse(aut_ep_dem == 1 & remain == 1, 1, breakdown),
                  breakdown = ifelse(aut_ep_dem == 1 & min_reg==0,1, breakdown),
                  
                  
                  
                  
                  # let's clear out stasis years:
                  
                  # for averted: all what is not breakdown AND not censored (see below)
                  averted = ifelse(aut_ep_dem == 1 & censored == 0 & breakdown == 0, 1, 0),
                  aut_ep_end_year = ifelse(censored == 0, last_ch_year, aut_ep_end_year)) %>%
    ungroup() %>%
    dplyr::mutate(aut_ep_id = paste(country_text_id, aut_ep_start_year, aut_ep_end_year, sep = "_"),
                  aut_ep = ifelse(year>aut_ep_end_year, 0, aut_ep)) %>%
    dplyr::filter(aut_ep ==1)  %>%
    
    
    
    # find year of breakdown: if remain == 1, then max_trans (last year of transition), if closed autocracy, take the first year of this OR for electoral autocracy take last_trans
    group_by(aut_ep_id) %>%         
    dplyr::mutate(remain_year = ifelse(remain == 1, max_trans, NA), # for the first condition above
                  closed_year = min(hablar::s(ifelse(v2x_regime==0, year, NA))), # for the second condition above
                  #last_trans_year = ifelse(!is.na(founding_elec), last_trans, NA), # for the third...
                  breakdown_end = ifelse(breakdown ==1 & !is.na(closed_year), closed_year, ifelse(!is.na(last_trans), last_trans, remain_year)),
                  # now we recode the aut_ep subtype based on breakdown year
                  aut_ep_dem = ifelse(breakdown==1 & year>breakdown_end, 0, aut_ep_dem),
                  aut_ep_aut = ifelse(breakdown==1 & year>breakdown_end, 1, aut_ep_aut),
                  
                  
                  # now, we need to define the sub episodes identifiers
                  sub_aut_ep_id = aut_ep_id,
                  sub_aut_ep_id = ifelse(aut_ep_aut==1 & breakdown ==1, paste(country_text_id, breakdown_end+1, aut_ep_end_year, sep = "_"), sub_aut_ep_id),
                  sub_aut_ep_id = ifelse(aut_ep_dem == 1 & breakdown ==1, paste(country_text_id, aut_ep_start_year, breakdown_end, sep = "_"), sub_aut_ep_id)) %>%
    # now we find whether sub-episodes are manifest
    group_by(sub_aut_ep_id) %>%
    dplyr::mutate(manifest = ifelse(aut_ep==1 & min(v2x_polyarchy, na.rm = T) - max(v2x_polyarchy, na.rm = T) < cum_incl, 1, 0))%>%
    # now we find whether sub episodes are censored
    ungroup() %>%
    group_by(aut_ep_id) %>%
    dplyr::mutate(sub_censored = censored,
                  sub_censored = ifelse(breakdown==1, 0, sub_censored))%>%
    
    # clean out all columns we don't need
    dplyr::select(country_name, country_id, country_text_id, year, v2x_regime, v2x_polyarchy, aut_ep, aut_ep_id, sub_aut_ep_id, aut_ep_start_year, aut_ep_end_year, manifest, censored, sub_censored,
                  pre_ep_year, aut_ep_aut, aut_ep_dem, averted, breakdown, breakdown_end) %>%
    
    
    # clean up variables for episodes that are not manifest
    
    
    # get a new column showing the type of each episode
    
    dplyr::mutate(aut_ep_type = ifelse(aut_ep_aut==1,"auto_regression", "no_ep"),
                  aut_ep_type = ifelse(breakdown==1,"breakdown", aut_ep_type),
                  aut_ep_type = ifelse(averted == 1, "averted", aut_ep_type),
                  aut_ep_type = ifelse(censored == 1, "censored", aut_ep_type))
  
  # define output options
  if(output == "summary"){
    summary <- full.df %>%
      dplyr::filter(aut_ep_type !="no_ep") %>%
      group_by(aut_ep_type) %>%
      mutate(count_ep = n_distinct(aut_ep_id),
             count_countries = n_distinct(country_id)) %>%
      distinct(aut_ep_type, count_ep, count_countries) %>%
      arrange(aut_ep_type)
    print(summary)}
  
  if(output == "table"){
    table <- full.df %>%
      dplyr::filter(aut_ep_type !="no_ep") %>%
      dplyr::select(country_name, aut_ep_type, aut_ep_start_year, aut_ep_end_year, aut_ep_id)%>%
      dplyr::distinct(country_name, aut_ep_start_year, aut_ep_end_year, aut_ep_type, aut_ep_id) %>%
      arrange(country_name)
    return(table)}
  {
    return(full.df)
  }
}

  ### done ;-) ###
