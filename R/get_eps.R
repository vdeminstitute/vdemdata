#' Get episodes of regime transformation (ERT)
#'
#' Helps to identify episodes of democratization (liberalization, democratic deepening) and autocratization (demcratic regression, autocratic regression) in the most recent vdem data set.
#'
#' \emph{Democratization} is an umbrella term for any movement towards demcracy - be it in autocracies or democracies.
#' \emph{liberalization} is defined as a subtype of democratiztion and specifically focuses on any movement towards democracy
#' which starts in autocracies. \emph{Democratic deepening} is also a subtype of democratization and
#' concerns all those which are already democratic and further improve their democratic traits (cf. Wilson et al., 2020).
#'
#' \emph{Autocratization} is defined as any movement towards autocracy which starts within democracies or autocracies (cf. Lührmann and Lindberg, Democratization, 2019).
#' \emph{Democratic regression} is defined as a subtype of autocratization and specifically focuses on any movement towards autocracy
#' which starts in democracies. \emph{Autocratic regression} is also a subtype of autocratization and
#' concerns all those which are already autocratic and further decline (cf. Boese et al., forthcoming in Democratization, 2020).
#'
#' @param data The data based on which the episodes are identified.
#' By default the most recent vdem data set.
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
#' @param output The default output is a data frame. For details see the ERT codebook.
#'
#' @return A data frame specifying episodes of regime transformation in the most recent Vdem data set.
#'
#' Democratization episodes: democratic deepening for those episodes starting in democracy ("dem_ep_dem") and
#' liberalization for those episodes starting in autocracy ("dem_ep_aut"), further distinguishing successful episodes of democratic transitions ("success"), and three types of failure,
#' (1) preempted ("fail_preem"), (2) reverted ("fail_rev"), and (3) stabilized autocracy ("fail_stab").
#'
#' Autocratization episodes: democratic regression for those episodes starting in democracy ("aut_ep_dem") and
#' autocratic regression for those episodes starting in autocracy ("aut_ep_aut"), further distinguishing subtypes of democratic regression into (1) breakdown ("breakdown"), and (2) averted democratic regression ("averted").
#'
#'
#' @import dplyr
#' @import Rcpp
#' @import hablar
#' @import tidyr
#' @import plm
#' @export
#'
#' @examples
#' #Don't run
#' #Get the episodes with standard parameters:
#' #episodes <- get_eps()
#'
### set the parameters ###
get_eps <- function(data = vdemdata::vdem,
                             start_incl = 0.01,
                             cum_incl = 0.1,
                             year_turn = 0.03,    # NOTE: year_turn is implemented in the c++ script but still needs to be setted here, otherwise it cannot be changed by user of package´
                             cum_turn = 0.1,
                             tolerance = 5,
                             output = "df")
                             {

  if(year_turn == 0)
    print("You set year_turn = 0. Did you mean to do this? Doing so means an episode ends when it experiences a year of no annual change on the EDI. Perhaps, instead, you meant to set its value equal to cum_turn. See p.3 of the ERT codebook.")

   ### DATA CLEANING AND PREP ###

  # selecting the variables we need to construct the episodes dataframe #


  full.df <- data %>%
    dplyr::select(country_name, country_id, country_text_id, year,
                  v2x_polyarchy, codingstart, codingend, matches("v2x_polyarchy"),
                  gapstart1, gapstart2, gapstart3, gapend1, gapend2, gapend3,
                  v2x_regime, matches("v2eltype"), v2elasmoff_ord) %>%
    dplyr::filter(year >= 1900) %>%
    dplyr::arrange(country_text_id, year) %>%
    dplyr::group_by(country_id) %>%
    # make codingstart 1900 or first year thereafter
    dplyr::mutate(codingstart2 = min(hablar::s(ifelse(!is.na(v2x_regime), year, NA))),
                  # tag original sample for later use
                  origsample = 1) %>%
    # we need to balance the dataset to deal with gaps in coding
    # this balances the dataset
    plm::make.pconsecutive(balanced=T, index=c("country_id", "year")) %>%
    dplyr::group_by(country_id) %>%
    # this fills missing variables we need that are constant within countries
    tidyr::fill(c(country_text_id, country_name, codingend, gapstart1, gapend1, gapstart2, gapend2,
                  gapstart3, gapend3)) %>%
    tidyr::fill(c(country_text_id, country_name,codingend, gapstart1, gapend1, gapstart2, gapend2,
                  gapstart3, gapend3), .direction="up")  %>%
    # here we need to recode the gaps as only during the period prior to and during the gap (for our censoring variables)
    dplyr::mutate(gapstart = ifelse(year<=gapend1, gapstart1, NA),
                  gapend = ifelse(year<=gapend1, gapend1, NA),
                  gapstart = ifelse(!is.na(gapend2) & year>gapend1 & year<=gapend2, gapstart2, gapstart),
                  gapend = ifelse(!is.na(gapend2) & year>gapend1 & year<=gapend2, gapend2, gapend),
                  gapstart = ifelse(!is.na(gapend3) & year>gapend2 & year<=gapend3, gapstart3, gapstart),
                  gapend = ifelse(!is.na(gapend3) & year>gapend2 & year<=gapend3, gapend3, gapend)) %>%

    #### CODING THE REGIME TYPE VARIABLES ###

    dplyr::arrange(country_id, year) %>%
    # here we code whether a regime change event on RoW occurred in the given country year, 1 = to democracy, -1 = to autocracy
    dplyr::mutate(row_regch_event = ifelse(v2x_regime>1 & dplyr::lag(v2x_regime<2, n=1), 1, 0),
                  row_regch_event = ifelse(v2x_regime<2 & dplyr::lag(v2x_regime>1, n=1), -1, row_regch_event),
                  # here we code the year of the most recent RoW regime change event
                  row_regch_year = ifelse(row_regch_event==-1 | row_regch_event==1, year, NA),
                  # here we code the filled regime change variable, telling us what was the type of the most recent RoW regime change
                  row_regch_filled = ifelse(!is.na(row_regch_year), row_regch_event, NA)) %>%
    # intially we fill everything
    tidyr::fill(c(row_regch_filled, row_regch_year)) %>%
    # here we replace with NA for gaps
    dplyr::mutate(row_regch_filled = ifelse(!is.na(row_regch_year) & ((!is.na(gapend1) & row_regch_year<gapstart1 & year>=gapstart1) |
                                                                        (!is.na(gapend2) & row_regch_year<gapstart2 & year>=gapstart2) |
                                                                        (!is.na(gapend3) & row_regch_year<gapstart3 & year>=gapstart3)),
                                            NA, row_regch_filled),
                  row_regch_year = ifelse(is.na(row_regch_filled), NA, row_regch_year)) %>%
    ungroup() %>%
    group_by(country_id, row_regch_year) %>%
    # here we check whether the RoW regime change is censored
    # censored near end of coding
    dplyr::mutate(row_regch_censored = ifelse(codingend-row_regch_year<tolerance, 1, 0),
                  # censored near gap
                  row_regch_censored = ifelse(!is.na(gapstart) & gapstart-row_regch_year<tolerance, 1, row_regch_censored),
                  # here we check to see if a regime change to democracy produced a founding election
                  dem_founding_elec = min(hablar::s(ifelse(v2x_regime>1 & year>=row_regch_year & v2elasmoff_ord > 1 &
                                                             # must hold leg, exec, or CA election
                                                             (v2eltype_0 == 1 | v2eltype_4 ==1 | v2eltype_6 ==1),
                                                           year, NA))),
                  row_demtrans_dum = ifelse(row_regch_event==1 & !is.na(dem_founding_elec), 1, NA),
                  row_demtrans_dum = ifelse(row_regch_event==1 & is.na(dem_founding_elec), 0, row_demtrans_dum),
                  row_regch_censored = ifelse(row_demtrans_dum==1, 0, row_regch_censored),
                  row_demtrans_dum = ifelse(row_regch_censored==1 & row_demtrans_dum==0, NA, row_demtrans_dum),

                  # here we check to see if a regime change to autocracy produced a democratic breakdown
                  # we start by looking for autocratic founding elections
                  aut_founding_elec = min(hablar::s(ifelse(v2x_regime==1 & year>=row_regch_year &
                                                             # must hold leg, exec, or CA election
                                                             (v2eltype_0 == 1 | v2eltype_4 ==1 | v2eltype_6 ==1),
                                                           year, NA))),
                  # we also check if it remained autocratic for the tolerance period
                  aut_stabilized = min(hablar::s(ifelse(v2x_regime==1 & year==row_regch_year &
                                                          dplyr::lead(v2x_regime==1, n=tolerance), 1, NA))),
                  # finally if it became closed
                  aut_closed = ifelse(row_regch_event==-1,1-min(hablar::s(v2x_regime)),NA),
                  # check to see if any of the above conditons hold
                  row_breakdown_dum = ifelse(row_regch_event==-1 & (!is.na(aut_founding_elec) |
                                                                      (!is.na(aut_stabilized) & aut_stabilized==1) |
                                                                      (!is.na(aut_closed) & aut_closed==1)), 1, NA),
                  row_breakdown_dum = ifelse(row_regch_event == -1 & is.na(row_breakdown_dum), 0, row_breakdown_dum),
                  row_regch_censored = ifelse(!is.na(row_breakdown_dum) & row_breakdown_dum==1, 0, row_regch_censored),
                  row_breakdown_dum = ifelse(!is.na(row_regch_censored) & row_regch_censored==1, NA, row_breakdown_dum)) %>%
    # here we code the regimes based on our criteria for democracy and autocracy
    ungroup() %>%
    group_by(country_id) %>%
    arrange(country_id, year) %>%
    # year the country transitioned to democracy on RoW provided it held a founding election
    dplyr::mutate(reg_start_year=ifelse(!is.na(dem_founding_elec) & row_regch_event==1, year, NA),
                  # year the country transitioned to autocracy on RoW provided closed, or electoral autocracy persisted or held election
                  reg_start_year=ifelse(!is.na(row_breakdown_dum) & row_breakdown_dum==1, year, reg_start_year),
                  # here we coding founding as first year observed
                  reg_start_year = ifelse(year==codingstart2, year, reg_start_year),
                  # here we code founding as first year observed after a gap
                  reg_start_year = ifelse(!is.na(gapend1) & year==gapend1+1, year, reg_start_year),
                  reg_start_year = ifelse(!is.na(gapend2) & year==gapend2+1, year, reg_start_year),
                  reg_start_year = ifelse(!is.na(gapend3) & year==gapend3+1, year, reg_start_year)) %>%
    tidyr::fill(reg_start_year) %>%
    dplyr::mutate(reg_start_year = ifelse(!is.na(reg_start_year) & ((!is.na(gapend1) & reg_start_year<gapstart1 & year>=gapstart1) |  # here we replace with NA for gaps
                                                                      (!is.na(gapend2) & reg_start_year<gapstart2 & year>=gapstart2) |
                                                                      (!is.na(gapend3) & reg_start_year<gapstart3 & year>=gapstart3)),
                                          NA, reg_start_year)) %>%
    ungroup() %>%
    group_by(country_id, reg_start_year) %>%
    # regime type is democracy (1) if v2x_regime is democratic in starting year
    dplyr::mutate(reg_type = ifelse(year==reg_start_year & v2x_regime>1, 1, NA),
                  # regime type is autocratic (0) if v2x_regime is autocratic in starting year
                  reg_type = ifelse(year==reg_start_year & v2x_regime<2, 0, reg_type),
                  # fill for entire regime period
                  reg_type = min(hablar::s(reg_type))) %>%
    ungroup() %>%
    group_by(country_id) %>%
    arrange(country_id, year) %>%
    # here we look for years where democratic becomes autocratic or vice versa
    dplyr::mutate(reg_trans = ifelse(!is.na(reg_type), reg_type-dplyr::lag(reg_type, n=1), NA),
                  # then we need to recode the starting years based on actual regime changes
                  reg_start_year = ifelse(!is.na(reg_trans) & reg_trans!=0, year, NA),
                  # here we coding founding as first year observed
                  reg_start_year = ifelse(year==codingstart2, year, reg_start_year),
                  # here we code founding as first year observed after a gap
                  reg_start_year = ifelse(!is.na(gapend1) & year==gapend1+1, year, reg_start_year),
                  reg_start_year = ifelse(!is.na(gapend2) & year==gapend2+1, year, reg_start_year),
                  reg_start_year = ifelse(!is.na(gapend3) & year==gapend3+1, year, reg_start_year)) %>%
    tidyr::fill(reg_start_year) %>%
    # here we replace with NA for gaps
    dplyr::mutate(reg_start_year = ifelse(!is.na(reg_start_year) & ((!is.na(gapend1) & reg_start_year<gapstart1 & year>=gapstart1) |
                                                                      (!is.na(gapend2) & reg_start_year<gapstart2 & year>=gapstart2) |
                                                                      (!is.na(gapend3) & reg_start_year<gapstart3 & year>=gapstart3)),
                                          NA, reg_start_year)) %>%
    ungroup() %>%
    group_by(country_id, reg_start_year) %>%
    # here we code the end of the regime
    dplyr::mutate(reg_end_year = dplyr::last(year),
                  # here we code the id for the regime
                  reg_id = ifelse(!is.na(reg_start_year), paste(country_text_id, reg_start_year, reg_end_year, sep = "_"), NA),
                  # here we recode the demtrans and breakdown dummies based on actual regime changes
                  row_demtrans_dum = ifelse(reg_trans==0 | is.na(reg_trans), 0, row_demtrans_dum),
                  row_breakdown_dum = ifelse(reg_trans==0 | is.na(reg_trans), 0, row_breakdown_dum),
                  # here we create a founding election variable for democratic regimes
                  founding_elec = min(hablar::s(dem_founding_elec))) %>%
    ungroup() %>%
    # make sure the data are sorted and grouped properly before sending to C++!!!!
    arrange(country_text_id, year) %>%
    group_by(country_text_id) %>%

    #### CODING THE DEMOCRATIZATION EPISODES ####

  ### detect and save potential episodes with the help of the c++ function find_seqs

  dplyr::mutate(episode_id = find_seqs_dem(v2x_polyarchy, v2x_regime,
                                           start_incl, year_turn=year_turn*-1, cum_turn=cum_turn*-1,
                                           tolerance),
                # set a temporary id for these potential episodes and group accordinly
                character_id = ifelse(!is.na(episode_id), paste(country_text_id, episode_id, sep = "_"), NA)) %>%
    ungroup() %>%
    group_by(character_id) %>%
    # general check: is there a potential democratization episode?
    dplyr::mutate(dem_ep = ifelse(!is.na(episode_id), 1, 0),
                  # we check whether the cumulated change in each potential episode was substantial (> cum_inc), i.e. the episode is manifest
                  dem_ep = ifelse(dem_ep==1 & max(v2x_polyarchy, na.rm = T) - min(v2x_polyarchy, na.rm = T) >= cum_incl, 1, 0)) %>%
    ungroup() %>%
    # then we clean out variables for non-manifest episodes
    dplyr::mutate(episode_id = ifelse(dem_ep!=1, NA, episode_id),
                  character_id = ifelse(dem_ep!=1, NA, character_id)) %>%
    group_by(character_id) %>%
    # generate the initial end year for the episode (note:  we have to filter out the stasis years that C++ gives us, but we will do this later):
    dplyr::mutate(dem_ep_end_year = ifelse(dem_ep==1, last(year), NA),
                  #  find potentially censored episodes (note: we might change this later depending on termination)
                  dem_ep_censored = ifelse(dem_ep==1 & codingend-dem_ep_end_year<tolerance, 1, 0),
                  dem_ep_censored = ifelse(dem_ep==1 & !is.na(gapstart) & (gapstart-1)-dem_ep_end_year<tolerance, 1, dem_ep_censored),
                  # generate the start year for the potential episode as the first year after the pre-episode year
                  dem_ep_start_year = ifelse(dem_ep==1,first(year)+1, NA),
                  # here we code a dummy for the pre-episode year
                  dem_pre_ep_year = ifelse(dem_ep==1, ifelse(year == dplyr::first(year), 1, 0), 0),
                  # we create a unique identifier for episodes and phases using the country_text_id, start, and end years
                  dem_ep_id = ifelse(dem_ep==1, paste(country_text_id, dem_ep_start_year, dem_ep_end_year, sep = "_"), NA)) %>%
    ungroup() %>%
    # remove the old identifiers we no longer need
    dplyr::select(-character_id, -episode_id) %>%
    # make sure the data is sorted properly
    dplyr::arrange(country_name, year) %>%
    # just to make sure we have a dataframe
    as.data.frame %>%


    ####### code termination type of democratization episode

    # democratization episodes end when one of three things happens:
    # 1. the case experiences an annual drop <= year_turn
    # 2. the case experiences a gradual drop <= cum_turn over the tolerance period (or less)
    # 3. the case experiences no annual increase = start_incl for the tolerance period (or more)

    # first find the last positive change on EDI equal to the start_incl parameter
    group_by(dem_ep_id) %>%
    dplyr::mutate(last_ch_year = max(hablar::s(ifelse(v2x_polyarchy-dplyr::lag(v2x_polyarchy, n=1)>=start_incl, year, NA))),
                  # here we just replace with NA non-episode years
                  last_ch_year = ifelse(dem_ep==0, NA, last_ch_year)) %>%
    # now lets make sure to group by the country (not the episode!) and arrange by country-year
    group_by(country_id) %>%
    arrange(country_id, year)

  # then check to see what happened the after the episode had its last substantive change equal to start_incl
  # we start with the yearly drop, aka year_turn
  year_drop <- list()
  # here we loop over the number of years (n) equal to the tolerance period after the last_change_year
  for (i in 1:tolerance) {
    # we calculate the first difference in the EDI for each of these yearly changes within the tolernce
    year_drop[[i]] <- ifelse(full.df$year == full.df$last_ch_year & dplyr::lead(full.df$country_id, n=i)==full.df$country_id, dplyr::lead(full.df$v2x_polyarchy, n=i)-dplyr::lead(full.df$v2x_polyarchy, n=i-1), NA)
  }
  # then we generate a dataframe from these calculations
  df1 <- do.call(cbind, lapply(year_drop, data.frame, stringsAsFactors=FALSE))
  # this just renames the columns to match the years ahead we are looking
  names <- paste0('year', seq(1:tolerance))
  colnames(df1) <- names
  # this transforms the result into a dataframe that we can use as a column in our existing dataframe
  # first write a small function to deal with Inf
  my.min <- function(x) ifelse(!all(is.na(x)), min(x, na.rm=T), NA)
  year_drop <- df1 %>%
    dplyr::mutate(year_drop = ifelse(apply(df1, 1, FUN = my.min) < year_turn*-1, 1,NA))  %>%
    dplyr::select(year_drop)
  # now we can also use the first-differences we calculated above to look for stasis as well
  # note - we will have to clean this up later to account for cum_turn as well
  stasis <- df1 %>%
    # this checks whether the maximum annual change is less than start_incl over the tolerance period &
    #    that it is also greater than the year_turn parameter, i.e. stasis
    dplyr::mutate(stasis = ifelse(apply(df1, 1, FUN = max) < start_incl & apply(df1, 1, FUN = min) >= year_turn*-1, 1,NA))  %>%
    dplyr::select(stasis)

  # now we look for a gradual drop equal to cum_drop over the tolerance period
  cum_drop <- list()
  # here we loop over time equal to the tolerance, looking for the difference between the last_change_year and that year on the EDI
  for (i in 1:tolerance) {
    cum_drop[[i]] <- ifelse(full.df$year == full.df$last_ch_year & dplyr::lead(full.df$country_id, n=i)==full.df$country_id, dplyr::lead(full.df$v2x_polyarchy, n=i)-full.df$v2x_polyarchy, NA)
  }
  # then we rename the columns and generate a dataframe we can use for our existing data
  df <- do.call(cbind, lapply(cum_drop, data.frame, stringsAsFactors=FALSE))
  names <- paste0('cum', seq(1:tolerance))
  colnames(df) <- names
  cum_drop <- df %>%
    dplyr::mutate(cum_drop = ifelse(apply(df, 1, FUN = my.min) <= cum_turn*-1, 1,NA)) %>%
    dplyr::select(cum_drop)

  # merge these new columns to our full.df
  full.df <- full.df %>%
    tibble::rownames_to_column('newid') %>%
    left_join(tibble::rownames_to_column(year_drop, 'newid'), by = 'newid') %>%
    left_join(tibble::rownames_to_column(cum_drop, 'newid'), by = 'newid') %>%
    left_join(tibble::rownames_to_column(stasis, 'newid'), by = 'newid') %>%
    select(-newid) %>%

    # now lets make sure to group by the country (not the episode!) and arrange by country-year
    group_by(country_id) %>%
    arrange(country_id, year) %>%

    # now we check to see if the country reverted to a closed autocracy within the episode period
    dplyr::mutate(back_closed = ifelse(dplyr::lead(v2x_regime, n=1) == 0 & v2x_regime > 0, year, NA)) %>%
    ungroup() %>%
    group_by(dem_ep_id) %>%
    # here we then find the first time in the episode that a change from another regime to closed autocracy occurs
    dplyr::mutate(back_closed = min(hablar::s(back_closed)),
                  back_closed = ifelse(is.na(dem_ep_id), NA, back_closed),
                  # we recode the last change year as the first year the country reverted back to closed autocracy (e.g. Sudan has this happen before last change)
                  last_ch_year = ifelse(!is.na(back_closed) & last_ch_year>back_closed, back_closed, last_ch_year),
                  # now we can finally code our termination variable
                  # first, lets fill everything in for the episode
                  stasis = ifelse(dem_ep==1, max(hablar::s(stasis)), NA),
                  year_drop = ifelse(dem_ep==1, max(hablar::s(year_drop)), NA),
                  cum_drop = ifelse(dem_ep==1, max(hablar::s(cum_drop)), NA),
                  # then we can code the termination variable
                  dem_ep_termination = ifelse(dem_ep==1 & !is.na(stasis) & is.na(year_drop) & is.na(cum_drop)
                                              & is.na(back_closed), 1, NA),
                  dem_ep_termination = ifelse(dem_ep==1 & !is.na(year_drop) & is.na(back_closed), 2, dem_ep_termination),
                  dem_ep_termination = ifelse(dem_ep==1 & !is.na(cum_drop) & is.na(year_drop) & is.na(back_closed), 3, dem_ep_termination),
                  dem_ep_termination = ifelse(dem_ep==1 & !is.na(back_closed), 4, dem_ep_termination),
                  dem_ep_termination = ifelse(dem_ep==1 & dem_ep_censored==1 & is.na(dem_ep_termination), 0, dem_ep_termination),
                  # now we can clean up the other variables to reflect the true end of the episodes that are not censored
                  # first, let's fix the censored variable
                  dem_ep_censored = ifelse(dem_ep_termination !=0 & dem_ep==1, 0, dem_ep_censored),
                  # then we recode the end year as the final positive change if not censored
                  dem_ep_end_year = ifelse(dem_ep_censored==0 & dem_ep==1, last_ch_year, dem_ep_end_year),
                  # then we clean up the other variables for non-episode years
                  dem_ep_termination = ifelse(dem_ep==1 & year>dem_ep_end_year, NA, dem_ep_termination),
                  dem_ep_start_year = ifelse(dem_ep==1 & year>dem_ep_end_year, NA, dem_ep_start_year),
                  dem_ep_end_year = ifelse(dem_ep==1 & year>dem_ep_end_year, NA, dem_ep_end_year),
                  dem_ep = ifelse(is.na(dem_ep_end_year), 0, dem_ep)) %>%
    ungroup() %>%
    dplyr::mutate(dem_ep_id = ifelse(dem_ep==1, paste(country_text_id, dem_ep_start_year, dem_ep_end_year, sep = "_"), NA)) %>%
    group_by(dem_ep_id) %>%
    arrange(country_id, year) %>%


    ##### code the phase and outcome type of episode

    # we code a variable that captures the type or "phase" of democratization, were 1 = "democratic deepening" and 2 = "autocratic liberalization"
    # note: the year of the democratic transition is included in the autocratic liberalization phase
    dplyr::mutate(sub_dem_ep = ifelse(dem_ep==1 & reg_type==1 & reg_trans!=1, 1, 0),
                  sub_dem_ep = ifelse(dem_ep==1 & (reg_type==0 | (reg_type==1 & reg_trans==1)),
                                      2, sub_dem_ep),
                  sub_dem_ep = ifelse(dem_pre_ep_year==1, dplyr::lead(sub_dem_ep, n=1), sub_dem_ep),
                  # we code the start and end dates for these phases
                  sub_dem_ep_start_year = ifelse(dem_ep==1 & year==dem_ep_start_year, dem_ep_start_year, NA),
                  sub_dem_ep_start_year = ifelse(sub_dem_ep==1 & dplyr::lag(sub_dem_ep==2, n=1), year, sub_dem_ep_start_year),
                  sub_dem_ep_end_year = ifelse(dem_ep==1 & year==dem_ep_end_year, dem_ep_end_year, NA),
                  sub_dem_ep_end_year = ifelse(sub_dem_ep==2 & year!=dem_ep_end_year & dem_pre_ep_year==0 &
                                                 dplyr::lead(sub_dem_ep==1, n=1), year,
                                               sub_dem_ep_end_year)) %>%
    tidyr::fill(sub_dem_ep_start_year) %>%
    tidyr::fill(sub_dem_ep_end_year, sub_dem_ep_start_year, .direction="up") %>%
    dplyr::mutate(sub_dem_ep_id = ifelse(dem_ep==1, paste(country_text_id, sub_dem_ep_start_year, sub_dem_ep_end_year, sep = "_"), NA)) %>%
    ungroup() %>%
    group_by(dem_ep_id) %>%
    # did a regime change on RoW during the episode produce a genuine democratic transition?
    dplyr::mutate(dem_ep_outcome = ifelse(sub_dem_ep==2 & reg_trans==1 & dem_pre_ep_year==0, 1, NA),
                  # did a regime change on RoW during the episode fail to produce a democratic transition?
                  dem_ep_outcome = ifelse(sub_dem_ep==2 & any(row_regch_event==1 & dem_pre_ep_year==0) &
                                            year==dem_ep_end_year & dem_ep_censored==0 &
                                            is.na(dem_ep_outcome), 2, dem_ep_outcome),
                  # did the autocratic liberalization phase result in a stabilized electoral autocracy?
                  dem_ep_outcome = ifelse(sub_dem_ep==2 & year==dem_ep_end_year & dem_ep_termination==1
                                          & is.na(dem_ep_outcome), 3, dem_ep_outcome),
                  # did the autocratic liberalization phase result in a failed liberalization?
                  dem_ep_outcome = ifelse(sub_dem_ep==2 & year==dem_ep_end_year &
                                            (dem_ep_termination==2 | dem_ep_termination==3 | dem_ep_termination==4) &
                                            is.na(dem_ep_outcome), 4, dem_ep_outcome),
                  # code the outcome for completed democratic deepening
                  dem_ep_outcome = ifelse(sub_dem_ep==1 & year==dem_ep_end_year &
                                            dem_ep_censored==0 & is.na(dem_ep_outcome), 5, dem_ep_outcome),
                  # code censored episodes
                  dem_ep_outcome = ifelse(dem_ep==1 & dem_ep_censored==1 & is.na(dem_ep_outcome) & year==dem_ep_end_year, 6, dem_ep_outcome),
                  dem_ep_outcome = ifelse(dem_ep==0, 0, dem_ep_outcome)) %>%
    ungroup() %>%
    group_by(sub_dem_ep_id) %>%
    arrange(country_id, year) %>%
    # fill for the entire phase of episode
    dplyr::mutate(dem_ep_outcome = min(hablar::s(dem_ep_outcome))) %>%
    ungroup() %>%
    group_by(dem_ep_id) %>%
    dplyr::mutate(dem_ep_censored = ifelse(dem_ep==1 & max(dem_ep_outcome)!=6, 0, dem_ep_censored)) %>%
    ungroup() %>%
    group_by(country_text_id) %>%
    arrange(country_id, year) %>%
    select(-stasis)


  #### CODING THE AUTOCRATIZATION EPISODES ####

  ### detect and save potential episodes with the help of the c++ function find_seqs

  full.df <- full.df %>% dplyr::mutate(episode_id = find_seqs_aut(v2x_polyarchy, v2x_regime,
                                                                  start_incl=start_incl*-1, year_turn, cum_turn, tolerance),
                                       # set a temporary id for these potential episodes and group accordinly
                                       character_id = ifelse(!is.na(episode_id), paste(country_text_id, episode_id, sep = "_"), NA)) %>%
    ungroup() %>%
    group_by(character_id) %>%
    # general check: is there a potential autocratization episode?
    dplyr::mutate(aut_ep = ifelse(!is.na(episode_id), 1, 0),
                  # we check whether the cumulated change in each potential episode was substantial (> cum_inc), i.e. the episode is manifest
                  aut_ep = ifelse(aut_ep==1 & min(hablar::s(v2x_polyarchy)) - max(hablar::s(v2x_polyarchy)) <= cum_incl*-1, 1, 0)) %>%
    ungroup() %>%
    # then we clean out variables for non-manifest episodes
    dplyr::mutate(episode_id = ifelse(aut_ep!=1, NA, episode_id),
                  character_id = ifelse(aut_ep!=1, NA, character_id)) %>%
    group_by(character_id) %>%
    # generate the initial end year for the episode (note:  we have to filter out the stasis years that C++ gives us, but we will do this later):
    dplyr::mutate(aut_ep_end_year = ifelse(aut_ep==1, last(year), NA),
                  #  find potentially censored episodes (note: we might change this later depending on termination)
                  aut_ep_censored = ifelse(aut_ep==1 & codingend-aut_ep_end_year<tolerance, 1, 0),
                  aut_ep_censored = ifelse(aut_ep==1 & !is.na(gapstart) & (gapstart-1)-aut_ep_end_year<tolerance, 1, aut_ep_censored),
                  # generate the start year for the potential episode as the first year after the pre-episode year
                  aut_ep_start_year = ifelse(aut_ep==1,first(year)+1, NA),
                  # here we code a dummy for the pre-episode year
                  aut_pre_ep_year = ifelse(aut_ep==1, ifelse(year == dplyr::first(year), 1, 0), 0),
                  # we create a unique identifier for episodes and phases using the country_text_id, start, and end years
                  aut_ep_id = ifelse(aut_ep==1, paste(country_text_id, aut_ep_start_year, aut_ep_end_year, sep = "_"), NA)) %>%
    ungroup() %>%
    # remove the old identifiers we no longer need
    dplyr::select(-character_id, -episode_id) %>%
    # make sure the data is sorted properly
    dplyr::arrange(country_name, year) %>%
    # just to make sure we have a dataframe
    as.data.frame %>%

    ####### code termination type of autocratization episode

    # autocrtization episodes end when one of three things happens:
    # 1. the case experiences an annual increase >= year_turn
    # 2. the case experiences a gradual increase >= cum_turn over the tolerance period (or less)
    # 3. the case experiences no annual decrease = start_incl for the tolerance period (or more)

    # first find the last negative change on EDI equal to the start_incl parameter
    group_by(aut_ep_id) %>%
    dplyr::mutate(last_ch_year = max(hablar::s(ifelse(v2x_polyarchy-dplyr::lag(v2x_polyarchy, n=1)<=start_incl*-1, year, NA))),
                  # here we just replace with NA non-episode years
                  last_ch_year = ifelse(aut_ep==0, NA, last_ch_year)) %>%
    # now lets make sure to group by the country (not the episode!) and arrange by country-year
    group_by(country_id) %>%
    arrange(country_id, year)

  #### then check to see what happened the after the episode had its last substantive change equal to start_incl

  # we start with the yearly increase, aka year_turn
  year_incr <- list()
  # here we loop over the number of years (n) equal to the tolerance period after the last_change_year
  for (i in 1:tolerance) {
    # we calculate the first difference in the EDI for each of these yearly changes within the tolernce
    year_incr[[i]] <- ifelse(full.df$year == full.df$last_ch_year & dplyr::lead(full.df$country_id, n=i)==full.df$country_id, dplyr::lead(full.df$v2x_polyarchy, n=i)-dplyr::lead(full.df$v2x_polyarchy, n=i-1), NA)
  }
  # then we generate a dataframe from these calculations
  df1 <- do.call(cbind, lapply(year_incr, data.frame, stringsAsFactors=FALSE))
  # this just renames the columns to match the years ahead we are looking
  names <- paste0('year', seq(1:tolerance))
  colnames(df1) <- names
  # this transforms the result into a dataframe that we can use as a column in our existing dataframe
  # first write a function to deal with INF warnings
  my.max <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)
  year_incr <- df1 %>%
    dplyr::mutate(year_incr = ifelse(apply(df1, 1, FUN = my.max) > year_turn, 1,NA))  %>%
    dplyr::select(year_incr)
  # now we can also use the first-differences we calculated above to look for stasis as well
  # this transforms the result into a dataframe that we can use as a column in our existing dataframe
  # note - we will have to clean this up later to account for cum_turn as well
  stasis <- df1 %>%
    # this checks whether the maximum annual change is less than start_incl over the tolerance period &
    #    that it is also greater than the year_turn parameter, i.e. stasis
    dplyr::mutate(stasis = ifelse(apply(df1, 1, FUN = min) > start_incl*-1 & apply(df1, 1, FUN = max) <= year_turn, 1,NA))  %>%
    dplyr::select(stasis)

  # now we look for a gradual drop equal to cum_drop over the tolerance period
  cum_incr <- list()
  # here we loop over time equal to the tolerance, looking for the difference between the last_change_year and that year on the EDI
  for (i in 1:tolerance) {
    cum_incr[[i]] <- ifelse(full.df$year == full.df$last_ch_year & dplyr::lead(full.df$country_id, n=i)==full.df$country_id, dplyr::lead(full.df$v2x_polyarchy, n=i)-full.df$v2x_polyarchy, NA)
  }
  # then we rename the columns and generate a dataframe we can use for our existing data
  df <- do.call(cbind, lapply(cum_incr, data.frame, stringsAsFactors=FALSE))
  names <- paste0('cum', seq(1:tolerance))
  colnames(df) <- names
  cum_incr <- df %>%
    dplyr::mutate(cum_incr = ifelse(apply(df, 1, FUN = my.max) >= cum_turn, 1,NA)) %>%
    dplyr::select(cum_incr)

  # merge these new columns to our full.df
  full.df <- full.df %>%
    tibble::rownames_to_column('newid') %>%
    left_join(tibble::rownames_to_column(year_incr, 'newid'), by = 'newid') %>%
    left_join(tibble::rownames_to_column(cum_incr, 'newid'), by = 'newid') %>%
    left_join(tibble::rownames_to_column(stasis, 'newid'), by = 'newid') %>%

    # now lets make sure to group by the autocratization episode and arrange by country-year
    ungroup() %>%
    group_by(aut_ep_id) %>%
    # now we can finally code our termination variable
    # first, lets fill everything in for the episode
    dplyr::mutate(stasis = ifelse(aut_ep==1, max(hablar::s(stasis)), NA),
                  year_incr = ifelse(aut_ep==1, max(hablar::s(year_incr)), NA),
                  cum_incr = ifelse(aut_ep==1, max(hablar::s(cum_incr)), NA),
                  # then we can code the termination variable
                  aut_ep_termination = ifelse(aut_ep==1 & !is.na(stasis) & is.na(year_incr) & is.na(cum_incr),
                                              1, NA),
                  aut_ep_termination = ifelse(aut_ep==1 & !is.na(year_incr), 2, aut_ep_termination),
                  aut_ep_termination = ifelse(aut_ep==1 & !is.na(cum_incr) & is.na(year_incr), 3, aut_ep_termination),
                  aut_ep_termination = ifelse(aut_ep==1 & aut_ep_censored==1 & is.na(aut_ep_termination), 0, aut_ep_termination),
                  # now we can clean up the other variables to reflect the true end of the episodes that are not censored
                  # first, let's fix the censored variable
                  aut_ep_censored = ifelse(aut_ep_termination !=0 & aut_ep==1, 0, aut_ep_censored),
                  # then we recode the end year as the final positive change if not censored
                  aut_ep_end_year = ifelse(aut_ep_censored==0 & aut_ep==1, last_ch_year, aut_ep_end_year),
                  # then we clean up the other variables for non-episode years
                  aut_ep_termination = ifelse(aut_ep==1 & year>aut_ep_end_year, NA, aut_ep_termination),
                  aut_ep_start_year = ifelse(aut_ep==1 & year>aut_ep_end_year, NA, aut_ep_start_year),
                  aut_ep_end_year = ifelse(aut_ep==1 & year>aut_ep_end_year, NA, aut_ep_end_year),
                  aut_ep = ifelse(is.na(aut_ep_end_year), 0, aut_ep)) %>%
    ungroup() %>%
    dplyr::mutate(aut_ep_id = ifelse(aut_ep==1, paste(country_text_id, aut_ep_start_year, aut_ep_end_year, sep = "_"), NA)) %>%
    group_by(aut_ep_id) %>%
    arrange(country_id, year) %>%

    ##### code the phase and outcome type of episode

    # we code a variable that captures the type or "phase" of autocratization, were 1 = "democratic regression" and 2 = "autocratic regression"
    # note: the year of the democratic breakdown is included in the democratic regression phase
    dplyr::mutate(sub_aut_ep = ifelse(aut_ep==1 & (reg_type==1 | (reg_type==0 & reg_trans==-1)), 1, 0),
                  sub_aut_ep = ifelse(aut_ep==1 & reg_type==0 & reg_trans!=-1, 2, sub_aut_ep),
                  sub_aut_ep = ifelse(aut_pre_ep_year==1, dplyr::lead(sub_aut_ep, n=1), sub_aut_ep),
                  # we code the start and end dates for these phases
                  sub_aut_ep_start_year = ifelse(aut_ep==1 & year==aut_ep_start_year, aut_ep_start_year, NA),
                  sub_aut_ep_start_year = ifelse(sub_aut_ep==2 & dplyr::lag(sub_aut_ep==1, n=1), year, sub_aut_ep_start_year),
                  sub_aut_ep_end_year = ifelse(aut_ep==1 & year==aut_ep_end_year, aut_ep_end_year, NA),
                  sub_aut_ep_end_year = ifelse(sub_aut_ep==1 & year!=aut_ep_end_year & aut_pre_ep_year==0 &
                                                 dplyr::lead(sub_aut_ep==2, n=1), year,
                                               sub_aut_ep_end_year)) %>%
    tidyr::fill(sub_aut_ep_start_year) %>%
    tidyr::fill(sub_aut_ep_end_year, sub_aut_ep_start_year, .direction="up") %>%
    dplyr::mutate(sub_aut_ep_id = ifelse(aut_ep==1, paste(country_text_id, sub_aut_ep_start_year, sub_aut_ep_end_year, sep = "_"), NA)) %>%
    ungroup() %>%
    group_by(aut_ep_id) %>%
    # did a regime change on RoW during the episode of democratic regression produce a genuine democratic breakdown?
    dplyr::mutate(aut_ep_outcome = ifelse(sub_aut_ep==1 & reg_trans==-1 & aut_pre_ep_year==0, 1, NA),
                  # did the episode of democratic regression fail to produce a democratic breakdown?
                  aut_ep_outcome = ifelse(sub_aut_ep==1 & aut_ep_censored==0 & aut_pre_ep_year==0 &
                                            is.na(aut_ep_outcome), 2, aut_ep_outcome),
                  # code the outcome for completed autocratic regression
                  aut_ep_outcome = ifelse(sub_aut_ep==2 & year==aut_ep_end_year &
                                            aut_ep_censored==0 & is.na(aut_ep_outcome), 3, aut_ep_outcome),
                  # code censored episodes
                  aut_ep_outcome = ifelse(aut_ep==1 & aut_ep_censored==1 & is.na(aut_ep_outcome) & year==aut_ep_end_year, 4, aut_ep_outcome),
                  aut_ep_outcome = ifelse(aut_ep==0, 0, aut_ep_outcome)) %>%
    ungroup() %>%
    group_by(sub_aut_ep_id) %>%
    arrange(country_id, year) %>%
    # fill for the entire phase of episode
    dplyr::mutate(aut_ep_outcome = min(hablar::s(aut_ep_outcome))) %>%
    ungroup() %>%
    group_by(aut_ep_id) %>%
    dplyr::mutate(aut_ep_censored = ifelse(aut_ep==1 & max(aut_ep_outcome)!=4, 0, aut_ep_censored)) %>%
    ungroup() %>%
    group_by(country_text_id) %>%
    arrange(country_id, year) %>%

    # clean out values from pre-episode year
    dplyr::mutate(dem_ep = ifelse(dem_pre_ep_year==1, 0, dem_ep),
                  dem_ep_termination = ifelse(dem_pre_ep_year==1, NA, dem_ep_termination),
                  sub_dem_ep = ifelse(dem_pre_ep_year==1, 0, sub_dem_ep),
                  dem_ep_outcome_all = dem_ep_outcome,
                  dem_ep_outcome = ifelse(dem_pre_ep_year==1, 0, dem_ep_outcome),
                  dem_ep_censored = ifelse(dem_pre_ep_year==1, 0, dem_ep_censored),
                  aut_ep = ifelse(aut_pre_ep_year==1, 0, aut_ep),
                  aut_ep_termination = ifelse(aut_pre_ep_year==1, NA, aut_ep_termination),
                  sub_aut_ep = ifelse(aut_pre_ep_year==1, 0, sub_aut_ep),
                  aut_ep_outcome_all = aut_ep_outcome,
                  aut_ep_outcome = ifelse(aut_pre_ep_year==1, 0, aut_ep_outcome),
                  aut_ep_censored = ifelse(aut_pre_ep_year==1, 0, aut_ep_censored)) %>%

    # select the variables we need to keep
    filter(!is.na(origsample)) %>%
    select(country_id, country_text_id, country_name, year, v2x_regime, v2x_polyarchy, v2x_polyarchy_codelow, v2x_polyarchy_codehigh,
           reg_start_year, reg_end_year, reg_id, reg_type, reg_trans, founding_elec, row_regch_event, row_regch_censored,
           dem_ep, dem_ep_id, dem_ep_start_year, dem_ep_end_year, dem_pre_ep_year, dem_ep_termination,
           sub_dem_ep, sub_dem_ep_id, sub_dem_ep_start_year, sub_dem_ep_end_year, dem_ep_outcome, dem_ep_censored,
           aut_ep, aut_ep_id, aut_ep_start_year, aut_ep_end_year, aut_pre_ep_year, aut_ep_termination,
           sub_aut_ep, sub_aut_ep_id, sub_aut_ep_start_year, sub_aut_ep_end_year, aut_ep_outcome, aut_ep_censored)


  {
    return(full.df)
  }
}
  ### done ;-) ###
