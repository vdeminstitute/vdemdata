#' An interactive function to fix the overlap between episodes of democratization and autocratization.
#'
#' @param episodes The outcome of get_eps(), episodes of regime transformation (ERT), to be used for fixing potential overlaps (depending on individual parameter setting). By default with standard parameters.
#'
#' @param overlap The outcome of find_overlap(merged), depending on individual parameter settings.
#'
#' @return A "merged" dataframe after recoding overlapping episodes based on the interactive function.
#'
#' @import dplyr
#' @export
#'
#' @examples
#' #Don't run
#' #Fix the overlap between democratization and autocratization episodes
#'
#' #fix_overlap(episodes)
#'
fix_overlap <- function(
  episodes = vdemdata::get_eps(),
  overlap = vdemdata::find_overlap(episodes))
{
  merged <- episodes
  method <- menu(c("Manually (case-by-case)", "Assign all the same way"), title= "How do you want to fix the overlap?")
  if(method == 2){
    which_assign <- menu(c("As autocratization episode years", "As democratization episode years", "To both","To neither"), title="How would you like to assign the overlappling country-years?")
    if(which_assign == 1){
      for(i in nrow(overlap)){
        assign = "Autocratization episodes"
        merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2]) & merged$year == as.numeric(overlap[i,3]),
               c("dem_ep", "dem_pre_ep_year", "sub_dem_ep", "dem_ep_outcome", "dem_ep_censored")] <- 0
        merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
               c("dem_ep_id", "dem_ep_start_year", "dem_ep_end_year", "dem_ep_termination", "sub_dem_ep_id", "sub_dem_ep_start_year", "sub_dem_ep_end_year")] <- NA
      }
    }
    if(which_assign == 2){
      for(i in nrow(overlap)){
        assign = "Democratization episodes"
        merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
               c("aut_ep", "aut_pre_ep_year", "sub_aut_ep", "aut_ep_outcome", "aut_ep_censored")] <- 0
        merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
               c("aut_ep_id", "aut_ep_start_year", "aut_ep_end_year", "aut_ep_termination", "sub_aut_ep_id", "sub_aut_ep_start_year", "sub_aut_ep_end_year")] <- NA
      }
    }
    if(which_assign == 3){
      assign = "both"
    }
    if(which_assign == 4){
      for(i in nrow(overlap)){
        assign = "neither"
        merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
               c("dem_ep", "dem_pre_ep_year", "sub_dem_ep", "dem_ep_outcome", "dem_ep_censored")] <- 0
        merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
               c("dem_ep_id", "dem_ep_start_year", "dem_ep_end_year", "dem_ep_termination", "sub_dem_ep_id", "sub_dem_ep_start_year", "sub_dem_ep_end_year")] <- NA
        merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
               c("aut_ep", "aut_pre_ep_year", "sub_aut_ep", "aut_ep_outcome", "aut_ep_censored")] <- 0
        merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
               c("aut_ep_id", "aut_ep_start_year", "aut_ep_end_year", "aut_ep_termination", "sub_aut_ep_id", "sub_aut_ep_start_year", "sub_aut_ep_end_year")] <- NA
      }
    }

    print(paste0("Thank you. All overlapping country-years have been assigned to ", assign))
  }
  if(method == 1){
    overlap2 <- overlap %>% left_join(merged)
    for(i in unique(overlap2$year)){
      dat <- overlap2 %>% filter(year == i)
      start_year <- dat$year[1] - 5
      end_year <- dat$year[nrow(dat)] + 5
      merged_overlap <- merged %>%
        filter(country_name == dat$country_name, year >= start_year, year <= end_year) %>%
        dplyr::select(country_name, year, v2x_regime, v2x_polyarchy) %>%
        mutate(overlap_ind = case_when(year %in% dat$year ~ 1,
                                       T~0)) #%>%
      # mutate(country_name = case_when(overlap_ind == 1~ crayon::red(as.character(country_name)),
      #                                 T~as.character(country_name)),
      #        year = case_when(overlap_ind == 1~ crayon::red(as.character(year)),
      #                                 T~as.character(year)),
      #        v2x_regime = case_when(overlap_ind == 1~ crayon::red(as.character(v2x_regime)),
      #                                 T~as.character(v2x_regime)),
      #        v2x_polyarchy = case_when(overlap_ind == 1~ crayon::red(as.character(v2x_polyarchy)),
      #                                 T~as.character(v2x_polyarchy)),
      #        overlap_ind = case_when(overlap_ind == 1~ crayon::red(as.character(overlap_ind)),
      #                                  T~as.character(overlap_ind)))

      print(as.data.frame(merged_overlap))
      which_assign <- menu(c("Assign overlap to autocratization", "Assign overlap to democratization", "Assign to both", "Assign to neither"), title= "How do you want to fix the overlap?")
      if(which_assign == 1){
        for(i in 1:nrow(dat)){
          assign = "Autocratization episodes"
          merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                 c("dem_ep", "dem_pre_ep_year", "sub_dem_ep", "dem_ep_outcome", "dem_ep_censored")] <- 0
          merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                 c("dem_ep_id", "dem_ep_start_year", "dem_ep_end_year", "dem_ep_termination", "sub_dem_ep_id", "sub_dem_ep_start_year", "sub_dem_ep_end_year")] <- NA
        }
      }
      if(which_assign == 2){
        for(i in 1:nrow(dat)){
          assign = "Democratization episodes"
          merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                 c("aut_ep", "aut_pre_ep_year", "sub_aut_ep", "aut_ep_outcome", "aut_ep_censored")] <- 0
          merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                 c("aut_ep_id", "aut_ep_start_year", "aut_ep_end_year", "aut_ep_termination", "sub_aut_ep_id", "sub_aut_ep_start_year", "sub_aut_ep_end_year")] <- NA
        }
      }
      if(which_assign == 3){
        assign = "both"
      }
      if(which_assign == 4){
        for(i in 1:nrow(dat)){
          assign = "neither"
          merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                 c("dem_ep", "dem_pre_ep_year", "sub_dem_ep", "dem_ep_outcome", "dem_ep_censored")] <- 0
          merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                 c("dem_ep_id", "dem_ep_start_year", "dem_ep_end_year", "dem_ep_termination", "sub_dem_ep_id", "sub_dem_ep_start_year", "sub_dem_ep_end_year")] <- NA
          merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                 c("aut_ep", "aut_pre_ep_year", "sub_aut_ep", "aut_ep_outcome", "aut_ep_censored")] <- 0
          merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                 c("aut_ep_id", "aut_ep_start_year", "aut_ep_end_year", "aut_ep_termination", "sub_aut_ep_id", "sub_aut_ep_start_year", "sub_aut_ep_end_year")] <- NA
        }
      }
      if(which_assign %in% c(1:4)){
        print(paste("The overlapping country-years for this episode in",dat$country_name,"have been assigned to", assign))}
      if(which_assign == 5){
        print(as.data.frame(merged_dat))
        for(i in dat$year){
          ind_year <- menu(c("Assign overlap to autocratization", "Assign overlap to democratization", "Assign to both", "Assign to neither"), title= paste0("How do you want to fix the dat for ",i,"?"))
          if(ind_year == 1){
            for(i in 1:nrow(dat)){
              assign = "Autocratization episodes"
              merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                     c("dem_ep", "dem_pre_ep_year", "sub_dem_ep", "dem_ep_outcome", "dem_ep_censored")] <- 0
              merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                     c("dem_ep_id", "dem_ep_start_year", "dem_ep_end_year", "dem_ep_termination", "sub_dem_ep_id", "sub_dem_ep_start_year", "sub_dem_ep_end_year")] <- NA
            }
          }
          if(ind_year == 2){
            for(i in 1:nrow(dat)){
              assign = "Democratization episodes"
              merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                     c("aut_ep", "aut_pre_ep_year", "sub_aut_ep", "aut_ep_outcome", "aut_ep_censored")] <- 0
              merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                     c("aut_ep_id", "aut_ep_start_year", "aut_ep_end_year", "aut_ep_termination", "sub_aut_ep_id", "sub_aut_ep_start_year", "sub_aut_ep_end_year")] <- NA
            }
          }
          if(ind_year == 3){
            assign = "both"
          }
          if(ind_year == 4){
            for(i in 1:nrow(dat)){
              assign = "neither"
              merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                     c("dem_ep", "dem_pre_ep_year", "sub_dem_ep", "dem_ep_outcome", "dem_ep_censored")] <- 0
              merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                     c("dem_ep_id", "dem_ep_start_year", "dem_ep_end_year", "dem_ep_termination", "sub_dem_ep_id", "sub_dem_ep_start_year", "sub_dem_ep_end_year")] <- NA
              merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                     c("aut_ep", "aut_pre_ep_year", "sub_aut_ep", "aut_ep_outcome", "aut_ep_censored")] <- 0
              merged[merged$country_text_id == as.character(overlap[i,1]) & merged$country_name == as.character(overlap[i,2])& merged$year == as.numeric(overlap[i,3]),
                     c("aut_ep_id", "aut_ep_start_year", "aut_ep_end_year", "aut_ep_termination", "sub_aut_ep_id", "sub_aut_ep_start_year", "sub_aut_ep_end_year")] <- NA
            }
          }
          print(paste0("You have assigned the ",i," observation to ", assign))
        }
      }
    }
  }
  return(merged)
}

