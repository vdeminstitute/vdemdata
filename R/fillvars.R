#' Fill election-specific variables in V-Dem data
#' 
#' A function to carryforward election-specific variables in V-Dem provided no interruption in 
#' the electoral regime (v2x_elecreg). 
#' 
#' Some variables in the V-Dem dataset are coded at the election-year only. However, users 
#' may wish to include these in country-year analyses. This function fills the election specific
#' indicators for subsequent years, provided that there has not been any interruption in the 
#' electoral regime, as defined by v2x_elecreg. 
#' 
#' More specifically, it carries forward values between elections for all variables that are 
#' election-specific, unless v2x_elecreg becomes zero. When v2x_elecreg becomes zero, 
#' users may either fill the resulting missing values with zeros (the smallest possible score).
#' 
#' Note: This function only fills variables with the suffix _osp, _ord, _mean, and _nr. The estimates
#' from the measurement model are not included because these do not have an absolute low point, on
#' their approximate z-score distributions. Future iterations of this function may provide additonal
#' options for filling these values with some theoretical minimum. 
#' 
#' Specifically, the variables filled include:
#' "v2elmulpar", "v2elpeace", "v2elrgstry", "v2elvotbuy", "v2elirreg", "v2elintim",  "v2elboycot", "v2elfrcamp", 
#' "v2elpdcamp", "v2elfrfair", "v2elaccept", "v2elasmoff".
#' 
#' @param fill_na Whether the NA values for breaks in the electoral regime should be filled with zeros. The default = TRUE.
#' 
#' @return A data frame with the most recent V-Dem dataset from the vdemdata package where election variables have 
#' been filled. 
#' 
#' @import dplyr

fillvars <- function(fill_na = TRUE) {
  
# here are the election-specific variables
vars <- vdemdata::vdem  %>%
    select(contains(c("v2elmulpar", "v2elpeace", "v2elrgstry", "v2elvotbuy", 
                      "v2elirreg", "v2elintim",  "v2elboycot", "v2elfrcamp", "v2elpdcamp", "v2elfrfair", 
                      "v2elaccept", "v2elasmoff"))) %>%
    select(contains(c("_osp", "_ord", "_mean", "_nr"))) %>%
    colnames()  
# let's start with the vdem dataset
vdem_filled <- vdemdata::vdem %>% 
# here we group and arrange the data into country-year 
group_by(country_text_id) %>%
  arrange(country_text_id, year) %>%
# then we generate an identifier for each electoral regime
  # starting with the first observed year, then every time elecreg becomes zero
  mutate(elec_reg_start = ifelse(year==min(year) | (v2x_elecreg == 0 & lag(v2x_elecreg==1, n=1)), year, NA)) %>%
  # then we fill this identifier within the country
  tidyr::fill(elec_reg_start, .direction="down") %>%
  # now re-group by the new electoral regime identifier
  group_by(country_text_id, elec_reg_start) %>%
  arrange(country_text_id, year) %>%
  tidyr::fill(all_of(vars), .direction = "down") %>%
  ungroup() %>%
  select(-c(elec_reg_start))

# then we can optionally fill the election variables with zeros if there is no electoral regime (v2x_elecreg==0)
  # note: this isn't exactly precise, but gives lowest possible scores to cases without electoral regime
if(fill_na==TRUE) {
  vdem_filled <- vdem_filled %>%
  purrr::modify_at(vars, ~ if_else(is.na(.) & vdem_filled$v2x_elecreg==0, 0, .))
} else {
  vdem_filled <- vdem_filled
}
{
return(vdem_filled)
}
}

