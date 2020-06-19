# ==========================================================================
# Test get_eps() function
# ==========================================================================
context("Produce data frame with the data for ERT")

#
# Let's test data inputs for the first argument because the rest is already
# covered by the tests. Ideally, what we would like to get as a result is 
#

col_names <- c("country_name", "country_id", "country_text_id",
	"year", "codingstart", "codingend",
	"v2x_polyarchy", "gapstart1", "gapstart2", "gapstart3",
	"gapend1", "gapend2", "gapend3", "v2x_regime",
	"v2elasmoff_ord", "v2eltype_0", "v2eltype_4",
	"v2eltype_6","v2x_polyarchy_codelow","v2x_polyarchy_codehigh")

new_cols <- c("country_id", "country_text_id", "country_name",
	"year", "v2x_regime", "v2x_polyarchy", "v2x_polyarchy_codelow",
	"v2x_polyarchy_codehigh", "reg_start_year", "reg_end_year",
	"reg_id", "reg_type", "reg_trans", "founding_elec", "row_regch_event",
	"row_regch_censored", "dem_ep", "dem_ep_id", "dem_ep_start_year",
	"dem_ep_end_year", "dem_pre_ep_year", "dem_ep_termination",
	"sub_dem_ep", "sub_dem_ep_id", "sub_dem_ep_start_year", "sub_dem_ep_end_year",
	"dem_ep_outcome", "dem_ep_censored", "aut_ep", "aut_ep_id",
	"aut_ep_start_year", "aut_ep_end_year", "aut_pre_ep_year",
	"aut_ep_termination", "sub_aut_ep", "sub_aut_ep_id",
	"sub_aut_ep_start_year", "sub_aut_ep_end_year", "aut_ep_outcome",
	"aut_ep_censored")

df <- matrix(1:180, ncol = 20, dimnames = list(NULL, col_names))

test_that("Wrong input format", {
	expect_error(get_eps(data = NULL))
	expect_error(get_eps(data = c(1:10)))
	expect_error(get_eps(data = df))
	})

df <- as.data.frame(df)
df$year <- 1898:{1898 + 8}

test_that("Variables from script are in the dataset", {
	expect_error(get_eps(data = df[, -4]))
	expect_error(get_eps(data = df[, -5]))
	expect_error(get_eps(data = df[, -6]))
	expect_error(get_eps(data = df[, -7]))
	expect_error(get_eps(data = df[, -14]))
	expect_error(get_eps(data = df[, -15]))
	expect_error(get_eps(data = df[, -16]))
	expect_error(get_eps(data = df[, -17]))
	expect_error(get_eps(data = df[, -18]))
	})

df$v2x_regime[1:3] <- NA_real_
df$v2x_polyarchy[4:5] <- NA_real_
data <- vdemdata::vdem

test_that("Data dimensions", {
	expect_equal(nrow(get_eps()), nrow(data[data$year >= 1900,]))
	expect_equal(ncol(get_eps()), length(new_cols))
	# what if we introduce some missingness to v2x_regime
	expect_equal(nrow(get_eps(data = df)), nrow(df[df$year >= 1900,]))
	})

test_that("Check column type output", {
	expect_equal(class(get_eps()$v2x_polyarchy), class(vdemdata::vdem$v2x_polyarchy))
	expect_equal(class(get_eps()$v2x_regime), class(vdemdata::vdem$v2x_regime))
	expect_equal(class(get_eps()$v2x_polyarchy_codelow), class(vdemdata::vdem$v2x_polyarchy_codelow))
	expect_equal(class(get_eps()$v2x_polyarchy_codehigh), class(vdemdata::vdem$v2x_polyarchy_codehigh))
	})

test_that("Missingness in new variables", {
	expect_equal(sum(is.na(get_eps(data = df)$v2x_regime)), sum(is.na(df$v2x_regime[df$year >= 1900])))
	expect_equal(sum(is.na(get_eps(data = df)$v2x_polyarchy)), sum(is.na(df$v2x_polyarchy)))
	})

test_that("Equal values in input and output", {
	expect_equal(get_eps(data = df)$v2x_polyarchy, df[df$year >= 1900,"v2x_polyarchy"])
	expect_equal(get_eps(data = df)$v2x_regime, df[df$year >= 1900,"v2x_regime"])	
	})

df$v2x_regime <- c(NA, NA, NA, 8, 8, 8, 8, 8, 8)
df$v2eltype_0 <- 1

test_that("Nonsensical values", {
	# should we get anything except for NA if v2x_regime is missing?
	expect_equal(sum(is.na(get_eps(data = df)$dem_ep)), sum(is.na(df[df$year >= 1900, "v2x_regime"])))
	# should we get the same reg_end_year if the number of rows for each country is 1?
	expect_equal(get_eps(data = df)$reg_end_year, df[df$year >= 1900, "year"])
	})

df <- rbind.data.frame(df, df[c(4, 7),])

test_that("Several rows of nonsensical data", {
	expect_error(get_eps(data = df)) # should it throw an error if there're duplicated rows in id columns?
	# or deat with this somehow?
	expect_equal(nrow(get_eps(data = df)), {nrow(df[df$year >= 1900,]) - 1})
	})

df[10, "year"] <- 1902
df[11, "year"] <- 1905

test_that("Non-duplicating several rows per country", {
	# should it be 1906 in every value?
	expect_equal(get_eps(data = df)$reg_end_year, c(NA_real_, 1902, 1902, 1902, 1903, 1905, 1905, 1905, 1906))
	expect_equal(sum(!is.na(get_eps(data = df)$reg_trans)), 2)
	expect_equal(nrow(get_eps(data = df)), nrow(df[df$year >= 1900,]))
	})

rm(data, col_names, new_cols, df)