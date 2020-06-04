# A sequence starts when there is a positive change of a certain
# threshold (start_incl). It ends according to one of the following
# conditions:
#  - Single unit decrease past a threshold (year_turn)
#  - Cumulative decrease over a period where there is no change over
#    the starting threshold (start_incl)
#  - No further changes over the starting threshold (start_incl) for a
#    fixed period of time (tolerance)
#  - Missing value
#  - v2x_regime reverts to 0
###

test_that("Invalid function arguments", {
    expect_error(find_seqs_dem(c(1, 2), 1))
    expect_error(find_seqs_dem(1, 1, start_incl = -1))
    expect_error(find_seqs_dem(1, 1, year_turn = 1))
    expect_error(find_seqs_dem(1, 1, cum_turn = 1))
    expect_error(find_seqs_dem(1, 1, tolerance = 0))
})

test_that("Basic increasing seq", {
    polyarchy <- 1:10
    regime <- 1:10

    # Basic single increasing sequence with default arguments
    out <- find_seqs_dem(polyarchy, regime)
    expect_equal(out, rep(1, 10))

    # Higher threshold for initial increase means that there should be
    # no democratization sequence
    out <- find_seqs_dem(polyarchy, regime, start_incl = 2)
    expect_equal(out, rep(NA_real_, 10))
})

test_that("Multiple sequences", {
    polyarchy <- c(1:2, 1, 5)
    regime <- rep(1, 4)

    out <- find_seqs_dem(polyarchy, regime)
    expect_equal(out, c(1, 1, 2, 2))

    out <- find_seqs_dem(polyarchy, regime, start_inc = 2)
    expect_equal(out, c(NA, NA, 1, 1))

    out <- find_seqs_dem(polyarchy, regime, year_turn = -2)
    expect_equal(out, c(1, 1, 2, 2))

    out <- find_seqs_dem(polyarchy, regime, cum_turn = -2)
    expect_equal(out, c(1, 1, 2, 2))

    out <- find_seqs_dem(polyarchy, regime, year_turn = -2, cum_turn = -2)
    expect_equal(out, c(1, 1, 1, 1))

    polyarchy <- c(1:2, 1, 0, 0)
    regime <- rep(1, 5)

    out <- find_seqs_dem(polyarchy, regime, year_turn = -1, cum_turn = -1)
    expect_equal(out, c(1, 1, 1, NA, NA))
})

test_that("Stasis", {
    polyarchy <- c(1, rep(2, 4))
    regime <- rep(1, 5)

    out <- find_seqs_dem(polyarchy, regime)
    expect_equal(out, rep(1, 5))

    out <- find_seqs_dem(polyarchy, regime, tolerance = 1)
    expect_equal(out, c(1, 1, NA, NA, NA))

    out <- find_seqs_dem(polyarchy, regime, tolerance = 2)
    expect_equal(out, c(1, 1, 1, NA, NA))

    # TODO: Is this the expected behaviour we want?
    out <- find_seqs_dem(polyarchy, regime, tolerance = 3)
    expect_equal(out, rep(1, 5))

    polyarchy <- c(2, 1, 2, 1, 1, 1)
    regime <- rep(1, 6)

    out <- find_seqs_dem(polyarchy, regime)
    expect_equal(out, c(NA, 1, 1, NA, NA, NA))

    # TODO: Is this the expected behaviour we want?
    out <- find_seqs_dem(polyarchy, regime, year_turn = -2, cum_turn = -2, tolerance = 2)
    expect_equal(out, c(NA, 1, 1, 1, NA, NA))
})

test_that("Handles NA", {
    polyarchy <- c(NA, 1:2)
    regime <- c(NA, 1, 1)

    out <- find_seqs_dem(polyarchy, regime)
    expect_equal(out, c(NA, 1, 1))

    polyarchy <- c(1:2, NA)
    regime <- c(1, 1, NA)

    out <- find_seqs_dem(polyarchy, regime)
    expect_equal(out, c(1, 1, NA))

    polyarchy <- c(1, NA, 2, 3, NA)
    regime <- c(1, NA, 1, 1, NA)

    out <- find_seqs_dem(polyarchy, regime)
    expect_equal(out, c(NA, NA, 1, 1, NA))

    polyarchy <- c(NA, 1, 2, NA, 1, 0)
    regime <- c(NA, 1, 1, NA, 1, 1)

    out <- find_seqs_dem(polyarchy, regime)
    expect_equal(out, c(NA, 1, 1, NA, NA, NA))

    out <- find_seqs_dem(polyarchy, regime, year_turn = -2, cum_turn = -2)
    expect_equal(out, c(NA, 1, 1, NA, NA, NA))
})

test_that("Decrease in v2x_regime", {
    polyarchy <- 1:2
    regime <- c(1, 0)

    out <- find_seqs_dem(polyarchy, regime)
    expect_equal(out, c(1, 1))

    polyarchy <- 1:3
    regime <- c(1, 0, 0)

    # TODO: Is this the expected behaviour we want?
    out <- find_seqs_dem(polyarchy, regime)
    expect_equal(out, c(1, 2, 2))

    polyarchy <- c(1:2, 2, 2, 2)
    regime <- c(1, 1, 1, 0, 1)

    # TODO: Is this the expected behaviour we want?
    out <- find_seqs_dem(polyarchy, regime)
    expect_equal(out, c(1, 1, 1, NA, NA))

    polyarchy <- 1:3
    regime <- c(2, 1, 1)

    out <- find_seqs_dem(polyarchy, regime)
    expect_equal(out, c(1, 1, 1))

    polyarchy <- 1:2
    regime <- c(0, 0)

    out <- find_seqs_dem(polyarchy, regime)
    expect_equal(out, c(1, 1))
})
