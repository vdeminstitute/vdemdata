# A sequence starts when there is a negative change of a certain
# threshold (start_incl). It ends according to one of the following
# conditions:
#  - Single unit increase past the threshold (year_turn)
#  - Cumulative increase over a period where there is no change over
#    the starting threshold (start_incl)
#  - No further changes below the starting threshold (start_incl) for
#    a fixed period of time (tolerance)
#  - Missing value
#  - v2x_regime reverts to 3
###

test_that("Invalid function arguments", {
    expect_error(find_seqs_aut(c(1, 2), 1))
    expect_error(find_seqs_aut(1, 1, start_incl = 1))
    expect_error(find_seqs_aut(1, 1, year_turn = -1))
    expect_error(find_seqs_aut(1, 1, cum_turn = -1))
    expect_error(find_seqs_aut(1, 1, tolerance = 0))
})

test_that("Basic decreasing seq", {
    polyarchy <- 10:1
    regime <- rep(1, 10)

    out <- find_seqs_aut(polyarchy, regime)
    expect_equal(out, rep(1, 10))

    # Increase the start_incl threshold
    out <- find_seqs_aut(polyarchy, regime, start_incl = -2)
    expect_equal(out, rep(NA_real_, 10))
})

test_that("Multiple sequences", {
    polyarchy <- c(2:1, 3, 1)
    regime <- rep(1, 4)

    out <- find_seqs_aut(polyarchy, regime)
    expect_equal(out, c(1, 1, 2, 2))

    out <- find_seqs_aut(polyarchy, regime, start_incl = -2)
    expect_equal(out, c(NA, NA, 1, 1))

    out <- find_seqs_aut(polyarchy, regime, cum_turn = 2, year_turn = 2)
    expect_equal(out, c(1, 1, 1, 1))

    polyarchy <- c(2:1, 2, 3, 3)
    regime <- rep(1, 5)

    out <- find_seqs_aut(polyarchy, regime, year_turn = 1, cum_turn = 1)
    expect_equal(out, c(1, 1, 1, NA, NA))
})

test_that("Stasis", {
    polyarchy <- c(2, rep(1, 4))
    regime <- rep(1, 5)

    out <- find_seqs_aut(polyarchy, regime)
    expect_equal(out, rep(1, 5))

    out <- find_seqs_aut(polyarchy, regime, tolerance = 1)
    expect_equal(out, c(1, 1, NA, NA, NA))

    out <- find_seqs_aut(polyarchy, regime, tolerance = 2)
    expect_equal(out, c(1, 1, 1, NA, NA))

    # TODO: same issue as find_seqs_dem
    out <- find_seqs_aut(polyarchy, regime, tolerance = 3)
    expect_equal(out, c(1, 1, 1, 1, 1))

    polyarchy <- c(1, 2, 1, 2, 1, 1, 1)
    regime <- c(rep(1, 7))

    out <- find_seqs_aut(polyarchy, regime)
    expect_equal(out, c(NA, 1, 1, 2, 2, 2, 2))

    out <- find_seqs_aut(polyarchy, regime, year_turn = 2, cum_turn = 2, tolerance = 2)
    expect_equal(out, c(NA, 1, 1, 1, 1, 1, 1))
})

test_that("Handles NA", {
    polyarchy <- c(NA, 2, 1, NA)
    regime <- c(NA, 1, 1, NA)

    out <- find_seqs_aut(polyarchy, regime)
    expect_equal(out, c(NA, 1, 1, NA))

    polyarchy <- c(NA, 2, 1, NA, 0, NA, 3, 1)
    regime <- c(NA, 1, 1, NA, 1, NA, 1, 1)

    out <- find_seqs_aut(polyarchy, regime)
    expect_equal(out, c(NA, 1, 1, NA, NA, NA, 2, 2))
})

test_that("Decrease in v2x_regime", {
    polyarchy <- c(3, 2, 1)
    regime <- c(2, 1, 1)

    out <- find_seqs_aut(polyarchy, regime)
    expect_equal(out, c(1, 1, 1))

    polyarchy <- c(3, 2, 1, 1, 1)
    regime <- c(1, 3, 1, 1, 1)

    # TODO: same issue as find_seqs_dem
    out <- find_seqs_aut(polyarchy, regime)
    expect_equal(out, c(1, 2, 2, 2, 2))

    polyarchy <- c(3, 2, 1, 1, 1, 1)
    regime <- c(1, 1, 1, 1, 3, 1)

    out <- find_seqs_aut(polyarchy, regime)
    expect_equal(out, c(1, 1, 1, 1, NA, NA))
})
