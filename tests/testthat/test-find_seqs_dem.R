test_that("Finding democratization sequences", {
    out <- find_seqs_dem(1, 1)
    expect_equal(out, NA_real_)
})
