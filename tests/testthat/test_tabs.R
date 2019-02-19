### Testing of rtabsplus - tab functions
# Jesse Zlotoff
# 2/19/19

context("tab functions")
library(rtabsplus)
require(tibble)

data(mtcars)

test_that("output is a tibble", {
    expect_true(is_tibble(stab(mtcars, "gear")))
    expect_true(is_tibble(stab(mtcars, "gear", "carb")))
})

test_that("inputs are converted to factors", {
    expect_error(stab(mtcars, "gear", weight_var="wt", to_factor=FALSE))
    w <- stab(mtcars, "gear", weight_var="wt")
    expect_true(length(w)==2 & nrow(w)==3)
})

test_that("invalid inputs are handled correctly", {
    temp <- mtcars
    temp$all_na <- NA
    expect_error(stab(temp, "all_na"))
    expect_error(stab(temp, "fake_column"), "fake_column not present in dataframe")
    temp$vv1 <- 1
    expect_error(stab(temp, "gear"), "vv1 already present in dataframe")
    expect_error(stab(temp$vv1, "gear"), "first argument is not a dataframe")
})
