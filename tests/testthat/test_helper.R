context("Helper")
library(softdev)


test_that("Name correctly specified",{
  expect_match(make_filename("2015"), "accident_2015.csv.bz2")
  expect_that(make_filename("2015"), is_a("character"))
}
)

test_that("Checks for the correct ouput class",{
  expect_that(fars_read_years(c(2014, 2015))[[1]], is_a("tbl_df"))
  expect_that(fars_read_years(c(2014, 2015))[[2]], is_a("tbl_df"))
  expect_that(fars_read_years(c(2014, 2015)), is_a("list"))
})
