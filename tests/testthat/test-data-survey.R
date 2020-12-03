test_that("get_mid_calendar_quarter() returns correct value", {
  start <- c("2005-04-01", "2010-12-15", "2016-01-01")
  end <-c("2005-08-01", "2011-05-15", "2016-06-01")
  expect_equal(get_mid_calendar_quarter(start, end),
               c("CY2005Q2", "CY2011Q1", "CY2016Q1"))
})

test_that("get_mid_calendar_quarter() returns error if arguments not Date", {
  expect_error(get_mid_calendar_quarter("2016-01-01", NA),
               "!is.na\\(end_date\\) is not TRUE")
  expect_error(get_mid_calendar_quarter("2016-01-01", "jibberish"),
               "character string is not in a standard unambiguous format")
  expect_error(get_mid_calendar_quarter(NA, "2016-01-01"),
               "!is.na\\(start_date\\) is not TRUE")
  expect_error(get_mid_calendar_quarter("2016-01-01", "2015-12-01"),
               "start_date <= end_date is not TRUE")
})
