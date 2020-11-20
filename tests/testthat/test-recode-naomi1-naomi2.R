test_that("recode_age_group() works", {

  expect_equal(recode_naomi1_age_group(c("15-19", "15+", "00+")),
               c("Y015_019", "Y015_999", "Y000_999"))

  expect_error(recode_naomi1_age_group(c("15-19", "0+", "jibberish")),
               "Invalid age_groups found: 0\\+, jibberish")
})
