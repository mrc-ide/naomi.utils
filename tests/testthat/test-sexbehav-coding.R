## One synthetic respondent per missing-value rule. Group "NA" = excluded from every risk group.

group_of <- function(d, spouse = FALSE) {
  cohab <- if (spouse) d$sexcohabspouse else d$sexcohab
  nonreg <- if (spouse) d$sexnonregspouse else d$sexnonreg
  dplyr::case_when(
    is.na(d$nosex12m) ~ "NA",
    d$sexpaid12m == 1 ~ "paid",
    nonreg == 1 ~ "nonreg",
    cohab == 1 ~ "cohab",
    d$nosex12m == 1 ~ "nosex"
  )
}

dhs_row <- function(...) {
  d <- data.frame(v766b = NA, v767a = NA, v767b = NA, v767c = NA, v504 = NA,
                  v501 = NA, v527 = NA, v791a = NA, v793 = NA)
  args <- list(...)
  d[names(args)] <- args
  d
}

test_that("DHS complete cases are coded as before", {
  d <- rbind(dhs_row(v766b = 0, v527 = 401),
             dhs_row(v766b = 1, v767a = 1, v504 = 1, v501 = 1, v527 = 203),
             dhs_row(v766b = 1, v767a = 7, v501 = 2, v527 = 203),
             dhs_row(v766b = 1, v767a = 2, v527 = 203),
             dhs_row(v766b = 2, v767a = 1, v767b = 3, v504 = 1, v501 = 1, v527 = 101),
             dhs_row(v766b = 1, v767a = 6, v527 = 101),
             dhs_row(v766b = 1, v767a = 1, v504 = 2, v501 = 1, v527 = 302))
  out <- code_sexbehav_dhs(d)
  expect_equal(group_of(out), c("nosex", "cohab", "cohab", "nonreg", "nonreg", "paid", "nonreg"))
  ## Spouse definition: married partner living elsewhere is cohabiting
  expect_equal(group_of(out, spouse = TRUE)[7], "cohab")
})

test_that("DHS 1 partner, type missing: cohabiting only if living with partner", {
  d <- rbind(dhs_row(v766b = 1, v501 = 2, v527 = 203),
             dhs_row(v766b = 1, v501 = 1, v504 = 1, v527 = 203),
             dhs_row(v766b = 1, v767a = 99, v501 = 0, v527 = 203))
  expect_equal(group_of(code_sexbehav_dhs(d)), c("cohab", "cohab", "NA"))
})

test_that("DHS partner count missing, spouse/live-in: cohabiting or non-regular", {
  d <- rbind(dhs_row(v766b = 99, v767a = 1, v504 = 1, v501 = 1),
             dhs_row(v766b = 99, v767a = 7, v501 = 2),
             dhs_row(v766b = 99, v767a = 1, v767b = 3, v504 = 1, v501 = 1))
  expect_equal(group_of(code_sexbehav_dhs(d)), c("cohab", "cohab", "nonreg"))
})

test_that("DHS count and type missing, partner elsewhere: non-regular if sex in past year", {
  d <- rbind(dhs_row(v504 = 2, v501 = 2, v527 = 305),
             dhs_row(v504 = 2, v501 = 2, v527 = 402),
             dhs_row(v504 = 2, v501 = 2))
  expect_equal(group_of(code_sexbehav_dhs(d)), c("nonreg", "nosex", "NA"))
})

test_that("DHS respondents not asked the module are excluded", {
  out <- code_sexbehav_dhs(dhs_row(v504 = 1, v501 = 1))
  expect_equal(group_of(out), "NA")
  expect_true(is.na(out$sex12m))
})

test_that("DHS don't know number of partners is non-regular", {
  expect_equal(group_of(code_sexbehav_dhs(dhs_row(v766b = 98, v767a = 1, v527 = 203))), "nonreg")
})

phia_row <- function(...) {
  d <- data.frame(sexever = 1, sex12months = 1, part12monum = NA, part12modkr = NA,
                  firstsxagedk = NA, partrelation1 = NA, partrelation2 = NA,
                  partrelation3 = NA, partlivew1 = NA, partlivew2 = NA,
                  partlivew3 = NA, sellsx12mo = NA, buysx12mo = NA)
  args <- list(...)
  d[names(args)] <- args
  d
}

test_that("PHIA complete cases", {
  d <- rbind(phia_row(sexever = 2, sex12months = 99),
             phia_row(sex12months = 2, part12monum = 0),
             phia_row(part12monum = 1, partrelation1 = 2),
             phia_row(part12monum = 1, partrelation1 = 1, partlivew1 = 1),
             phia_row(part12monum = 1, partrelation1 = 1, partlivew1 = 2),
             phia_row(part12monum = 1, partrelation1 = 9),
             phia_row(part12monum = 3, partrelation1 = 1, partlivew1 = 1),
             phia_row(part12monum = 1, partrelation1 = 5, sellsx12mo = 1))
  out <- code_sexbehav_phia(d)
  expect_equal(group_of(out), c("nosex", "nosex", "cohab", "cohab", "nonreg", "nonreg", "nonreg", "paid"))
  expect_equal(group_of(out, spouse = TRUE)[5], "cohab")
})

test_that("PHIA sex flag missing is excluded unless 0 partners, never had sex or don't know", {
  d <- rbind(phia_row(sex12months = 99),
             phia_row(sex12months = 99, part12monum = 0),
             phia_row(sexever = 2, sex12months = 99),
             phia_row(sex12months = 99, part12modkr = -8),
             phia_row(sex12months = 99, part12monum = -8),
             phia_row(sex12months = 99, part12monum = -9))
  expect_equal(group_of(code_sexbehav_phia(d)), c("NA", "nosex", "nosex", "nonreg", "nonreg", "NA"))
})

test_that("PHIA had sex, partner count blank/don't know/out of range: non-regular", {
  d <- rbind(phia_row(),
             phia_row(part12modkr = -8),
             phia_row(part12monum = -8),
             phia_row(part12monum = -7))
  expect_equal(group_of(code_sexbehav_phia(d)), rep("nonreg", 4))
})

test_that("PHIA refused count, unknown debut + blank count, unknown partner type: excluded", {
  d <- rbind(phia_row(part12monum = -9),
             phia_row(part12modkr = -9),
             phia_row(firstsxagedk = -8),
             phia_row(part12monum = 1, partrelation1 = -8),
             phia_row(part12monum = 1, partrelation1 = -9))
  expect_equal(group_of(code_sexbehav_phia(d)), rep("NA", 5))
})
