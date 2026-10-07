#' Extract sexual behaviour categorical variables from MICS surveys.
#'
#' @param ind Individuals dataset: the MICS women's (`wm`) or men's (`mn`) file.
#' @param survey_id The survey name.
#' @param gender `"female"` for the women's file; anything else is treated as
#'   the men's file.
#' @return Sexual behaviour categorical variables
#' @export
extract_sexbehav_mics <- function(ind, survey_id, gender) {
  if(gender == "female") {
    sb_vars <- c(
      "sb1", # Age at first sexual intercourse - if 0, no sexual debut
      # 95 if first time is when started living with first husband/partner
      # lots of 99s - missing
      "sb2u", # Units of time since last sexual intercourse
      # 1 = days
      # 2 = weeks
      # 3 = months
      # 4 = years
      # 9 = missing
      # NA = no sexual debut
      "sb2n", # Number of time since last sexual intercourse
      "sb4", # Most recent partner type
      # 1 = husband
      # 2 = cohabiting partner
      # 3 = boyfriend
      # 4 = casual acquaintance
      # 5 = client/sex worker
      # 6 = other
      # 9 = missing
      "sb7", # any other partners in past 12 months?
      # 1 = Yes
      # 2 = No
      # 9 = missing
      "sb9" # type of second most recent partner
      # coding same as SB4
      # "MA1" # currently living with partner - doesn't actually correct data we need
      # 1 = Yes, currently married
      # 2 = Yes, living with a partner
      # 3 = No, not in union
      # 9 = missing
    )

    dat <- ind %>%
      dplyr::mutate(
        survey_id = survey_id,
        individual_id = paste0(wm1,"_",wm2,"_",wm3)
      ) %>%
      dplyr::select(survey_id, individual_id, tidyselect::any_of(sb_vars))

    # Fixes issue with e.g. some surveys not having paid sex questions
    dat[setdiff(sb_vars, names(dat))] <- NA

    dat %>%
      dplyr::mutate(
        # Reports sexual activity in the last 12 months
        sex12m = dplyr::case_when(
          sb4 %in% 1:6 ~ TRUE,
          sb2u==1 ~ TRUE, # Assuming that if you gave answer of days/weeks/months but
          sb2u==2 ~ TRUE, # didn't know the number (i.e. SB2N==99) that you had sex in
          sb2u==3 ~ TRUE, # the prior year
          TRUE ~ FALSE
        ),
        # Does not report sexual activity in the last 12 months
        nosex12m = dplyr::case_when(
          sex12m == TRUE ~ FALSE,
          sex12m == FALSE ~ TRUE,
          is.na(sex12m) ~ NA
        ),
        # Reports sexual activity with exactly one cohabiting partner in the past 12 months
        sexcohab = dplyr::case_when(
          sex12m == FALSE ~ FALSE,
          sb7==2 & (sb4==1 | sb4==2) ~ TRUE,
          sb7==9 & is.na(sb9) & (sb4==1 | sb4==2) ~ TRUE, # assuming that if you were missing
          # the yes/no other partners in past 12 months and also missing the type of
          # second partner, you only had one partner
          TRUE ~ FALSE
        ),
        # NO DIFFERENTIATION HERE - MICS DOESN'T DISTINGUISH MARITAL NON-COHABITING PARTNERS
        # Reports sexual activity with exactly one cohabiting partner or exactly one married partner who is living away
        # Either the spouse lives in, in which case they are cohabiting, or they live away, in which case they are also
        # covered here. So there is no need to check with the partlivew variable where the spouse is living.
        sexcohabspouse = dplyr::case_when(
          sex12m == FALSE ~ FALSE,
          sb7==2 & (sb4==1 | sb4==2) ~ TRUE,
          sb7==9 & is.na(sb9) & (sb4==1 | sb4==2) ~ TRUE, # assuming that if you were missing
          # the yes/no other partners in past 12 months and also missing the type of
          # second partner, you only had one partner
          TRUE ~ FALSE
        ),
        # Reports sexual activity with greater than one partner or any non-cohabiting partner
        sexnonreg = dplyr::case_when(
          nosex12m == TRUE ~ FALSE,
          sexcohab == TRUE ~ FALSE,
          !sb4 %in% c(1,2) ~ TRUE,
          sb7 == 1 | !is.na(sb9) ~ TRUE,
          TRUE ~ FALSE
        ),
        # NO DIFFERENTIATION HERE - MICS DOESN'T DISTINGUISH MARITAL NON-COHABITING PARTNERS
        # Reports sexual activity with greater than one partner or any non-marital non-cohabiting partner
        sexnonregspouse = dplyr::case_when(
          nosex12m == TRUE ~ FALSE,
          sexcohab == TRUE ~ FALSE,
          !sb4 %in% c(1,2) ~ TRUE,
          sb7 == 1 | !is.na(sb9) ~ TRUE,
          TRUE ~ FALSE
        ),
        # Reports having exchanged gifts, cash, or anything else for sex in the past 12 months
        sexpaid12m = dplyr::case_when(
          nosex12m == TRUE ~ FALSE,
          (sb4 == 5) | (sb9 == 5) ~ TRUE,
          TRUE ~ FALSE
        ),
        # Indicator for including any non-missing observations for selling sex (i.e. whether it was in the questionnaire)
        giftsvar = FALSE,
        # Either sexnonreg or sexpaid12m
        sexnonregplus = dplyr::case_when(
          sexnonreg == TRUE ~ TRUE,
          sexpaid12m == TRUE ~ TRUE,
          TRUE ~ FALSE
        ),
        # Either sexnonregspouse or sexpaid12m
        sexnonregspouseplus = dplyr::case_when(
          sexnonregspouse == TRUE ~ TRUE,
          sexpaid12m == TRUE ~ TRUE,
          TRUE ~ FALSE
        ),
        # Just want the highest risk category that an individual belongs to
        nosex12m = ifelse(sexcohab | sexnonreg | sexpaid12m, FALSE, nosex12m),
        sexcohab = ifelse(sexnonreg | sexpaid12m, FALSE, sexcohab),
        sexcohabspouse = ifelse(sexnonregspouse | sexpaid12m, FALSE, sexcohabspouse),
        sexnonreg = ifelse(sexpaid12m, FALSE, sexnonreg),
        sexnonregspouse = ifelse(sexpaid12m, FALSE, sexnonregspouse),
        # Turn everything from TRUE / FALSE coding to 1 / 0
        dplyr::across(sex12m:sexnonregspouseplus, ~ as.numeric(.x))
      ) %>%
      dplyr::select(-tidyselect::all_of(sb_vars))
  } else {
    sb_vars <- c(
      "msb1", # Age at first sexual intercourse - if 0, no sexual debut
      # 95 if first time is when started living with first husband/partner
      # lots of 99s - missing
      "msb2u", # Units of time since last sexual intercourse
      # 1 = days
      # 2 = weeks
      # 3 = months
      # 4 = years
      # 9 = missing
      # NA = no sexual debut
      "msb2n", # Number of time since last sexual intercourse
      "msb4", # Most recent partner type
      # 1 = husband
      # 2 = cohabiting partner
      # 3 = boyfriend
      # 4 = casual acquaintance
      # 5 = client/sex worker
      # 6 = other
      # 9 = missing
      "msb7", # any other partners in past 12 months?
      # 1 = Yes
      # 2 = No
      # 9 = missing
      "msb9" # type of second most recent partner
      # coding same as SB4
      # "MMA1" # currently living with partner - doesn't actually correct data we need
      # 1 = Yes, currently married
      # 2 = Yes, living with a partner
      # 3 = No, not in union
      # 9 = missing
    )

    dat <- ind %>%
      dplyr::mutate(
        survey_id = survey_id,
        individual_id = paste0(mwm1,"_",mwm2,"_",mwm3)
      ) %>%
      dplyr::select(survey_id, individual_id, tidyselect::any_of(sb_vars))

    # Fixes issue with e.g. some surveys not having paid sex questions
    dat[setdiff(sb_vars, names(dat))] <- NA

    dat %>%
      dplyr::mutate(
        # Reports sexual activity in the last 12 months
        sex12m = dplyr::case_when(
          msb4 %in% 1:6 ~ TRUE,
          msb2u==1 ~ TRUE, # Assuming that if you gave answer of days/weeks/months but
          msb2u==2 ~ TRUE, # didn't know the number (i.e. SB2N==99) that you had sex in
          msb2u==3 ~ TRUE, # the prior year
          TRUE ~ FALSE
        ),
        # Does not report sexual activity in the last 12 months
        nosex12m = dplyr::case_when(
          sex12m == TRUE ~ FALSE,
          sex12m == FALSE ~ TRUE,
          is.na(sex12m) ~ NA
        ),
        # Reports sexual activity with exactly one cohabiting partner in the past 12 months
        sexcohab = dplyr::case_when(
          sex12m == FALSE ~ FALSE,
          msb7==2 & (msb4==1 | msb4==2) ~ TRUE,
          msb7==9 & is.na(msb9) & (msb4==1 | msb4==2) ~ TRUE, # assuming that if you were missing
          # the yes/no other partners in past 12 months and also missing the type of
          # second partner, you only had one partner
          TRUE ~ FALSE
        ),
        # NO DIFFERENTIATION HERE - MICS DOESN'T DISTINGUISH MARITAL NON-COHABITING PARTNERS
        # Reports sexual activity with exactly one cohabiting partner or exactly one married partner who is living away
        # Either the spouse lives in, in which case they are cohabiting, or they live away, in which case they are also
        # covered here. So there is no need to check with the partlivew variable where the spouse is living.
        sexcohabspouse = dplyr::case_when(
          sex12m == FALSE ~ FALSE,
          msb7==2 & (msb4==1 | msb4==2) ~ TRUE,
          msb7==9 & is.na(msb9) & (msb4==1 | msb4==2) ~ TRUE, # assuming that if you were missing
          # the yes/no other partners in past 12 months and also missing the type of
          # second partner, you only had one partner
          TRUE ~ FALSE
        ),
        # Reports sexual activity with greater than one partner or any non-cohabiting partner
        sexnonreg = dplyr::case_when(
          nosex12m == TRUE ~ FALSE,
          sexcohab == TRUE ~ FALSE,
          !msb4 %in% c(1,2) ~ TRUE,
          msb7 == 1 | !is.na(msb9) ~ TRUE,
          TRUE ~ FALSE
        ),
        # NO DIFFERENTIATION HERE - MICS DOESN'T DISTINGUISH MARITAL NON-COHABITING PARTNERS
        # Reports sexual activity with greater than one partner or any non-marital non-cohabiting partner
        sexnonregspouse = dplyr::case_when(
          nosex12m == TRUE ~ FALSE,
          sexcohab == TRUE ~ FALSE,
          !msb4 %in% c(1,2) ~ TRUE,
          msb7 == 1 | !is.na(msb9) ~ TRUE,
          TRUE ~ FALSE
        ),
        # Reports having exchanged gifts, cash, or anything else for sex in the past 12 months
        sexpaid12m = dplyr::case_when(
          nosex12m == TRUE ~ FALSE,
          (msb4 == 5) | (msb9 == 5) ~ TRUE,
          TRUE ~ FALSE
        ),
        # Indicator for including any non-missing observations for selling sex (i.e. whether it was in the questionnaire)
        giftsvar = FALSE,
        # Either sexnonreg or sexpaid12m
        sexnonregplus = dplyr::case_when(
          sexnonreg == TRUE ~ TRUE,
          sexpaid12m == TRUE ~ TRUE,
          TRUE ~ FALSE
        ),
        # Either sexnonregspouse or sexpaid12m
        sexnonregspouseplus = dplyr::case_when(
          sexnonregspouse == TRUE ~ TRUE,
          sexpaid12m == TRUE ~ TRUE,
          TRUE ~ FALSE
        ),
        # Just want the highest risk category that an individual belongs to
        nosex12m = ifelse(sexcohab | sexnonreg | sexpaid12m, FALSE, nosex12m),
        sexcohab = ifelse(sexnonreg | sexpaid12m, FALSE, sexcohab),
        sexcohabspouse = ifelse(sexnonregspouse | sexpaid12m, FALSE, sexcohabspouse),
        sexnonreg = ifelse(sexpaid12m, FALSE, sexnonreg),
        sexnonregspouse = ifelse(sexpaid12m, FALSE, sexnonregspouse),
        # Turn everything from TRUE / FALSE coding to 1 / 0
        dplyr::across(sex12m:sexnonregspouseplus, ~ as.numeric(.x))
      ) %>%
      dplyr::select(-tidyselect::all_of(sb_vars))
  }

}
