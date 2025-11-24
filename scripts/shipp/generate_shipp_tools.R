# Generate SHIPP for all countries with Naomi results
# Date: September-25
# Author: Rachel Esra
# ------------------------------------------------------------------------------
library(dplyr)
library(naomi.resources)
library(naomi.utils)


# Set path to folder with approved Naomi files
dir <- "C:/Users/Test/Avenir Health Dropbox/Avenir Shared Drive/DataSets/UNAIDS/2025 Estimates"

naomi.dir <- file.path(dir, "Naomi")

naomi.files <- list.files(naomi.dir, full.names = TRUE)
naomi.files <- naomi.files[grepl(".zip", naomi.files)]
naomi.iso3 <- toupper(substr(basename(naomi.files), 1, 3))

exclude <- c("ERI", "GNB", "HTI")
naomi_file_map <- data.frame("iso3" = naomi.iso3, "naomi_file"= naomi.files) |>
  filter(!(iso3 %in% exclude))

shipp_dir <- "C:/Users/Test/Avenir Health Dropbox/Avenir Shared Drive/DataSets/UNAIDS/2025 Estimates/SHIPP"


# This function will generate SHIPP files based on all Naomi outputs in `naomi_file_map`
# and save to `shipp_dir`. It will also generate `results` that will save out any errors
# thrown when generating the tools.

shipp_ahoy <- function(iso){
  withCallingHandlers(
    {
      print(iso)
      naomi_file <- naomi_file_map[naomi_file_map$iso3 == iso, ]$naomi_file
      generate_shipp_tool(naomi_file, pjnz = NULL,
                          path = file.path(shipp_dir, paste0(iso, "_2025_shipp.xlsx")))
      
      data.frame(iso3 = iso, success = TRUE, message = "success", stringsAsFactors = FALSE)
    },
    error = function(e) {
      data.frame(iso3 = iso, success = FALSE, message = as.character(e$message), stringsAsFactors = FALSE)
    }
  )
}

iso_to_run <- naomi_file_map$iso3
iso_to_run <- c("GIN")

results <- do.call(rbind, lapply(iso_to_run, shipp_ahoy))

# To debug -> Run through SHIPP code in function shipp_generate_risk_populations ()
# line by line for a single county line by line, example for MWI:

#  Set options
iso = "ETH"
naomi_output = naomi_file_map[naomi_file_map$iso3 == iso, ]$naomi_file
pjnz = NULL
consensus_est = "goals"
survey_year = 2018

outputs <- naomi::read_output_package(naomi_output)
options <- outputs$fit$model_options
iso <- options$area_scope

if(iso %in% c("ZAF", "MWI")){quarter = options$calendar_quarter_t3}else{quarter = options$calendar_quarter_t2}

# Check for concordence between area_ids in shipp resources from `naomi.resources`
#  and Naomi estimates
naomi.utils:::assert_shipp_resource_hierarchy(outputs, options)

# Format naomi output
naomi <- naomi.utils:::shipp_format_naomi(outputs, options, quarter)

# Naomi population
naomi_pop <- naomi$naomi_long %>%
  dplyr::filter(indicator == "population") %>%
  dplyr::select(area_id, area_level,sex, age_group, area_level,
                spectrum_region_code, population = mean)
naomi_pop$iso3 <- iso

# Consensus estimates
goals <- naomi.resources::load_shipp_exdata("goals", "SSA") |> dplyr::filter(iso3 == iso)

if(consensus_est == "goals"){kp_wb = NULL}
if(!is.null(pjnz) && consensus_est == "kp_wb"){
  kp_wb <- naomi.utils:::extract_kp_workbook(pjnz)}


# Disaggregate subnational KP PSEs from Stevens et al. analysis to 5-year bands
fsw_est <- shipp_disaggregate_fsw(outputs, iso, naomi_pop,
                                  consensus_est, goals, kp_wb)
pwid_est <- shipp_disaggregate_pwid(outputs, iso, naomi_pop,
                                    consensus_est, goals, kp_wb)
msm_est <- shipp_disaggregate_msm(outputs, iso, naomi_pop,
                                  consensus_est, goals, kp_wb)

# Adjust SAE model output with KP proportions
female_srb <- shipp_adjust_sexbehav_fsw(outputs, options, fsw_est)
male_srb <- shipp_adjust_sexbehav_msm_pwid(outputs, options, msm_est, pwid_est)

# Calculate risk group prevalence
female_logit_prevalence <- shipp_calculate_prevalence_female(naomi$naomi_long,
                                                             options,
                                                             fsw_est,
                                                             female_srb,
                                                             survey_year,
                                                             quarter)

male_logit_prevalence <- shipp_calculate_prevalence_male(naomi$naomi_long,
                                                         outputs$meta_area,
                                                         options,
                                                         msm_est,
                                                         male_srb,
                                                         survey_year,
                                                         quarter)

# Calculate risk group incidence
female_incidence <- shipp_calculate_incidence_female(naomi$naomi_long,
                                                     options, iso,
                                                     female_srb,
                                                     female_logit_prevalence,
                                                     survey_year,
                                                     consensus_est, goals, kp_wb)


male_incidence <- shipp_calculate_incidence_male(naomi$naomi_long,
                                                 options, iso,
                                                 male_srb,
                                                 male_logit_prevalence,
                                                 survey_year,
                                                 consensus_est, goals, kp_wb)

meta <- data.frame(kp = c("FSW", "MSM", "PWID"),
                   consensus_estimate = c(unique(fsw_est$consensus_estimate),
                                          unique(msm_est$consensus_estimate),
                                          unique(pwid_est$consensus_estimate)))


v <- list(female_incidence = female_incidence,
          male_incidence = male_incidence,
          naomi_output = naomi$naomi_wide,
          meta_consensus = meta)

v <- shipp_reformat_output(v)

v

