# naomi.utils 0.0.20

Sexual risk behaviour survey extraction for the SHIPP small-area model,
upstreamed from `krisher1/naomi.utils@sexbehav-vars-adam` (Katie Risher,
Adam Howes):

* Add `create_sexbehav_dhs()`, `extract_sexbehav_phia()`,
  `extract_sexbehav_mics()` and `check_survey_sexbehav()`: derive individual
  sexual risk groups (`nosex12m`, `sexcohab`, `sexnonreg`, `sexpaid12m`, ...)
  from DHS, PHIA and MICS microdata.
* Add `calc_survey_sexbehav_indicators()`: survey-weighted proportion in each
  risk group, and HIV indicators within each risk group, by area/sex/age.
* `calc_survey_hiv_indicators()` signature and output are unchanged; it now
  shares its estimation code with `calc_survey_sexbehav_indicators()`. Also
  fixes an error when `age_group_include` is set.
* **Breaking:** `create_individual_hiv_dhs()` now errors if a survey has no HIV
  test (AR) dataset. Previously such surveys silently got no HIV data. New
  argument `hiv_testing` (logical, one per survey or one for all): `TRUE`
  requires the AR dataset, `FALSE` skips it and gives `NA` HIV fields. It
  defaults to the `hiv_testing` column of `surveys` if present, else `TRUE`.
  Callers with surveys that have no AR dataset (e.g. ML2001DHS, ZM2002DHS)
  must set their `hiv_testing` to `FALSE`.
* `create_surveys_dhs()` gains `sexbehav = FALSE` (and `sexbehav_*` selection
  arguments): when `TRUE`, also returns the sexual behaviour surveys, with
  logical `hiv_testing` and `sexbehav` columns, so one task can build survey
  inputs for both. Either list may be empty in this mode (e.g. NGA has no DHS
  HIV testing). The default output is unchanged.
* Tests now run in `R CMD check` (`tests/testthat.R` was misplaced); adds tests
  for the survey indicator functions and fixes two SHIPP workbook tests that
  passed the wrong input.

# naomi.utils 0.0.19

Updates to SHIPP workbook processing code for 2025 estimates update:
* Add fix to avoid new infections among KP exceeding total new infections in a given age/district
* Add fix to avoid PSE for KPs exceeding size of non-regular partners group
* Ensure Naomi T3 pulled in as "current estimates" for countries that used custom Naomi version with two household surveys surveys (MWI, ZAF)
* Year for sexual risk behaviour survey results now pulled in from "Model inputs" tab in the SHIPP workbook template. 
  Year set to year of most recent survey with sexual risk behaviour data and to 2018 for countries where
  most recent sexual risk behaviour survey is older that 2018.
* Add option to produce SHIPP without scaling to national envelope of KP PSE and new infections from Goals.
* Define SHIPP incidence category thresholds once (`shipp_incidence_breaks`:
  0.2 / 0.5 / 2 per 100 person-years) and use them everywhere. Fixes
  `incidence_cat` in the female/male incidence outputs, which still used the
  old 0.3 / 1 / 3 thresholds.
* Fix Naomi `Incicategory` recode in `shipp_format_naomi()` comparing incidence
  as character strings rather than numbers.

# naomi.utils 0.0.18

* Fix `R CMD check` errors and warnings on master
  * Declare packages used via `::` in `Imports` (`gridExtra`, `naomi`,
    `naomi.resources`, `openxlsx2`, `readr`, `stringr`, `tibble`, `withr`,
    `zip`) and test packages in `Suggests` (`mockery`, `openxlsx`)
  * Replace `:::` with `::` for exported `naomi.resources::load_shipp_exdata()`
    and drop `naomi.utils:::` on an internal call
  * Fix `write_sf_shp_zip()` example (`sf::read_sf`)
  * Regenerate stale documentation and document missing arguments
* `naomi_debug()` no longer uploads to Imperial SharePoint (`spud` is
  deprecated): it downloads the debug bundle to `<root>/<jobid>/`, with `root`
  defaulting to `NAOMI_DEBUG_ONEDRIVE`. The `dest_folder` argument is replaced
  by `root`.
* Remove `tests/test-shipp.R` and `tests/test-helpers.R`: they were copied from
  the superseded naomi SHIPP PR (mrc-ide/naomi#413) and relied on hintr
  fixtures that don't exist in this package. SHIPP tests to be rewritten in
  `tests/testthat`.

# naomi.utils 0.0.17

* Remove `spud` dependency as package is deprecated 
* Update debug functions in line with updated data structure in fit object 
downloaded from download_debug()
* Fix GE dataset parsing broken by R/sf upgrade
  * After upgrading to R 4.5.2, readRDS() now returns cached GE GPS files as sf 
    objects rather than plain data frames. This caused type.convert() to fail on 
    the retained sfc geometry column.
  * Fix adds st_drop_geometry() before as.data.frame() to remove the geometry 
    column before processing, with the sf object reconstructed from LONGNUM/LATNUM 
    as before.

# naomi.utils 0.0.16

* Patch logic error in `gather_areas()`.

# naomi.utils 0.0.15

* Remove traduire hooks from SHIPP errors.

# naomi.utils 0.0.14

* Add functions to generate SHIPP tool from Naomi output and Spectrum file.


# naomi.utils 0.0.13

* Copy `get_age_groups()` from naomi package. 
* Naomi [and therefore eppasm] are included as dependencies in other packages just to use this function. This causes slow loading and hits API data limit when installing packages on the orderly server.

# naomi.utils 0.0.12

* Add `clear_rdhs_cache` argument to `rdhs::download_dataset()` call

# naomi.utils 0.0.11

* Extract individual survey weights for all respondents and adjust male survey weights
  such that pooled male/female data are weighted representative of the full adult 
  population (https://userforum.dhsprogram.com/index.php?t=msg&th=6387&goto=13190&#msg_13190).
* Patch [`create_surveys_dhs()`] to enable extraction of surveys from more than one country in
  one function call (change `== iso3` to `%in% iso3`).


# naomi.utils 0.0.10

* Patch `assert_pop_data_check()` and `assert_area_id_check()` to specify `dplyr::setdiff()` (instead of base `generics::setdiff()`.

# naomi.utils 0.0.9

* Patch `create_surveys_dhs()` to ensure that numeric is always returned for `MinAgeMen`, `MaxAgeMen`, `MinAgeWomen`, `MaxAgeWomen` (issue #13, @athowes).

# naomi.utils 0.0.8

* Use download.file(..., mode = "wb") in WorldPop extraction.

# naomi.utils 0.0.8

* Handle countries where no surveys have MR datasets (e.g. Congo).

# naomi.utils 0.0.7

* Handle countries where no surveys have survey cluster datasets (e.g The Gambia).

# naomi.utils 0.0.6

* Patch: add area_name column to population dataset extract.
* Require column `area_name` in `validate_naomi_population()`.


# naomi.utils 0.0.5

* Added a `NEWS.md` file to track changes to the package.

* Add functions `naomi_extract_worldpop()` and `naomi_extract_gpw()`
  to create Naomi population datasets from WorldPop and GPW v4.11 
  population raster datasets.
