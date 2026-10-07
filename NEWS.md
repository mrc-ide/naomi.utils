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
* Exclude `tests/test-shipp.R` and `tests/test-helpers.R` from the build: they
  were copied from the unmerged naomi SHIPP PR and rely on hintr fixtures that
  don't exist in this package. To be rewritten as `tests/testthat` tests.

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
