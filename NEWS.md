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
