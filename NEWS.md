# naomi.utils 0.0.9

* Add function `create_hivtesting_dhs()` to extract individual HIV  testing
   history variables from DHS.
* Calculate male weighting factor to reweight individual weight to represent
  adult population. Calculation follows description by Tom Pullum on DHS user
  forum: https://userforum.dhsprogram.com/index.php?t=msg&th=6387&goto=13190&#msg_13190.
  

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
