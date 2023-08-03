#! /usr/bin/env Rscript
"Delete estimates 2024 ADR datasets. We need to do this because we want to
  retrun the data copy script copying over data from a release
  instead of the current version of the data.

Usage:
    remove_estimates_datasets.R  [--dry-run] [--site=<site>] --key=<key>
    remove_estimates_datasets.R -h | --help

Options
    --dry-run     Run in dry-run mode.
    --site=<site> ADR site to use dev or prod [default: dev].
    --key=<key>   ADR auth key.
" -> doc

args <- docopt::docopt(doc)
dry_run <- args[["dry_run"]]
key <- args[["key"]]
site <- args[["site"]]
if (site == "prod") {
  url <- "https://adr.unaids.org/"
} else if (site == "dev") {
  url <- "https://dev.adr.fjelltopp.org/"
} else {
  stop("Site must be prod or dev")
}
message("Deleting datasets from ", url)

ckanr::ckanr_setup(url = url, key = key)
type <- "country-estimates-24"

datasets <- ckanr::package_search(q = sprintf("type:%s", type),
                                  rows = 1000)

for (dataset in datasets$results) {
  message("Removing dataset ", dataset$name)
  attempt <- 0
  complete <- FALSE
  while (!complete && attempt < 5) {
    attempt <- attempt + 1
    tryCatch({
      if (!dry_run) {
        ckanr::package_delete(dataset$id)
      }
      message("Succesfully removed ", dataset$name)
      complete <- TRUE
    },
    error = function(e) {
      if (grepl("Authorization Error", e$message)) {
        message(e$message)
        attempt <<- 10 ## Exit immediately
      } else {
        ## If we've failed to delete it might be because ADR is
        ## dealing with too many requests. Wait with incremental back off
        wait <- 30 * attempt
        message(sprintf(paste0("Failed to remove dataset %s: attempt %s. ",
                               "Trying again in %s seconds."),
                dataset$name, attempt, wait))
        Sys.sleep(wait)
      }
    })
  }
  if (!complete) {
    message(sprintf(
      "Failed to remove %s, either no permission or ADR overloaded with requests",
      dataset$name))
  }
}
