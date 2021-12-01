#! /usr/bin/env Rscript
"Copy ADR datasets from a source package to a destination package. The `src`
  and `dest` can be changed or the resources which are copied over can be
  changed too. A dataset will not be created if it already exists in the
  `dest` package type. Similary a dataset won't be created if there isn't a
  unique package for that country and type (as we don't know which to
  copy from). Before running delete any 2022 packages you want to replace.

Usage:
    copy_adr_data.R  [--dry-run] [--site=<site>] --key=<key>
    copy_adr_data.R -h | --help

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
  url <-  "https://adr.unaids.org/"
} else if (site == "dev") {
  url <- "https://dev.adr.fjelltopp.org/"
} else {
  stop("Site must be prod or dev")
}
message("Running migration on site ", url)

ckanr::ckanr_setup(url = url, key = key)
src <- "inputs-unaids-estimates"
dest <- "country-estimates-22"
## The display name of the package being created, ADR requies this
dest_name <- "HIV Estimates 2022"
resources <- c("inputs-unaids-geographic", "inputs-unaids-anc",
               "inputs-unaids-art", "inputs-unaids-survey",
               "inputs-unaids-population", "inputs-unaids-spectrum-file")

## No built in way to send custom commands to CKAN so do a gross hack
ckan_create_release <- function(id) {
  res <- ckanr:::ckan_POST(url = ckanr::get_default_url(),
                    method = "dataset_version_create",
                    body = jsonlite::toJSON(list(
                      dataset_id = id,
                      name = "2021 data transfer",
                      notes = paste0("Data automatically transferred from ",
                                     "2021 dataset. Added blank columns to ART",
                                     " data for new indicators.")
                    ), auto_unbox = TRUE),
                    key = ckanr::get_default_key(),
                    encode = "json",
                    headers = ckanr:::ctj())
  ckanr:::jsl(res)
}

packages_src <- ckanr::package_search(q = sprintf("type:%s", src),
                                       rows = 1000)
packages_dest <- ckanr::package_search(q = sprintf("type:%s", dest),
                                       rows = 1000)

## Don't migrate data for Fjeltopp, Naomi development team, Imperial, unaids
## or avenir orgs as these are usually duplicates of country data
orgs_to_ignore <- c("fjelltopp", "avenir-health", "imperial-college-london",
                    "naomi-development-team", "unaids")
orgs <- vapply(packages_src$results,
               function(result) {
                 result[["organization"]][["name"]]
               }, character(1))
packages_src$results <- packages_src$results[!(orgs %in% orgs_to_ignore)]

## Only create new packages for countries which don't already exist
countries_src <- vapply(packages_src$results, "[[", character(1),
                        "geo-location")
countries_dest <- vapply(packages_dest$results, "[[", character(1),
                         "geo-location")
countries_keep <- !(countries_src %in% countries_dest)
countries_src <- countries_src[countries_keep]

## If more than 1 dataset for a country - don't do anything report out
multiple <- names(table(countries_src))[table(countries_src) > 1]
for (country in multiple) {
  message(sprintf("%s has more than 1 %s dataset, don't know how to migrate",
                  country, src))
}
countries_to_copy <- countries_src[!(countries_src %in% multiple)]
countries_to_copy <- c("Cameroon", "Mali")

packages_keep <- vapply(packages_src$results, function(package) {
  package[["geo-location"]] %in% countries_to_copy
}, logical(1))
packages_copy <- packages_src$results[packages_keep]

## For each country, download 2021 resources
## Upload as 2022 dataset in imperial-college-london org
## then transfer from there
t <- tempfile()
dir.create(t)
for (package in packages_copy) {
  country_dir <- file.path(t, package[["geo-location"]])
  dir.create(country_dir)
  message("Creating package for ", package[["geo-location"]])
  if (!dry_run) {
    tryCatch({
      new_package <- NULL
      new_package <- ckanr::package_create(
        type = dest, owner_org = package[["organization"]][["name"]],
        extras = list("geo-location" = package[["geo-location"]],
                      type_name = dest_name,
                      maintainer_email = "naomi-support@unaids.org",
                      year = "2022"),
        maintainer = "Naomi team",
        notes = "A record of the input data and final HIV estimates.")
    },
    error = function(e) {
      message("Failed to create package for ", package[["geo-location"]])
    })
    if (is.null(new_package)) {
      next
    }
  }
  resource_types <- vapply(package$resources, function(resource) {
    type <- resource[["resource_type"]]
    if (is.null(type)) {
      type <- "none"
    }
    type
  }, character(1))
  create_release <- TRUE
  for (resource in intersect(resources, resource_types)) {
    details <- package$resources[resource_types == resource]
    if (length(details) > 1) {
      message(sprintf("package %s has more than 1 resource of type %s - skipping",
                   package[["name"]], resource))
      next
    }
    details <- details[[1]]
    ## basename of the URL contains the name of the file when first uploaded
    path <- file.path(country_dir, basename(details[["url"]]))
    tryCatch({
      ckanr::ckan_fetch(details$url, store = "disk", path = path)
    },
    error = function(e) {
      message(sprintf("Failed to download file %s for country %s - skipping",
                      resource, package[["geo-location"]]))
    })
    if (!file.exists(path)) {
      next
    }
    message(sprintf("Uploading %s file for country %s",
                    resource, package[["geo-location"]]))
    if (!dry_run) {
      tryCatch({
        ## We want to add new columns "art_new", "vl_tested_12mos" and
        ## "vl_suppressed_12mos" to ART data so it passes validation
        ## and remove year column as we no longer use this
        if (resource == "inputs-unaids-art") {
          x <- read.csv(path)
          if (!("art_new" %in% colnames(x))) {
            x$art_new <- NA_integer_
          }
          if (!("vl_tested_12mos" %in% colnames(x))) {
            x$vl_tested_12mos <- NA_integer_
          }
          if (!("vl_suppressed_12mos" %in% colnames(x))) {
            x$vl_suppressed_12mos <- NA_integer_
          }
          x$year <- NULL
          path <- file.path(country_dir, paste0("updated", basename(path)))
          write.csv(x, path, quote = FALSE, row.names = FALSE,
                    na = "")
        }
        new_resource <- ckanr::resource_create(
          new_package[["id"]],
          name = details[["name"]],
          upload = path,
          extras = list(
            restricted = paste0('{"allowed_organizations": "unaids", ',
                                '"allowed_users": "", "level": "restricted"}'),
            resource_type = resource))
      },
      error = function(e) {
        message(sprintf("Failed to upload file %s for country %s - skipping",
                        resource, package[["geo-location"]]))
        create_release <<- FALSE
      })
    }
  }
  if (!dry_run && create_release) {
    message(sprintf("Creating release for %s", package[["geo-location"]]))
    tryCatch({
      ckan_create_release(new_package[["id"]])
    },
    error = function(e) {
      message(sprintf("Failed to create release for country %s",
                      package[["geo-location"]]))
    })
  }
  if (dry_run) {
    package_url <- "dry run - not created"
  } else {
    package_url <- paste0(url, dest, "/", new_package$name)
  }
  message(sprintf("Copy complete for %s see %s", package[["geo-location"]],
                  package_url))
}