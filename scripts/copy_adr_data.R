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
  url <- "https://adr.unaids.org/"
} else if (site == "dev") {
  url <- "https://dev.adr.fjelltopp.org/"
} else {
  stop("Site must be prod or dev")
}
message("Running migration on site ", url)

ckanr::ckanr_setup(url = url, key = key)
src <- "country-estimates-22"
dest <- "country-estimates-23"
## The display name of the package being created, ADR requies this
dest_name <- "HIV Estimates 2023"
resources <- c("inputs-unaids-geographic", "inputs-unaids-anc",
               "inputs-unaids-art", "inputs-unaids-survey",
               "inputs-unaids-population", "inputs-unaids-spectrum-file",
               "inputs-unaids-shiny90-survey", "inputs-unaids-hiv-testing")

## No built in way to send custom commands to CKAN so do a gross hack
ckan_create_release <- function(id) {
  res <- ckanr:::ckan_POST(url = ckanr::get_default_url(),
                    method = "dataset_version_create",
                    body = jsonlite::toJSON(list(
                      dataset_id = id,
                      name = "2022 data transfer",
                      notes = paste0("Data automatically transferred from ",
                                     "2022 dataset. Added blank columns to ANC",
                                     " data for new indicators.")
                    ), auto_unbox = TRUE),
                    key = ckanr::get_default_key(),
                    encode = "json",
                    headers = ckanr:::ctj())
  ckanr:::jsl(res)
}

list_releases <- function(dataset_id) {
  res <- ckanr:::ckan_GET(
    url = ckanr::get_default_url(),
    method = "dataset_version_list",
    query = list(dataset_id = dataset_id),
    key = ckanr::get_default_key(),
    headers = ckanr:::ctj()
  )
  ckanr:::jsl(res)
}

package_show <- function(dataset_id, release_id) {
  res <- ckanr:::ckan_GET(
    url = ckanr::get_default_url(),
    method = "package_show",
    query = list(id = dataset_id, release = release_id),
    key = ckanr::get_default_key(),
    headers = ckanr:::ctj()
  )
  ckanr:::jsl(res)
}

get_release <- function(package, release_name) {
  releases <- list_releases(package$id)
  if (length(releases) == 0) {
    message(sprintf("No release found for %s using current data", package$name))
    return(package)
  }
  release_id <- NULL
  for (rel in releases) {
    if (rel$name == release_name) {
      release_id <- rel$id
      message(sprintf("Found release %s for package %s",
                      release_name, package$name))
      break
    }
  }
  if (is.null(release_id)) {
    message(sprintf(
      paste0(
        "Release %s not found in package %s with name %s, ",
        "falling back to most recent release"
      ),
      release_name, package$id, package$name
    ))
    release_id <- releases[[1]]$id
  }
  package_show(package$id, release_id)
}

packages_src <- ckanr::package_search(q = sprintf("type:%s", src),
                                       rows = 1000)
packages_dest <- ckanr::package_search(q = sprintf("type:%s", dest),
                                       rows = 1000)

## Don't migrate data for Fjeltopp, Naomi development team, Imperial, unaids
## or avenir orgs as these are usually duplicates of country data
orgs_to_ignore <- c("fjelltopp", "avenir-health", "imperial-college-london",
                    "naomi-development-team", "unaids", "project-balance")
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

packages_keep <- vapply(packages_src$results, function(package) {
  package[["geo-location"]] %in% countries_to_copy
}, logical(1))
packages_copy <- packages_src$results[packages_keep]

## Get named release for package
releases <- lapply(packages_copy, get_release, "naomi_final_2022")
releases <- releases[vapply(releases, function(x) !is.null(x), logical(1))]

upload_package <- function(package_id, package_name, path, resource_type) {
  ckanr::resource_create(
    package_id,
    name = package_name,
    upload = path,
    extras = list(
      restricted = paste0('{"allowed_organizations": "unaids", ',
                          '"allowed_users": "", "level": "restricted"}'),
      resource_type = resource_type))
}

## For each country, download 2021 resources
## Upload as 2022 dataset in countries organisation
t <- tempfile()
dir.create(t)
package_no <- 0
for (package in releases) {
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
                      year = "2023"),
        maintainer = "Naomi team",
        notes = "A record of the input data and final HIV estimates.")
    },
    error = function(e) {
      message("Failed to create package for ", package[["geo-location"]])
      message(paste0("  ", e$message))
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
    ## if in a release the name of the file is file_name.extension?activity_id=...
    ## uploading with this filename means ADR cannot detect file type and screws up
    ## validation
    file_name <- strsplit(basename(details[["url"]]), "?", fixed = TRUE)[[1]][1]
    path <- file.path(country_dir, file_name)
    upload <- TRUE
    tryCatch({
      httr::GET(utils::URLencode(details$url), httr::write_disk(path),
                httr::add_headers("Content-Type" = "application/json",
                                  "X-CKAN-API-Key" = key))
      if (resource == "inputs-unaids-geographic") {
        out <- readLines(path, n = 1, skipNul = TRUE)
        ## If you do not have access to download the resource CKAN returns
        ## you some HTML login page, if we get this then we want to error.
        ## Make sure this is a JSON file
        if (!startsWith(out[1], "{")) {
          upload <- FALSE
          stop("Your account does not have access to resource")
        }
      } else {
        out <- readLines(path, n = 2, skipNul = TRUE)
        ## If you do not have access to download the resource CKAN returns
        ## you some HTML login page, if we get this then we want to error.
        ## Make sure the csv does not contain HTML tags
        if (out[1] == "<!DOCTYPE html>" || out[2] == "<!DOCTYPE html>") {
          upload <- FALSE
          stop("Your account does not have access to resource")
        }
      }
      ## Also can return a HTML Gateway Time-out page if issue hitting endpoint
      ## if this is the case then error
      if (any(grepl("504 Gateway Time-out", out))) {
        upload <- FALSE
        stop("ADR timed out, upload manually or try again later")
      }
    },
    error = function(e) {
      upload <- FALSE
      message(sprintf("Failed to download file %s for country %s - skipping",
                      resource, package[["geo-location"]]))
      message(paste0("  ", e$message))
    })
    if (!upload || !file.exists(path)) {
      create_release <<- FALSE
      next
    }
    message(sprintf("Uploading %s file for country %s",
                    resource, package[["geo-location"]]))
    if (!dry_run) {
      tryCatch({
        ## We want to add new columns "anc_known_neg" and "births_facility"
        ## to ANC data so it passes validation
        if (resource == "inputs-unaids-anc") {
          x <- read.csv(path)
          if (!("anc_known_neg" %in% colnames(x))) {
            x$anc_known_neg <- NA_integer_
          }
          if (!("births_facility" %in% colnames(x))) {
            x$births_facility <- NA_integer_
          }
          path <- file.path(country_dir, paste0("updated", basename(path)))
          write.csv(x, path, quote = FALSE, row.names = FALSE,
                    na = "")
        }
        new_resource <- NULL
        attempt <- 0
        while (is.null(new_resource) && attempt < 5) {
          attempt <- attempt + 1
          message(sprintf("Uploading %s for country %s: attempt %s",
                  resource, package[["geo-location"]], attempt))
          tryCatch(
            {
              new_resource <- upload_package(new_package[["id"]],
                                             details[["name"]],
                                             path,
                                             resource)
            },
            error = function(e) {
              wait <- 120 * attempt
              message(sprintf(paste0("Failed to upload file %s: attempt %s. ",
                                     "Trying again in %s seconds."),
                              resource, attempt, wait))
              ## If we've failed to upload it might be because ADR is
              ## dealing with too many requests. Wait with incremental back off
              Sys.sleep(wait)
            }
          )
        }
      },
      error = function(e) {
        message(sprintf("Failed to upload file %s for country %s - skipping",
                        resource, package[["geo-location"]]))
        message(paste0("  ", e$message))
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
      message(paste0("  ", e$message))
    })
  }
  if (dry_run) {
    package_url <- "dry run - not created"
  } else {
    package_url <- paste0(url, dest, "/", new_package$name)
  }
  message(sprintf("Copy complete for %s see %s", package[["geo-location"]],
                  package_url))
  package_no <- package_no + 1
  if ((package_no %% 5) == 0) {
    ## We've seen issues with ADR receiving too many requests in a short period
    ## of time. After every 5 packages are uploaded wait for 2 mins to give
    ## ADR time to handle requests
    message(paste0("5 packages uploaded, sleeping for 8 mins to avoid ",
                   "overloading ADR with requests"))
    Sys.sleep(60 * 8)
  }
}
