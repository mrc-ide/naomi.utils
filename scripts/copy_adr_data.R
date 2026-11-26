#! /usr/bin/env Rscript
"Copy ADR datasets from a source package to a destination package. The `src`
  and `dest` can be changed or the resources which are copied over can be
  changed too. The maintainer of each new dataset is set to the
  organisation's admin (never a facilitator), falling back to Naomi team
  and naomi-support@unaids.org if there isn't one. A dataset will not be
  created if it already exists in the `dest` package type. Similary a dataset won't be created if there isn't a
  unique package for that country and type (as we don't know which to
  copy from). Before running delete any destination year packages you
  want to replace.

  The API key is read from --key if given, otherwise from the ADR_API_KEY
  environment variable.

  A summary CSV is written with one row per dataset: status (success,
  partial, failed or skipped), link, whether copied from the release or
  latest data, admin and any file issues.

Usage:
    copy_adr_data.R  [--dry-run] [--site=<site>] [--key=<key>] [--summary=<path>]
    copy_adr_data.R -h | --help

Options
    --dry-run     Run in dry-run mode.
    --site=<site> ADR site to use dev or prod [default: dev].
    --key=<key>   ADR auth key, defaults to env var ADR_API_KEY.
    --summary=<path>  Path to write the summary CSV to, one row per dataset.
                      Defaults to copy_adr_data_summary_<date>_<time>.csv
" -> doc

args <- docopt::docopt(doc)
dry_run <- args[["dry_run"]]
summary_path <- args[["summary"]]
if (is.null(summary_path)) {
  summary_path <- sprintf("copy_adr_data_summary_%s.csv",
                          format(Sys.time(), "%Y-%m-%d_%H%M%S"))
}
key <- args[["key"]]
if (is.null(key) || !nzchar(key)) {
  key <- Sys.getenv("ADR_API_KEY")
}
if (!nzchar(key)) {
  stop("No ADR API key, pass --key or set the ADR_API_KEY env var")
}
site <- args[["site"]]
if (site == "prod") {
  url <- "https://adr.unaids.org/"
} else if (site == "dev") {
  url <- "https://dev.adr.fjelltopp.org/"
} else {
  stop("Site must be prod or dev")
}
message("Running migration on site ", url)

src_year <- "2026"
dest_year <- "2027"
src <- "country-estimates-26"
dest <- "country-estimates-27"
## The display name of the package being created, ADR requires this
dest_name <- paste("HIV Estimates", dest_year)
resources <- c("inputs-unaids-geographic", "inputs-unaids-anc",
               "inputs-unaids-art", "inputs-unaids-survey",
               "inputs-unaids-population", "inputs-unaids-spectrum-file",
               "inputs-unaids-shiny90-survey", "inputs-unaids-hiv-testing")

## Call a CKAN action. We don't use ckanr as it sends the key in the
## X-CKAN-API-Key header which ADR ignores, so requests are made anonymously.
## ADR needs it in the Authorization header.
ckan_action <- function(action, query = NULL, body = NULL,
                        encode = "json") {
  action_url <- paste0(url, "api/3/action/", action)
  auth <- httr::add_headers(Authorization = key)
  res <- if (is.null(body)) {
    httr::GET(action_url, query = query, auth)
  } else {
    httr::POST(action_url, body = body, encode = encode, auth)
  }
  text <- httr::content(res, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (httr::http_error(res) || !isTRUE(parsed$success)) {
    detail <- if (is.null(parsed$error)) substr(text, 1, 500) else
      jsonlite::toJSON(parsed$error, auto_unbox = TRUE)
    stop(sprintf("%s %s: %s", action, httr::status_code(res), detail),
         call. = FALSE)
  }
  parsed$result
}

ckan_create_release <- function(id) {
  ckan_action("dataset_version_create", body = list(
    dataset_id = id,
    name = paste(src_year, "data transfer"),
    notes = paste("Data automatically transferred from", src_year, "dataset.")
  ))
}

list_releases <- function(dataset_id) {
  ckan_action("dataset_version_list", query = list(dataset_id = dataset_id))
}

package_show <- function(dataset_id, release_id) {
  ckan_action("package_show", query = list(id = dataset_id,
                                           release = release_id))
}

package_search <- function(type) {
  ckan_action("package_search", query = list(q = sprintf("type:%s", type),
                                             rows = 1000,
                                             include_private = "true"))
}

## Fail fast if the key isn't accepted. Anonymous requests still succeed for
## reads, so without this a bad key only shows up when we try to write.
## organization_list_for_user returns the orgs the requesting user can
## create datasets in - empty for anonymous, every org for a sysadmin.
check_key <- function() {
  orgs <- ckan_action("organization_list_for_user",
                      query = list(permission = "create_dataset"))
  if (length(orgs) == 0) {
    stop("API key was not accepted by ", url, " or the user cannot create ",
         "datasets in any organisation. Check the key is valid for this site.",
         call. = FALSE)
  }
  message(sprintf("API key accepted, user can create datasets in %s orgs",
                  length(orgs)))
}

check_key()

## Get the named release of a package, falling back to the latest data if
## there isn't one. Records which was used in `copy_source` for the summary.
get_release <- function(package, release_name) {
  releases <- list_releases(package$id)
  for (rel in releases) {
    if (rel$name == release_name) {
      message(sprintf("Found release %s for package %s",
                      release_name, package$name))
      out_package <- package_show(package$id, rel$id)
      out_package$copy_source <- paste("release", release_name)
      return(out_package)
    }
  }
  if (length(releases) == 0) {
    message(sprintf("No release found for %s using current data",
                    package$name))
    package$copy_source <- "latest (package has no releases)"
  } else {
    message(sprintf(
      paste0("Release %s not found in package %s with name %s, ",
             "falling back to current data"),
      release_name, package$id, package$name))
    package$copy_source <- sprintf("latest (release %s not found)",
                                   release_name)
  }
  package
}

default_maintainer_email <- "naomi-support@unaids.org"

## Directory containing this script, so we can find adr_users.R next to it
script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE),
                   value = TRUE)
  if (length(file_arg) == 0) {
    return(getwd())
  }
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
}

## Run adr_users.R to get the members of every organisation. Key is passed
## via env var rather than on the command line so it doesn't show up in
## the process list.
fetch_adr_users <- function(site, key) {
  users_script <- file.path(script_dir(), "adr_users.R")
  if (!file.exists(users_script)) {
    stop("Cannot find adr_users.R at ", users_script)
  }
  out_dir <- tempfile("adr_users")
  old_key <- Sys.getenv("ADR_API_KEY", unset = NA)
  Sys.setenv(ADR_API_KEY = key)
  on.exit({
    if (is.na(old_key)) Sys.unsetenv("ADR_API_KEY")
    else Sys.setenv(ADR_API_KEY = old_key)
  })
  status <- system2(file.path(R.home("bin"), "Rscript"),
                    c(shQuote(users_script), paste0("--site=", site),
                      "--csv-only", paste0("--output-dir=", shQuote(out_dir))))
  if (status != 0) {
    stop("adr_users.R failed with exit status ", status)
  }
  utils::read.csv(file.path(out_dir, "adr_users.csv"),
                  colClasses = "character", encoding = "UTF-8")
}

## Find the admin of an org to use as dataset maintainer. Facilitators are
## never selected. Returns a list with the admin's ADR user name, full name
## and email, falling back to the default "Naomi team" and naomi-support
## email if there isn't a suitable admin.
get_maintainer <- function(adr_users, org_name) {
  default <- list(name = NA_character_, fullname = "Naomi team",
                  email = default_maintainer_email)
  admins <- adr_users[adr_users$org_name == org_name &
                        adr_users$capacity == "admin", ]
  facilitator <- admins$is_facilitator == "TRUE"
  if (any(facilitator)) {
    message(sprintf("Ignoring %s facilitator admin(s) for organisation %s: %s",
                    sum(facilitator), org_name,
                    paste(admins$name[facilitator], collapse = ", ")))
    admins <- admins[!facilitator, ]
  }
  if (nrow(admins) == 0) {
    message(sprintf("No admin found for organisation %s, using %s",
                    org_name, default_maintainer_email))
    return(default)
  }
  if (nrow(admins) > 1) {
    message(sprintf("Found %s admins for organisation %s, using first: %s",
                    nrow(admins), org_name, admins$name[1]))
  }
  fullname <- admins$fullname[1]
  if (is.na(fullname) || !nzchar(fullname)) {
    fullname <- admins$name[1]
  }
  admin <- list(name = admins$name[1], fullname = fullname,
                email = admins$email[1])
  if (is.na(admin$email) || !nzchar(admin$email)) {
    message(sprintf(paste0("Admin %s of organisation %s has no email ",
                           "(is the key a sysadmin key?), using %s"),
                    admin$name, org_name, default_maintainer_email))
    return(default)
  }
  admin
}

format_maintainer <- function(maintainer) {
  if (is.na(maintainer$name)) {
    sprintf("default - %s <%s>", maintainer$fullname, maintainer$email)
  } else {
    sprintf("%s - %s <%s>", maintainer$name, maintainer$fullname,
            maintainer$email)
  }
}

adr_users <- fetch_adr_users(site, key)

packages_src <- package_search(src)
packages_dest <- package_search(dest)

## Don't migrate data for Fjeltopp, Naomi development team, Imperial, unaids,
## avenir, project balance or project linganisa orgs as these are usually
## duplicates of country data
orgs_to_ignore <- c("fjelltopp", "avenir-health", "imperial-college-london",
                    "naomi-development-team", "unaids", "project-balance",
                    "project-linganisa")
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
releases <- lapply(packages_copy, get_release, paste0("naomi_final_", src_year))
releases <- releases[vapply(releases, function(x) !is.null(x), logical(1))]

upload_package <- function(package_id, package_name, path, resource_type) {
  ckan_action("resource_create", encode = "multipart", body = list(
    package_id = package_id,
    name = package_name,
    upload = httr::upload_file(path),
    restricted = paste0('{"allowed_organizations": "unaids", ',
                        '"allowed_users": "", "level": "restricted"}'),
    resource_type = resource_type))
}

## One row of the summary CSV
summary_row <- function(country, org, status, maintainer = NULL,
                        dataset_url = "", source = "", source_url = "",
                        copied = character(0), issues = character(0),
                        missing = character(0), release_created = NA,
                        error = "") {
  collapse <- function(x) paste(x, collapse = "; ")
  data.frame(
    country = country,
    organisation = org,
    status = status,
    dry_run = dry_run,
    dataset_url = dataset_url,
    copied_from = source,
    source_dataset_url = source_url,
    admin_username = if (is.null(maintainer)) "" else
      ifelse(is.na(maintainer$name), "", maintainer$name),
    admin_name = if (is.null(maintainer)) "" else maintainer$fullname,
    admin_email = if (is.null(maintainer)) "" else maintainer$email,
    admin_is_default = if (is.null(maintainer)) NA else
      is.na(maintainer$name),
    n_files_copied = length(copied),
    n_file_issues = length(issues),
    files_copied = collapse(copied),
    file_issues = collapse(issues),
    files_missing_in_source = collapse(missing),
    release_created = release_created,
    error = error,
    stringsAsFactors = FALSE
  )
}

## Download a resource, returning the path or stopping with a reason
download_resource <- function(details, resource, country_dir) {
  ## basename of the URL contains the name of the file when first uploaded
  ## if in a release the name of the file is file_name.extension?activity_id=...
  ## uploading with this filename means ADR cannot detect file type and screws up
  ## validation
  file_name <- strsplit(basename(details[["url"]]), "?", fixed = TRUE)[[1]][1]
  path <- file.path(country_dir, file_name)
  ## TEMPORARY: resource URLs in releases always point at prod ADR, even
  ## on dev. Swap the scheme and host for the site we're running against.
  ## Remove once fixed in ADR.
  download_url <- sub("^https?://[^/]+/", url, details[["url"]])
  if (download_url != details[["url"]]) {
    message(sprintf("Rewrote download URL %s to %s", details[["url"]],
                    download_url))
  }
  httr::GET(utils::URLencode(download_url), httr::write_disk(path,
                                                              overwrite = TRUE),
            httr::add_headers("Content-Type" = "application/json",
                              "Authorization" = key))
  if (!file.exists(path)) {
    stop("Download did not produce a file")
  }
  if (resource == "inputs-unaids-geographic") {
    out <- readLines(path, n = 1, skipNul = TRUE)
    ## If you do not have access to download the resource CKAN returns
    ## you some HTML login page, if we get this then we want to error.
    ## Make sure this is a JSON file
    if (length(out) == 0 || !startsWith(out[1], "{")) {
      stop("Your account does not have access to resource")
    }
  } else {
    out <- readLines(path, n = 2, skipNul = TRUE)
    ## If you do not have access to download the resource CKAN returns
    ## you some HTML login page, if we get this then we want to error.
    ## Make sure the csv does not contain HTML tags
    if (any(out == "<!DOCTYPE html>")) {
      stop("Your account does not have access to resource")
    }
  }
  ## Also can return a HTML Gateway Time-out page if issue hitting endpoint
  ## if this is the case then error
  if (any(grepl("504 Gateway Time-out", out, useBytes = TRUE))) {
    stop("ADR timed out, upload manually or try again later")
  }
  path
}

## Upload a resource with retries, returning the new resource or stopping
upload_resource <- function(new_package, details, path, resource, country) {
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
    path <- file.path(dirname(path), paste0("updated", basename(path)))
    write.csv(x, path, quote = FALSE, row.names = FALSE, na = "")
  }
  max_attempts <- 5
  last_error <- ""
  for (attempt in seq_len(max_attempts)) {
    message(sprintf("Uploading %s for country %s: attempt %s",
                    resource, country, attempt))
    new_resource <- tryCatch(
      upload_package(new_package[["id"]], details[["name"]], path, resource),
      error = function(e) {
        last_error <<- conditionMessage(e)
        NULL
      })
    if (!is.null(new_resource)) {
      return(new_resource)
    }
    if (attempt < max_attempts) {
      wait <- 120 * attempt
      message(sprintf(paste0("Failed to upload file %s: attempt %s. ",
                             "Trying again in %s seconds."),
                      resource, attempt, wait))
      ## If we've failed to upload it might be because ADR is
      ## dealing with too many requests. Wait with incremental back off
      Sys.sleep(wait)
    }
  }
  stop(sprintf("upload failed after %s attempts: %s", max_attempts,
               last_error))
}

## Create the dest dataset for one country, copy its resources across and
## create a release. Returns a summary row.
copy_package <- function(package, work_dir) {
  country <- package[["geo-location"]]
  org <- package[["organization"]][["name"]]
  source_url <- paste0(url, src, "/", package[["name"]])
  country_dir <- file.path(work_dir, country)
  dir.create(country_dir)
  message("Creating package for ", country)
  maintainer <- get_maintainer(adr_users, org)
  message(sprintf("Using admin %s as maintainer for %s",
                  format_maintainer(maintainer), country))

  row <- function(status, ...) {
    summary_row(country, org, status, maintainer = maintainer,
                source = package$copy_source, source_url = source_url, ...)
  }

  new_package <- NULL
  if (!dry_run) {
    new_package <- tryCatch(
      ckan_action("package_create", body = list(
        type = dest,
        owner_org = org,
        private = FALSE,
        state = "active",
        "geo-location" = country,
        type_name = dest_name,
        maintainer_email = maintainer$email,
        maintainer = maintainer$fullname,
        year = src_year,
        notes = "A record of the input data and final HIV estimates.")),
      error = function(e) {
        message("Failed to create package for ", country)
        message(paste0("  ", conditionMessage(e)))
        structure(list(message = conditionMessage(e)), class = "failed")
      })
    if (inherits(new_package, "failed")) {
      return(row("failed",
                 error = paste("Failed to create dataset:",
                               new_package$message)))
    }
  }
  dataset_url <- if (dry_run) "" else paste0(url, dest, "/", new_package$name)

  resource_types <- vapply(package$resources, function(resource) {
    type <- resource[["resource_type"]]
    if (is.null(type)) {
      type <- "none"
    }
    type
  }, character(1))
  missing <- setdiff(resources, resource_types)
  copied <- character(0)
  issues <- character(0)
  if (length(missing) == length(resources)) {
    issues <- "source dataset has none of the input files"
  }
  for (resource in intersect(resources, resource_types)) {
    details <- package$resources[resource_types == resource]
    if (length(details) > 1) {
      message(sprintf("package %s has more than 1 resource of type %s - skipping",
                      package[["name"]], resource))
      issues <- c(issues, sprintf("%s: %s resources of this type in source, skipped",
                                  resource, length(details)))
      next
    }
    details <- details[[1]]
    path <- tryCatch(
      download_resource(details, resource, country_dir),
      error = function(e) {
        message(sprintf("Failed to download file %s for country %s - skipping",
                        resource, country))
        message(paste0("  ", conditionMessage(e)))
        issues <<- c(issues, sprintf("%s: download failed: %s", resource,
                                     conditionMessage(e)))
        NULL
      })
    if (is.null(path)) {
      next
    }
    message(sprintf("Uploading %s file for country %s", resource, country))
    if (!dry_run) {
      uploaded <- tryCatch({
        upload_resource(new_package, details, path, resource, country)
        TRUE
      },
      error = function(e) {
        message(sprintf("Failed to upload file %s for country %s - skipping",
                        resource, country))
        message(paste0("  ", conditionMessage(e)))
        issues <<- c(issues, sprintf("%s: %s", resource, conditionMessage(e)))
        FALSE
      })
      if (!uploaded) {
        next
      }
    }
    copied <- c(copied, resource)
  }

  ## Only create a release if every file copied cleanly
  release_created <- FALSE
  if (!dry_run && length(issues) == 0) {
    message(sprintf("Creating release for %s", country))
    release_created <- tryCatch({
      ckan_create_release(new_package[["id"]])
      TRUE
    },
    error = function(e) {
      message(sprintf("Failed to create release for country %s", country))
      message(paste0("  ", conditionMessage(e)))
      issues <<- c(issues, paste("release creation failed:",
                                 conditionMessage(e)))
      FALSE
    })
  }

  status <- if (length(issues) == 0) {
    "success"
  } else if (length(copied) > 0 || !dry_run) {
    ## Dataset exists on ADR but is incomplete
    "partial"
  } else {
    "failed"
  }
  message(sprintf("Copy %s for %s see %s, admin set to %s", status, country,
                  if (dry_run) "dry run - not created" else dataset_url,
                  format_maintainer(maintainer)))
  row(status, dataset_url = dataset_url, copied = copied, issues = issues,
      missing = missing,
      release_created = if (dry_run) NA else release_created)
}

## Write summary after every dataset so it's there even if the run dies
write_summary <- function(summary) {
  utils::write.csv(summary, summary_path, row.names = FALSE,
                   fileEncoding = "UTF-8")
}

## Countries we couldn't copy because there's more than 1 source dataset
summary <- do.call(rbind, lapply(multiple, function(country) {
  summary_row(country, "", "skipped",
              error = sprintf("More than 1 %s dataset for this country", src))
}))
if (is.null(summary)) {
  summary <- summary_row("", "", "")[0, ]
}
write_summary(summary)

## For each country, download previous year resources
## Upload as next year dataset in countries organisation
work_dir <- tempfile()
dir.create(work_dir)
package_no <- 0
for (package in releases) {
  summary <- rbind(summary, copy_package(package, work_dir))
  write_summary(summary)
  package_no <- package_no + 1
  if ((package_no %% 5) == 0) {
    ## We've seen issues with ADR receiving too many requests in a short period
    ## of time. After every 5 packages are uploaded wait for 2 mins to give
    ## ADR time to handle requests
    message(paste0("5 packages uploaded, sleeping for 8 mins to avoid ",
                   "overloading ADR with requests"))
    Sys.sleep(0 * 8)
  }
}

message("Summary written to ", normalizePath(summary_path))
status_counts <- table(summary$status)
message(paste(sprintf("  %s: %s", names(status_counts), status_counts),
              collapse = "\n"))
