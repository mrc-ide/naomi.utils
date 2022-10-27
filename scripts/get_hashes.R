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
    message(sprintf("No release found for %s - skipping", package$name))
    return(NULL)
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

hash_resource <- function(resource, location) {
  tryCatch({
    t <- tempfile()
    httr::GET(utils::URLencode(resource$url), httr::write_disk(t),
                httr::add_headers("Content-Type" = "application/json",
                                  "X-CKAN-API-Key" = api_key))
    if (resource$resource_type == "inputs-unaids-geographic") {
        out <- readLines(t, n = 1, skipNul = TRUE)
        ## If you do not have access to download the resource CKAN returns
        ## you some HTML login page, if we get this then we want to error.
        ## Make sure this is a JSON file
        if (!startsWith(out[1], "{")) {
          stop("Your account does not have access to resource")
        }
      } else {
        out <- readLines(t, n = 2, skipNul = TRUE)
        ## If you do not have access to download the resource CKAN returns
        ## you some HTML login page, if we get this then we want to error.
        ## Make sure the csv does not contain HTML tags
        if (out[1] == "<!DOCTYPE html>" || out[2] == "<!DOCTYPE html>") {
          stop("Your account does not have access to resource")
        }
      }
      ## Also can return a HTML Gateway Time-out page if issue hitting endpoint
      ## if this is the case then error
      if (any(grepl("504 Gateway Time-out", out))) {
        stop("ADR timed out, upload manually or try again later")
      }
    resource$hash <- unname(tools::md5sum(t))
    resource$filename <- strsplit(basename(resource$url), "?", fixed = TRUE)[[1]][1]
  }, error = function(e) {
    message(sprintf("Failed to download file %s for country %s - skipping",
                    resource$resource_type, resource$country))
    message(paste0("  ", e$message))
    resource <<- NULL
  })
  resource
}

get_hash_from_release <- function(release) {
  inputs <- c("inputs-unaids-geographic", "inputs-unaids-anc",
               "inputs-unaids-art", "inputs-unaids-survey",
               "inputs-unaids-population", "inputs-unaids-spectrum-file",
               "inputs-unaids-shiny90-survey", "inputs-unaids-hiv-testing")
  input <- lapply(release$resources, function(resource) {
    if (!is.null(resource$resource_type) && resource$resource_type %in% inputs) {
      list(country = release$`geo-location`,
           resource_type = resource$resource_type,
           url = resource$url)
    } else {
      NULL
    }
  })
  input <- input[vapply(input, function(x) !is.null(x) && length(x) > 0, logical(1))]
  hashes <- lapply(input, hash_resource)
  hashes <- hashes[vapply(hashes, function(x) !is.null(x), logical(1))]
  do.call(rbind.data.frame, hashes)
}

get_hashes <- function() {
  src <- "country-estimates-22"
  packages <- ckanr::package_search(q = sprintf("type:%s", src),
                                    rows = 1000)
  orgs_to_ignore <- c("fjelltopp", "avenir-health", "imperial-college-london",
                    "naomi-development-team", "unaids", "project-balance")
  orgs <- vapply(packages$results,
                 function(result) {
                   result[["organization"]][["name"]]
                 }, character(1))
  packages$results <- packages$results[!(orgs %in% orgs_to_ignore)]
  releases <- lapply(packages$results, get_release, "naomi_final_2022")
  releases <- releases[vapply(releases, function(x) !is.null(x), logical(1))]
  hashes <- lapply(releases, get_hash_from_release)
  do.call(rbind.data.frame, hashes)
}


# Get API key
## To set up API key in .Renviron: open your REnviron using
## file.edit("~/.Renviron") and save your key as "ADR_KEY"
api_key <- Sys.getenv("ADR_KEY")
url <- "https://adr.unaids.org/"
ckanr::ckanr_setup(url = url, key = api_key)
library(magrittr)
x <- get_hashes()
