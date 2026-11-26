#! /usr/bin/env Rscript
"Export the active members of every ADR organisation, with their role,
  email, job title and affiliation.

  This is adapted from a Python script Ian Wanyekii sent on 10/11/2026.

  Writes to --output-dir:
    adr_users.csv                         all active organisation members,
                                          with an is_facilitator column
    adr_users_full.xlsx                   all members, sorted for reading
    adr_users_minus_facilitators.xlsx     country team members only
    adr_facilitators.xlsx                 facilitators only
    teams/<country>_HIV_estimates_team_ADR.xlsx
                                          one file per country team

  Emails are only returned by ADR for a sysadmin key; with any other key the
  email column will be blank.

  The API key is read from --key if given, otherwise from the ADR_API_KEY
  environment variable.

Usage:
    adr_users.R --output-dir=<dir> [--site=<site>] [--key=<key>] [--csv-only]
    adr_users.R -h | --help

Options:
    --output-dir=<dir>  Directory to write outputs to.
    --site=<site>       ADR site to use dev or prod [default: dev].
    --key=<key>         ADR auth key, defaults to env var ADR_API_KEY.
    --csv-only          Only write adr_users.csv, skip the Excel outputs.
" -> doc

regional_mapping <- c(
  ## West and Central Africa
  "Benin" = "WCA", "Côte d’Ivoire" = "WCA", "Burundi" = "WCA",
  "Burkina Faso" = "WCA", "Cabo Verde" = "WCA", "Cameroon" = "WCA",
  "Chad" = "WCA", "Congo" = "WCA", "Gabon" = "WCA", "Guinea Bissau" = "WCA",
  "Guinea Ecuatorial" = "WCA", "Guinée" = "WCA", "Mali" = "WCA",
  "Mauritania" = "WCA", "Niger" = "WCA", "République centrafricaine" = "WCA",
  "République démocratique du Congo" = "WCA", "Guiné-Bissau" = "WCA",
  "São tomé e Príncipe" = "WCA", "Senegal" = "WCA", "Togo" = "WCA",
  "Gambia" = "WCA", "Ghana" = "WCA", "Liberia" = "WCA", "Nigeria" = "WCA",
  "Sierra Leone" = "WCA",
  ## East and Southern Africa
  "Angola" = "ESA", "Botswana" = "ESA", "Eritrea" = "ESA", "Eswatini" = "ESA",
  "Ethiopia" = "ESA", "Kenya" = "ESA", "Lesotho" = "ESA", "Malawi" = "ESA",
  "Mozambique" = "ESA", "Namibia" = "ESA", "Rwanda" = "ESA",
  "South Africa" = "ESA", "South Sudan" = "ESA", "Tanzania" = "ESA",
  "Uganda" = "ESA", "Zambia" = "ESA", "Zimbabwe" = "ESA", "Zanzibar" = "ESA",
  "Comoros" = "ESA", "Madagascar" = "ESA", "Mauritius" = "ESA",
  ## Others
  "Haiti" = "LAC",
  "Papua New Guinea" = "AP",
  ## Partners
  "Avenir Health" = "z_partners", "CDC" = "z_partners",
  "Fjelltopp" = "z_partners", "Global Fund" = "z_partners",
  "Imperial College London" = "z_partners",
  "Naomi Development Team" = "z_partners", "Project Balance" = "z_partners",
  "UNAIDS" = "z_partners", "UNICEF" = "z_partners", "USAID" = "z_partners",
  "UNAIDS Reference Group" = "z_partners",
  "XL Strategic Informatio LLC " = "z_partners", "Vanuatu" = "z_partners"
)

french_speaking <- c(
  "Benin", "Côte d’Ivoire", "Burundi", "Burkina Faso", "Cabo Verde",
  "Cameroon", "Chad", "Congo", "Gabon", "Guinea Bissau", "Guinea Ecuatorial",
  "Guinée", "Mali", "Mauritania", "Niger", "République centrafricaine",
  "République démocratique du Congo", "São tomé e Príncipe", "Senegal",
  "Guiné-Bissau", "Togo", "Comoros", "Madagascar", "Mauritius", "Haiti"
)

## ADR user names of facilitators, these are dropped from country team lists
facilitators <- c(
  "allen", "yakusika", "jberry", "tomek", "sberlow", "rtesra", "ekanmiang",
  "sbrar", "mmahy", "mehtan", "jgstover", "jeffeaton", "rwshiraishi",
  "rglaubius", "sgutreuter", "damisonih", "barretd", "aura-frangioni",
  "ostevens", "nmaina-5841", "ruu2", "herbsts", "clwamba", "carivs", "faikah",
  "nateheard", "ukwaanif", "eby-pascal", "buttonzhao", "cytusaidwa",
  "wanyekii", "ixf6", "m-kusumgar", "stevenforsythe", "gmahiane", "mcanne",
  "rash", "antoine-gasasira", "alex_allouin", "horaciore", "korenrompe",
  "rbhattacharjee", "ukwuanif", "adrien-allorant", "frescural", "feizzadeha",
  "nagbodo", "carla_xl", "mmgiroux", "betro001"
)

site_url <- function(site) {
  switch(site,
         prod = "https://adr.unaids.org/",
         dev = "https://dev.adr.fjelltopp.org/",
         stop("Site must be prod or dev", call. = FALSE))
}

resolve_key <- function(key) {
  if (is.null(key) || !nzchar(key)) {
    key <- Sys.getenv("ADR_API_KEY")
  }
  if (!nzchar(key)) {
    stop("No ADR API key, pass --key or set the ADR_API_KEY env var",
         call. = FALSE)
  }
  key
}

ckan_action <- function(url, key, action, query = list()) {
  res <- httr::GET(paste0(url, "api/3/action/", action),
                   query = query,
                   httr::add_headers(Authorization = key))
  body <- httr::content(res, as = "text", encoding = "UTF-8")
  if (httr::http_error(res)) {
    stop(sprintf("ADR request '%s' failed with status %s: %s",
                 action, httr::status_code(res), body), call. = FALSE)
  }
  parsed <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  if (!isTRUE(parsed$success)) {
    stop(sprintf("ADR request '%s' failed: %s", action,
                 jsonlite::toJSON(parsed$error, auto_unbox = TRUE)),
         call. = FALSE)
  }
  parsed$result
}

## CKAN caps how many results it returns per call (e.g. organization_list
## with all_fields defaults to 25), so page through with limit and offset.
## Older CKAN versions ignore limit/offset on some actions and return
## everything every time, so stop when a page contains nothing new.
ckan_paged <- function(url, key, action, query = list(), page_size = 25) {
  out <- list()
  seen <- character(0)
  offset <- 0
  repeat {
    page <- ckan_action(url, key, action,
                        c(query, list(limit = page_size, offset = offset)))
    ids <- vapply(page, function(x) x$id, character(1))
    new <- !(ids %in% seen)
    if (!any(new)) {
      break
    }
    out <- c(out, page[new])
    seen <- c(seen, ids[new])
    offset <- offset + length(page)
  }
  out
}

## Turns NULL (missing or JSON null) into "" so every field is a string
str_or_empty <- function(x) {
  if (is.null(x)) "" else as.character(x)
}

## For non-sysadmin keys CKAN returns a hash in place of the email (or
## nothing at all), treat anything which isn't an email address as missing
email_or_empty <- function(x) {
  x <- str_or_empty(x)
  if (grepl("@", x, fixed = TRUE)) x else ""
}

fetch_adr_users <- function(url, key) {
  orgs <- ckan_paged(url, key, "organization_list",
                     list(all_fields = "true", include_users = "true"))
  message(sprintf("Found %s organisations", length(orgs)))

  ## organization_list includes members but without email, job title or
  ## affiliation so join those on from user_list
  users <- ckan_paged(url, key, "user_list", page_size = 1000)
  users <- stats::setNames(users, vapply(users, function(u) u$id,
                                         character(1)))
  if (!any(vapply(users, function(u) nzchar(email_or_empty(u$email)),
                  logical(1)))) {
    message("WARNING: user_list returned no emails - the API key is ",
            "probably not a sysadmin key. Emails in the output will be blank.")
  }

  rows <- lapply(orgs, function(org) {
    members <- Filter(function(u) identical(u$state, "active"), org$users)
    if (length(members) == 0) {
      return(NULL)
    }
    title <- str_or_empty(org$title)
    region <- regional_mapping[title]
    data.frame(
      org = title,
      org_name = str_or_empty(org$name),
      capacity = vapply(members, function(u) str_or_empty(u$capacity),
                        character(1)),
      name = vapply(members, function(u) str_or_empty(u$name), character(1)),
      fullname = vapply(members, function(u) str_or_empty(u$fullname),
                        character(1)),
      email = vapply(members, function(u) email_or_empty(users[[u$id]]$email),
                     character(1)),
      job_title = vapply(members,
                         function(u) str_or_empty(users[[u$id]]$job_title),
                         character(1)),
      affiliation = vapply(members,
                           function(u) str_or_empty(users[[u$id]]$affiliation),
                           character(1)),
      region = if (is.na(region)) "" else unname(region),
      language = if (is.na(region)) "" else
        if (title %in% french_speaking) "French" else "English",
      stringsAsFactors = FALSE
    )
  })
  users <- do.call(rbind, rows)
  users$is_facilitator <- users$name %in% facilitators
  users
}

write_excel_outputs <- function(users, output_dir) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required for Excel output, install it or ",
         "use --csv-only", call. = FALSE)
  }
  users <- users[order(users$language, users$region, users$org,
                       -xtfrm(users$name)), ]
  readable <- data.frame(
    language = users$language,
    region = users$region,
    Country = users$org,
    Name = users$fullname,
    email = users$email,
    affiliation = users$affiliation,
    job_title = users$job_title,
    `ADR role` = users$capacity,
    ADRname = users$name,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(readable, file.path(output_dir, "adr_users_full.xlsx"))

  is_facilitator <- users$is_facilitator
  team <- readable[!is_facilitator, ]
  writexl::write_xlsx(team, file.path(output_dir,
                                      "adr_users_minus_facilitators.xlsx"))
  writexl::write_xlsx(readable[is_facilitator, ],
                      file.path(output_dir, "adr_facilitators.xlsx"))

  ## One file per country so ADR managers can see and contact their full team
  teams_dir <- file.path(output_dir, "teams")
  dir.create(teams_dir, showWarnings = FALSE)
  team <- team[team$region != "z_partners", ]
  team <- team[order(team$`ADR role`), c("region", "Country", "Name", "email",
                                         "affiliation", "job_title",
                                         "ADR role")]
  for (country in unique(team$Country)) {
    file_name <- paste0(gsub("[^[:alnum:]]+", "_", country),
                        "_HIV_estimates_team_ADR.xlsx")
    writexl::write_xlsx(team[team$Country == country, ],
                        file.path(teams_dir, file_name))
  }
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  opts <- docopt::docopt(doc, args)
  url <- site_url(opts[["site"]])
  key <- resolve_key(opts[["key"]])
  output_dir <- opts[["output_dir"]]
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  message("Fetching ADR users from ", url)
  users <- fetch_adr_users(url, key)

  csv_path <- file.path(output_dir, "adr_users.csv")
  utils::write.csv(users, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  message(sprintf("Wrote %s users to %s", nrow(users), csv_path))

  if (!opts[["csv_only"]]) {
    write_excel_outputs(users, output_dir)
    message("Wrote Excel outputs to ", output_dir)
  }
  invisible(users)
}

if (!interactive()) {
  main()
}
