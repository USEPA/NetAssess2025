## Run from the repository root with source("deploy.R") or Rscript deploy.R.
## Configure CONNECT_STAGING_SERVER locally and verify that its URL is staging.
## local() keeps deployment variables out of the calling R session.
local({
library(rsconnect)

app_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)

## Read metadata from this checkout; stop rather than deploy with unknown identity.
get_git_value <- function(args) {
  value <- system2("git", c("-C", shQuote(app_dir), args),
    stdout = TRUE, stderr = TRUE)
  status <- attr(value, "status")
  if (!is.null(status) && status != 0) {
    stop("Unable to read Git deployment metadata: ", paste(value, collapse = "\n"))
  }
  trimws(paste(value, collapse = "\n"))
}

## Use the branch name directly, without commit IDs or hash suffixes.
get_app_name <- function(branch) {
  if (identical(branch, "HEAD")) {
    stop("Check out a named branch before deploying; detached HEAD is not supported.")
  }
  ## Reject rather than rewrite names, so feature/foo cannot overwrite feature_foo.
  if (!grepl("^[A-Za-z0-9_-]+$", branch)) {
    stop("Deploy from a branch using only letters, digits, underscores, and hyphens.")
  }
  name <- paste0("ee_dvcalc_staging_", branch)
  if (nchar(name) > 64) {
    stop("Use a shorter branch name; the staging app name must be at most 64 characters.")
  }
  name
}

## Record the commit in bundle metadata, not in the app name.
current_branch <- get_git_value(c("rev-parse", "--abbrev-ref", "HEAD"))
git_commit <- get_git_value(c("rev-parse", "HEAD"))
current_commit <- substr(git_commit, 1, 7)
app_name <- get_app_name(current_branch)

## These settings belong to the local deployment process, not the hosted app.
## No generic/public server fallback is allowed; the URL itself is not validated.
#connect_server_url <- trimws(Sys.getenv("CONNECT_STAGING_SERVER"))
#if (!nzchar(connect_server_url)) {
#  stop("Set CONNECT_STAGING_SERVER to the staging Posit Connect URL.")
#}
connect_server_url <- "https://rstudio-connect.dmap-stage.aws.epa.gov"

connect_account <- trimws(Sys.getenv("CONNECT_STAGING_ACCOUNT"))
if (!nzchar(connect_account)) {
  connect_account <- "dmap"
}
connect_api_key <- Sys.getenv("CONNECT_STAGING_API_KEY")
## Match the existing rsconnect server alias where the staging accounts are saved.
connect_server_name <- "dmap"

## Register the staging URL locally. Without a supplied key, use saved credentials.
## Configure hosted AQS credentials separately in the Connect app environment.
rsconnect::addServer(url = connect_server_url, name = connect_server_name)
if (nzchar(connect_api_key)) {
  rsconnect::connectApiUser(
    account = connect_account,
    server = connect_server_name,
    apiKey = connect_api_key
  )
}

## Build a disposable copy; tracked app files stay unchanged, even on failure.
## Remove that copy when this local block exits, including after an error.
deployment_dir <- tempfile("ee_dvcalc_deploy_")
dir.create(deployment_dir)
on.exit(unlink(deployment_dir, recursive = TRUE), add = TRUE)

## Apply exclusions at every directory level while preserving runtime assets.
copy_app_files <- function(source_dir, target_dir) {
  entries <- list.files(source_dir, all.files = TRUE, no.. = TRUE)
  ## Never copy local environment/credential files, including example variants.
  entries <- entries[!grepl("^\\.Renviron($|\\.)|^\\.env($|\\.)", entries)]
  ## rsconnect scans bundled R/Rmd files for dependencies. Exclude deployment and
  ## database-refresh scripts so their non-runtime packages are not inferred.
  entries <- setdiff(entries, c(
    ".git", ".Rproj.user", "local_files", "rsconnect", "git_info.txt",
    "deploy.R", "EE_DVcalc_update.Rmd"
  ))
  for (entry in entries) {
    source <- file.path(source_dir, entry)
    target <- file.path(target_dir, entry)
    if (dir.exists(source)) {
      dir.create(target)
      copy_app_files(source, target)
    } else if (!file.copy(source, target, overwrite = TRUE)) {
      stop("Unable to package deployment file: ", source)
    }
  }
}

copy_app_files(app_dir, deployment_dir)

## Change only the copied global.R to load aqsdata.Rdata instead of public pins.
## Require exactly one recognized setting so unexpected source changes fail early.
## Concentration retrieval still uses the live AQS API in this mode.
global_file <- file.path(deployment_dir, "global.R")
global_lines <- readLines(global_file, warn = FALSE)
data_source_line <- grep("^public\\.server\\s*<-\\s*(TRUE|FALSE)\\s*$", global_lines)
if (length(data_source_line) != 1) {
  stop("Expected exactly one `public.server` TRUE/FALSE setting in global.R.")
}
global_lines[data_source_line] <- "public.server <- FALSE"
writeLines(global_lines, global_file)

## Include checkout identity and packaging time in the bundle for traceability.
deployed_at <- format(Sys.time(), "%Y-%m-%d %H:%M")
writeLines(c(
  paste0("branch: ", current_branch),
  paste0("commit: ", current_commit),
  paste0("deployed: ", deployed_at)
), file.path(deployment_dir, "git_info.txt"))

message(
  "Deploying ", app_name, " with ",
  "local Rdata data to the staging Connect server."
)

## Upload immediately, without a confirmation prompt or opening a browser.
## forceUpdate replaces this branch's existing staging app.
## Branch-only names create new apps rather than updating earlier hashed targets.
rsconnect::deployApp(
  appDir = deployment_dir,
  appName = app_name,
  account = connect_account,
  server = connect_server_name,
  launch.browser = FALSE,
  forceUpdate = TRUE
)
})
