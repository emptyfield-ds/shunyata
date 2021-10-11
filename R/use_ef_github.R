#' Create a Bare-Bones Repo on Empty Field DS
#'
#' `use_ef_github()` creates a bare-bones repo intended for use with
#' [`sync_module()`]
#'
#' @param repo_name The name of the repo to be created
#' @param private Logical. Should the repo be private?
#'
#' @return Opens the URL of the repo
#' @export
use_ef_github <- function(repo_name, private = FALSE) {
  check_no_gh_repo(repo_name)
  usethis::ui_done("Creating {usethis::ui_path(glue('emptyfield-ds/{repo_name}'))}")
  create <- gh::gh(
    "POST /orgs/{org}/repos",
    org = "emptyfield-ds",
    name = repo_name,
    visibility = ifelse(private, "private", "public"),
    .accept = "application/vnd.github.nebula-preview+json"
  )

  usethis::ui_todo("Sync a module to this repo with {usethis::ui_code('sync_module()')}")
  browseURL(create$html_url)
}
