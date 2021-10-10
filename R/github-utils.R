exists_on_gh <- function(repo) {
  tryCatch(
    {
      repo_info <- gh::gh(
        "/repos/{owner}/{repo}",
        owner = "emptyfield-ds",
        repo = repo
      )
      TRUE
    },
    http_error_404 = function(err) FALSE
  )
}

check_exists_on_gh <- function(repo) {
  found_it <- exists_on_gh(repo)
  if (!found_it) {
    usethis::ui_stop(
      "{usethis::ui_path(repo)} not found on GitHub. \\
      Do you need to use {usethis::ui_code('use_ef_github()')}?"
    )
  }
  invisible(TRUE)
}
