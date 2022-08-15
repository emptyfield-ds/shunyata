#' Archive an RStudio Cloud Course
#'
#' `rsc_archive_course()` removes members except for `rsc_user_id`, moving space
#' member projects to their own workspace.
#'
#' @param space_id The RStudio Cloud space ID
#' @param rsc_user_id The user ID of the course lead
#'
#' @return Invisibly, the course ID
#' @export
rsc_archive_course <- function(space_id, rsc_user_id = getOption("shunyata.rscloud_user_id")) {
  if (is.null(rsc_user_id)) {
    usethis::ui_stop("{usethis::ui_code('rsc_user_id')} not found")
  }

  course <- rscloud::rscloud_space(space_id)
  course %>%
    rscloud::space_member_list() %>%
    dplyr::filter(.data$user_id != rsc_user_id) %>%
    rscloud::space_member_remove(
      space = course,
      users = .,
      remove_projects = TRUE
    )

  usethis::ui_todo("Add `Archived` prefix to course name")
  usethis::ui_todo("Deactivate invite link")

  invisible(space_id)
}

#' Zip modules to upload to RStudio Cloud
#'
#' `zip_modules()` zips modules in a directory for upload to RStudio Cloud, e.g.
#' the `mastering_r_for_epi` repository.
#'
#' @return invisibly, a vector of directories
#' @export
zip_modules <- function() {
  dirs <- dir_ls(type = "directory")
  purrr::walk(dirs, zip_module)
}

zip_module <- function(.x) {
  withr::local_dir(.x)
  usethis::ui_done("Zipping {usethis::ui_path(.x)}")
  zip::zip(fs::path_ext_set(.x, ".zip"), fs::dir_ls())
}
