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
rsc_archive_course <- function(space_id, rsc_user_id = getOption("shunyata.rsc_user_id")) {
  if (!is.null(rsc_user_id)) {
    usethis::ui_stop("{usethis::ui_code('rsc_user_id')} not found")
  }

  course <- rscloud::rscloud_space(space_id)
  course %>%
    rscloud::space_member_list() %>%
    dplyr::filter(.data$user_id != rsc_user_id) %>%
    dplyr::pull(.data$user_id) %>%
    rscloud::space_member_remove(
      space = course,
      users = .,
      remove_projects = TRUE
    )

  usethis::ui_todo("Add `Archived` prefix to course name")
  usethis::ui_todo("Deactivate invite link")

  invisible(space_id)
}
