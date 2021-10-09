#' Use course template
#'
#' A wrapper around [usethis::use_template] to use templates in this package.
#'
#' @inheritParams usethis::use_template
#'
#' @export
use_course_template <- function(template, save_as = template, data = list(), ignore = FALSE, open = FALSE) {
  usethis::use_template(
    template,
    save_as = save_as,
    data = data,
    ignore = ignore,
    open = open,
    package = "shunyata"
  )
}
