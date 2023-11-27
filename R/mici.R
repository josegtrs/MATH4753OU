#' Creates 95% CI
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' @export
myci = function(x) {
  t.test(x, conf.level = .95)
}
