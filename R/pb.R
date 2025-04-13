#' @title Progress bar wrappers
#' @description These simple wrappers facilitate the internal implementation of progress bars via `pbapply` functions.
#' @param .min,.max,.pb,.t Arguments.
#' @example man/examples/example-pb.R
#' @seealso These functions wrap [`pbapply::setpb()`] and associated routines.
#' @name pb

#' @rdname pb
#' @export

pb_init <- function(.min, .max) {
  pbapply::startpb(min = .min, max = .max)
}

#' @rdname pb
#' @export

pb_tick <- function(.pb, .t) {
  pbapply::setpb(pb = .pb, value = .t)
}

#' @rdname pb
#' @export

pb_close <- function(.pb) {
  pbapply::closepb(pb = .pb)
}
