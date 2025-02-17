# INFO: test for scalar literals.
# TRUE: Logical, Integer and double
is_literal <- function(obj) {
  is.atomic(obj) && (is.logical(obj) || is.integer(obj) || is.double(obj))
}
