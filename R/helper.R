# helper functions
# ============================================================================
substi <- function(inp, replace, replace_with) {
  if (is.name(inp) && identical(inp, replace)) {
    return(bquote(.(replace_with)))
  }
  inp
}

