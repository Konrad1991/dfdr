
# recursive walk over ast 
# ==============================================================================
Main <- R6::R6Class("Main",
                    
  public = list(
  
    initialize = function() {},
  
    get_ast = function() {},
  
    get_code = function() {},
  
    get_vars = function() {},
  
    replace_sub_term = function() {},
  
    make_fct = function() {},
  
    unfold_if = function() {},
  
    calc_deriv = function() {},
  
    build_entire_fct = function() {}
  
  )
)

body_of_fct <- function(f) {
  brackets <- body(f)[[1]]
  body <- NULL
  if(brackets != as.name("{")) {
    body <- body(f)  
  } else {
    body <- body(f)[2:length(body(f))]   
  }
  return(body)
}

replace_terms <- function(b, y, x) {
  
}



jacobian <- function(f, y, x) {
  stopifnot("the function is missing"=!is.null(f))
  stopifnot("the variable y is missing"=!is.null(y))
  stopifnot("the variable x is missing"=!is.null(x))
  
  # extract body
  body <- body_of_fct(f)
  
  # replace terms if (in)-dependent variable is found at right hand side
  
  # call dfdr::d for each term at right hand side
  
  # build all parts together
}