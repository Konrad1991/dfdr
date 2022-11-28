jacobian <- function(f, y, x, derivs = NULL) {
  stopifnot("the function is missing"=!is.null(f))
  stopifnot("the variable y is missing"=!is.null(y))
  stopifnot("the variable x is missing"=!is.null(x))
  
  # helper functions
  body_of_fct <- function(f) {
    brackets <- body(f)[[1]]
    body <- NULL
    if(brackets != as.name("{")) {
      body <- body(f)  
    } else {
      l <- length(body(f))
      body <- body(f)[2:l]
    }
    return(body)
  }
  
  replace_all <- function(b, to_replace, replace_with) {
    r <- Replace$new()
    replace_with <- deparse(r$get_code(replace_with))
    print(replace_with)
    print(to_replace)
    r$set_replace(to_replace, replace_with)
    res <- c()
    for(i in seq_along(1:length(b))) {
      b_i <- r$replace(b[[i]])
      b_i_call <- r$get_code(b_i)
      res <- c(res, b_i_call)
    }
    res
  }
  
  fl <- dfdr:::init_fct_list()
  if(!is.null(derivs)) {
    fd <- derivs@funs
    for(i in seq_along(fd)) {
      fl <- append_fct(fl, fd[[i]]@name, fd[[i]]@dfdx, fd[[i]]@name_deriv)
    }
  }
  fct_list <- get_names(fl)

  # extract body
  body <- body_of_fct(f)

  # get variables
  # Examples:
  # dependent variable = y
  # independent variable = x
  # In main scope
  # y[1] = ...  : allowed
  # x[1] = 1    : this is actually allowed.
  #               However, it does not make sense, at least in the context of ode-systems.
  #               Throw either warning or error
  # a    = x[1] : the variable 'a' has to be replaced with x[1] in every following line (also in the if/else if/else blocks).
  # b    = y[1] : the variable 'b' has to be replaced with x[1] in every following line (also in the if/else if/else blocks).
  #
  # In if/else if/else block
  # y[1] = ...  : allowed
  # x[1] = 1    : not allowed.
  # a    = x[1] : not allowed
  # b    = y[1] : not allowed

  to_replace <- NULL
  to_replace_index <- NULL
  
  for(i in seq_along(1:length(body))) {
    in_if <- FALSE
    v <- Vars$new(fct_list)
    ast <- v$find_vars(body[[i]])
    if(body[[i]][[1]] == as.name("if")) {
      in_if <- TRUE
    }
    ls <- v$get_ls() 
    rs <- v$get_rs() 
    
    # check if block
    if(in_if) {
      if(length(ls) != 0) {
        for(j in seq_along(1:length(ls))) {
          tocheck <- ls[[j]]
          if(length(as.list(ls[[j]])) > 1) {
            tocheck <- as.list(ls[[j]])[[2]]
          }
          if(tocheck == quote(x)) stop("Found independent variable on left hand side in if block. This is not supported.")
        }
      } 
    }
    if(in_if) {
      if(length(rs) != 0) {
        for(j in seq_along(1:length(rs))) {
          tocheck <- ls[[j]]
          if(length(as.list(rs[[j]])) > 1) {
            tocheck <- as.list(rs[[j]])[[2]]
          }
          if(tocheck == quote(x)) stop("Found independent variable on right hand side in if block. This is not supported.")
          if(tocheck == quote(y)) stop("Found dependent variable on right hand side in if block. This is not supported.")
        }
      } 
    }
    
    if(!in_if) {
      body_rest <- NULL
      for(j in seq_along(1:length(rs))) {
        tocheck <- rs[[j]]
        if(length(as.list(rs[[j]])) > 1) {
          tocheck <- as.list(rs[[j]])[[2]]
        }
        if( (tocheck == quote(x)) || (tocheck == quote(y)) ) {
          # replace terms if (in)-dependent variable is found at right hand side
          body_rest <- body[i:length(body)]
          replace_with <- NULL
          if(length(body[[i]]) >= 3) {
              replace_with <- body[[i]][[3]]
          }
          
          body_rest <- replace_all(body_rest, ls, replace_with)
          #body[i:length(body)] <- body_rest
        }
      }
     #print(body_rest)
     #cat("\n\n")
    } # main block
    
  }
  



  

  
  
  # call dfdr::d for each term at right hand side
  
  # build all parts together
}

