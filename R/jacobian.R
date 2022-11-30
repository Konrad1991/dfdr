#' Compute jacobian function
#'
#' Creates a function that computes the jacobian-matrix of a function with respect to one parameter
#' and return a matrix of these.
#'
#' @param f  A function
#' @param y The variables to compute the derivatives of (the dependent variable). For example: \eqn{\frac{dy}{dx}}
#' @param x The variables to which respect the variables are calcualted (the independent variable). For example: \eqn{\frac{dy}{dx}}
#' @param use_names Should the gradient add variable names to the output of the function?
#' @return  A function that computes the jacboian-matrix of f at any point.
#' @export
#' @examples
#' f <- function(t, x, p) {
#' a <- p[1]
#' b <- p[2]
#' c <- p[3]
#' d <- p[4]
#' a <- x[1]
#' if(t > 8) {
#'   a[1] <- 1
#'   b <- 6
#'   #x[1] <- 3 # not allowed
#'   y[1] <- 3
#'   #a <- y[2] # not allowed
#'   #a <- x[2] # not allowed
#' }
#' 
#' y[1] <- x[1]*a - x[1]*x[2]*b + sin(a)
#' y[2] <- x[1]*x[2]*c - x[2]*d
#' return(y)
#' }
#' library(dfdr)
#' jacobian(f, y, x)
jacobian <- function(f, y, x, derivs = NULL) {
  stopifnot("the function is missing"=!is.null(f))
  stopifnot("the variable y is missing"=!is.null(y))
  stopifnot("the variable x is missing"=!is.null(x))
  
  y <- rlang::ensym(y) 
  y <- rlang::as_string(y)
  x <- rlang::ensym(x) 
  x <- rlang::as_string(x)

  # helper functions
  # ============================================================================
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
    r$set_replace(to_replace, replace_with)
    res <- c()
    for(i in seq_along(1:length(b))) {
      b_i <- r$replace(b[[i]])
      b_i_call <- r$get_code(b_i)
      res <- c(res, b_i_call)
    }
    res
  }
  
  check <- function(a, b, c) {
    a == b || a == c
  }
  
  #remove_bracket_from_var
  rbfv <- function(var) {
    ret <- var
    if(length(as.list(var)) > 1) {
      ret <- as.list(var)[[2]]
    }
    ret
  }
  
  # get rs from one line
  grs <- function(line) {
    if(length(line) >= 3) {
      return(line[[3]])
    } else {
      stop("Something went wrong.")
    }
  }
  
  # create function list
  # ============================================================================
  fl <- dfdr:::init_fct_list()
  if(!is.null(derivs)) {
    fd <- derivs@funs
    for(i in seq_along(fd)) {
      fl <- append_fct(fl, fd[[i]]@name, fd[[i]]@dfdx, fd[[i]]@name_deriv)
    }
  }
  fct_list <- get_names(fl)
  
  to_be_replaced <- NULL
  counter <- 1
  to_be_differentiated <- NULL
  # extract body and replacing
  # ============================================================================
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
    if(body[[i]][[1]] == as.name("return")) next
    
    ls <- v$get_ls() 
    rs <- v$get_rs() 
    
    # check if block
    if(in_if) {
      if(length(rs) != 0) {
        for(j in seq_along(1:length(ls))) {
          tocheck_left <- rbfv(ls[[j]])
          tocheck_right <- rbfv(rs[[j]])
          print(tocheck_left)
          print(tocheck_right)
          cat("\n\n")
          if(tocheck_left == x) stop("Found independent variable on left hand side in if block. This is not supported.")
          if(tocheck_left == y) break
          if( (tocheck_left != y) && ((tocheck_right == x) || (tocheck_right == y)) ) {
            #print(body[[i]])
            print(tocheck_left == y)
            #print(tocheck_left)
            stop("Found (in)-dependent variable on right hand side in if block. This is not supported.")
          }
        }
      }
    }

    # find variables which have to be replaced in the following lines
    if(!in_if) {
      for(j in seq_along(1:length(rs))) {
        tocheck_right <- rbfv(rs[[j]])
        tocheck_left <- rbfv(ls[[1]])
        if( check(tocheck_right, x, y) && tocheck_left != y ) {
          to_be_replaced[[counter]] <- c(index = i, to_be_replaced = ls, replace_with = deparse(body[[i]][[3]])) 
          counter <- counter + 1
          break
        } 
      }
    }
    
    # find y at left side
    if(!in_if) {
      to_be_differentiated <- c(index = i)  
    } else if(in_if) {
      #print(as.list(body[[i]]))
    }
    
    
  } # end loop over body
  
  # replace values
  # ============================================================================
  body <- as.list(body)
  for(i in seq_along(1:length(to_be_replaced))) {
    if(i == length(body)) break
    index <- to_be_replaced[[i]][[1]]
    temp_body <- body[index:length(body)]
    replace <- to_be_replaced[[i]][[2]]
    replace_with <- to_be_replaced[[i]][[3]]
    body[index:length(body)] <- unlist(replace_all(temp_body, replace, replace_with))
    body[[index]] <- NULL
  }
  
  print(body)
  
  # call dfdr::d for each term at right hand side
  # ============================================================================
  
  # build all parts together
  # ============================================================================
}

