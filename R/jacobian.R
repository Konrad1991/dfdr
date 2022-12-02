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
#' @details   
#' In main scope \cr
#' y[1] = ...  : allowed \cr
#' x[1] = 1    : allowed \cr
#' a    = x[1] : replace it \cr
#' b    = y[1] : replace it \cr
#' In if/else if/else block \cr
#' y[1] = ...  : only this is allowed! \cr
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
  
  # extract body and replacing
  # ============================================================================
  body <- body_of_fct(f)
  to_be_replaced <- NULL
  counter <- 1
  to_replace <- NULL
  to_replace_index <- NULL
  all_vars_left <- NULL 
  all_vars_right <- NULL
  
  for(i in seq_along(1:length(body))) {
    in_if <- FALSE
    v <- Vars$new(fct_list)
    ast <- v$find_vars(body[[i]])
    if(body[[i]][[1]] == as.name("if")) {
      in_if <- TRUE
    }
    if(body[[i]][[1]] == as.name("return")) next
    
    ls <- v$get_ls() 
    all_vars_left <- c(all_vars_left, ls)
    rs <- v$get_rs() 
    independet_at_rs <- sapply(rs, function(y) {
      tocheck <- rbfv(y)
      if(tocheck == x) {
        return(y)
      }
    })
    independet_at_rs[sapply(independet_at_rs, is.null)] <- NULL
    all_vars_right <- c(all_vars_right, independet_at_rs)
    
    # check if block
    if(in_if) {
      for(j in seq_along(1:length(ls))) {
        tocheck <- rbfv(ls[[j]])
        if(tocheck != y) stop("Only the dependent variable (Input argument = y) is allowed at the left side")
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
  
  # jac matrix creation
  # ============================================================================
  all_vars_left <- as.character(c(all_vars_left, lapply(formalArgs(f), str2lang)))
  jac_mat <- "jac_mat"
  while(jac_mat %in% all_vars_left) {
    jac_mat <- paste0(jac_mat, "1")
  }
  
  # find all independent variables for which 'd' has to be called
  # ============================================================================
  to_diff <- unique(all_vars_right)
  
  # call dfdr::d for each term at right hand side
  # ============================================================================
  body_new <- list()
  for(i in seq_along(1:length(body))) {
    in_if <- FALSE
    if(body[[i]][[1]] == as.name("if")) {
      in_if <- TRUE
    }
    if(body[[i]][[1]] == as.name("return")) next
    
    codeline <- NULL
    
    if(in_if) {
      u <- Unfoldif$new(diff, fl, to_diff, y, jac_mat)
      ast <- u$unfold(body[[i]])
      codeline <- u$get_code(ast)
      #body_new[[i]] <- codeline
      print(codeline)
    } else {
      codeline <- body[[i]]
      deriv <- diff(codeline[[2]], codeline[[3]], to_diff, y, fl, jac_mat)
      body_new[[i]] <- c(codeline, unlist(deriv))
    }
  }
  
  #print(body_new)
}

