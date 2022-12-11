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
#'     y[1] <- x[1]*a - x[1]*x[2]*b + sin(a)
#'     y[2] <- x[1]*x[2]*c - x[2]*d
#'     return(y)
#' }
#' library(dfdr)
#' jacobian(f, y, x)
jacobian <- function(f, y, x, derivs = NULL) {
  y <- rlang::ensym(y) 
  y <- rlang::as_string(y)
  x <- rlang::ensym(x) 
  x <- rlang::as_string(x)
  
  stopifnot("the function is missing"=!is.null(f))
  stopifnot("the variable y is missing"=!is.null(y))
  stopifnot("the variable x is missing"=!is.null(x))

  # create function list
  # ============================================================================
  fl <- dfdr:::init_fct_list()
  if(!is.null(derivs)) {
    fd <- derivs@funs
    for(i in seq_along(fd)) {
      fl <- append_fct(fl, fd[[i]]@name, fd[[i]]@dfdx, fd[[i]]@name_deriv, fd[[i]]@keep)
    }
  }
  fct_list <- get_names(fl)
  
  const_fcts <- character()
  for(i in fct_list) {
    if(get_keep(fl, fct_list[i])) const_fcts <- c(const_fcts, fct_list[i])
  }
  
  # extract body and replacing
  # ============================================================================
  body <- body_of_fct(f)
  to_be_replaced <- NULL
  counter <- 1
  to_replace <- NULL
  to_replace_index <- NULL
  all_vars_left <- NULL 
  all_vars_right <- NULL
  body_new <- as.list(body)
  counter_bn <- 1
  
  for(i in seq_along(1:length(body))) {
    in_if <- FALSE
    v <- Vars$new(const_fcts)
    ast <- v$find_vars(body[[i]])

    if(body[[i]][[1]] == as.name("if")) {
      in_if <- TRUE
    }
    if(body[[i]][[1]] == as.name("return")) next
    
    ls <- v$get_ls() 
    all_vars_left <- c(all_vars_left, ls)
    rs <- v$get_rs()

    if(identical(rs, list())) next
    independet_at_rs <- sapply(rs, function(y) {
      tocheck <- rbfv(y)
      if(tocheck == x) {
        return(y)
      }
    })
    independet_at_rs[sapply(independet_at_rs, is.null)] <- NULL
    all_vars_right <- c(all_vars_right, independet_at_rs)
    
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
    
    # replace directly
    index <- to_be_replaced[[counter - 1]][[1]]
    if(is.null(index)) next else index <- index + 1
    temp_body <- as.list(body_new)[index:length(body_new)]
    replace <- to_be_replaced[[counter - 1]][[2]]
    replace_with <- to_be_replaced[[counter - 1]][[3]]
    tmp_bd <- replace_all(temp_body, replace, replace_with)  
    for(k in seq_along(tmp_bd)) {
      tb <- tmp_bd[[k]]
      bn <- body_new[[index + k - 1]]
      if(!identical(tb, bn)) {
        body_new[[index + k - 1]] <- tb
      }
    }
    
    
    # check if block
    v <- Vars$new(const_fcts)
    ast <- v$find_vars(body_new[[i]])
    ls <- v$get_ls() 
    if(in_if) {
      for(j in seq_along(1:length(ls))) {
        tocheck <- rbfv(ls[[j]])
        if(tocheck != y) stop("Only the dependent variable (Input argument = y) is allowed at the left side")
      }
    }
    
    
    
  } # end loop over body
 
  body <- body_new
  
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
  
  body_new[[1]] <- str2lang( paste(noquote(jac_mat),
                                   "<- matrix(0, length(",
                                   x, "),",
                                   "length(",
                                   x, "))"
                                   ) )
  counter <- 2
  ret_found <- FALSE
  for(i in seq_along(1:length(body))) {
    in_if <- FALSE
    if(body[[i]][[1]] == as.name("if")) {
      in_if <- TRUE
    }
    if(body[[i]][[1]] == as.name("return")) {
      ret_found <- TRUE
      body_new[[counter]] <- bquote(return(.(str2lang(jac_mat)) ))
      counter <- counter + 1
      next
    }
    
    codeline <- NULL
    if(in_if) {
      u <- Unfold$new(diff, fl, to_diff, y, jac_mat)
      ast <- u$uf(body[[i]])
      codeline <- u$get_code(ast)
      body_new[[counter]] <- codeline
      counter <- counter + 1
    } else {
      codeline <- body[[i]]
      deriv <- diff(codeline[[2]], codeline[[3]], to_diff, y, fl, jac_mat)
      body_new[[counter]] <- codeline
      counter <- counter + 1
      for(j in seq_along(1:length(deriv))) {
        body_new[[counter]] <- deriv[[j]]
        counter <- counter + 1
      }
      
    }
  }
  if(ret_found == FALSE) {
    body_new[[counter]] <- bquote(return(.(str2lang(jac_mat)) ))
    counter <- counter + 1
  }
  
  body_new[sapply(body_new, is.null)] <- NULL
  body_new <- as.call(c(as.symbol("{"), body_new))
  body(f) <- body_new
  
  return(f)
}

