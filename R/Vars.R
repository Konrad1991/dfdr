Vars <- R6::R6Class(
  
"Vars",
                    
public = list(
  
  left_vars = NULL,
  right_vars = NULL,
  function_to_ignore = c("if", "else if", "else", "<", ">", "<=", ">=", "==", "!=", "{"),
  symbol_fct_call = NULL,
  left_side = FALSE,
  functions = c("[", "(", "+", "-", "/", "*", "log", "exp", "^", "return"),
  
  initialize = function(function_list) {
    self$functions <- c(self$functions, function_list)
  },

  save_rs = function(inp) {
    self$right_vars <- c(self$right_vars, inp)
  },
  
  save_ls = function(inp) {
    self$left_vars <- c(self$left_vars, inp)
  },
  
  find_vars = function(codeline) {
    if(!is.call(codeline)) {
      if(self$left_side) {
        if(as.name("[") == codeline) {
          self$left_side <- TRUE
        } else if(as.name("<-") == codeline) {
          self$left_side <- TRUE
        } else {
          self$left_side <- FALSE
        }
      } 
      
      return(codeline)
    } 
    
    codeline = as.call(codeline)
    codeline = as.list(codeline)
    
    if( (codeline[[1]] == as.name("<-")) ||
        (codeline[[1]] == as.name("=")) ) {

      self$save_ls(codeline[[2]])

      temp <- codeline[3:length(codeline)]
      for(i in seq_along(temp)) {
        ti <- temp[[i]]
        if(!is.call(ti) && length(ti) == 1L && is.name(ti)) {
          self$save_rs(ti)
        }  
      }
      
      self$left_side <- TRUE
      
    } else if( deparse(codeline[[1]]) %in% self$function_to_ignore) {
      
    } else {
      
      if(!is.call(codeline)) {
        if(codeline[[1]] != as.name("[")) {
          if(self$left_side == FALSE) {
            sapply(codeline, function(x) {
              if(is.symbol(x)) {
                temp <- deparse(x)
                if( !(temp %in% self$functions) ) {
                  self$save_rs(temp)
                }
              } else {
                if(is.language(x)) {
                  to_check <- as.list(x)[[1]]
                  to_check <- deparse(to_check)
                  if(! to_check %in% self$functions) {
                    self$symbol_fct_call <- c(self$symbol_fct_call, to_check)
                  }
                }
              }
            })
            
          } 
        } else {
          code <- c(as.name(codeline[[1]]), codeline[2:length(codeline)])
          code <- as.call(code)
          code <- rlang::enexpr(code)
          if(self$left_side == FALSE) {
            self$save_rs(code)
          } 

        }
          
      }
     
    }
    
    lapply(codeline, self$find_vars)
  },
  
  get_ls = function() {
    return(self$left_vars)
  },
  
  get_rs = function() {
    self$right_vars <- unique(self$right_vars)
    self$symbol_fct_call <- unique(self$symbol_fct_call)
    if(!is.null(self$symbol_fct_call) && !identical(self$symbol_fct_call, character(0))) {
      self$right_vars <- self$right_vars[self$right_vars != self$symbol_fct_call]  
    }
    
    ret <- lapply(self$right_vars, function(x) {
      if(is.character(x)) {
        ret <- str2lang(x)
        names(ret) <- NULL
        return(ret)
      }
      return(x)
    })
    return(ret)
  }
 
)   # end public list                 
)


