Replace <- R6::R6Class(

"Replace",

public = list(
  
  initialize = function(){},
  
  get_code = function(code) {
    out <- purrr::map_if(code, is.list, self$get_code)
    out <- as.call(out)
    return(out)
  },
  
  # replacing
  to_replace = NULL,
  replace_with = NULL,
  
  set_replace = function(to_replace, replace_with) {
    self$to_replace = to_replace
    self$replace_with = replace_with
  },
  
  replace = function(code) {
    if(!is.call(code)) {
      return(code)
    }
    fct = code[[1]]
    code <- sapply(code, function(x) {
      if(is.symbol(x)) {
        if(is.null(self$to_replace)) return(x)
        
        if(deparse(x) == self$to_replace) {
          x = str2lang(paste0("(", self$replace_with, ")"))
        }
        return(x)
      } else {
        return(x)
      }
    })
    code <- as.call(code)
    code = as.list(code)
    lapply(code, self$replace)  
  }
  
) # end public
                  
)
