context("Get variables")

test_that("We get variables from left & right hand side", {

  fl <- dfdr:::init_fct_list()
  fct_list <- get_names(fl)
  const_fcts <- character()
  for(i in fct_list) {
    if(get_keep(fl, fct_list[i])) const_fcts <- c(const_fcts, fct_list[i])
  }
  
  v <- Vars$new(const_fcts)
  ast <- v$find_vars(quote(c[1] <- d[1] + sin(1)))
  expect_equal(v$get_ls(), list(quote(c[1])))
  expect_equal(v$get_rs(), list(quote(d[1])))
  
  
  v <- Vars$new(const_fcts)
  ast <- v$find_vars(quote(c <- d[1] + sin(1)))
  expect_equal(v$get_ls(), list(quote(c)))
  expect_equal(v$get_rs(), list(quote(d[1])))
  
  v <- Vars$new(const_fcts)
  ast <- v$find_vars(quote(c[1] <- d + 1))
  expect_equal(v$get_ls(), list(quote(c[1])))
  expect_equal(v$get_rs(), list(quote(d)))
  
  
  v <- Vars$new(const_fcts)
  ast <- v$find_vars(quote(
    if(v < 1) {
      a <- c + r
      h <- 1 + rnorm(3) + j
    }
  ))
  expect_equal(v$get_ls(), list(quote(a), quote(h)))
  expect_equal(v$get_rs(), list(quote(c), quote(r), quote(j)))
  
  v <- Vars$new(const_fcts)
  ast <- v$find_vars(quote(c <- d + 1))
  expect_equal(v$get_ls(), list(quote(c)))
  expect_equal(v$get_rs(), list(quote(d)))
  
  v <- Vars$new(const_fcts)
  ast <- v$find_vars(quote(a <- b))
  expect_equal(v$get_ls(), list(quote(a)))
  expect_equal(v$get_rs(), list(quote(b)))
  
})
