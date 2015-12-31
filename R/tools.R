#' function for handling function calls
function_call <- function(){
  res <- list()
  call_info       <- sys.call(sys.parent())
  res$info <- call_info
  call_function   <- call_info[[1]]
  res$func <- as.character(call_function )
  call_par_names  <- names(as.list(call_info)[-1])
  res$par_names <- call_par_names
  call_par_names  <- ifelse(nchar(call_par_names)>0, paste(call_par_names, "=", sep=""), call_par_names)
  call_par_values <- call_info[-1]
  res$par_values <- as.character(call_par_values)
  call_par        <- paste(call_par_names, call_par_values, collapse=", ", sep="")
  full_call       <- paste0(call_function, "(", call_par, ")")
  res$call <- full_call
  return(res)
}
