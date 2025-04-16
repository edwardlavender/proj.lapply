#' @title Workflows
#' @description This internal function is the workhorse of [`cl_lapply_workflow()`]. The function wraps an `algorithm` function.
#' @param .sim,.datasets,.constructor,.algorithm,.write,.coffee Arguments directly inherited from [`cl_lapply_workflow()`]. `sim` is the `iteration` [`data.table`] row.
#' @param .verbose A `logical` variable that defines whether or not to send user outputs to the console or a `.txt` file. 
#' @details For details, see the wrapper function [`cl_lapply_workflow()`].
#' @author Edward Lavender
#' @name workflow
#' @keywords internal

workflow <- function(.sim, .datasets, .constructor, ..., 
                     .algorithm, .write, .coffee, .verbose) {
  
  # Initialise
  coffee_do(.coffee)
  cat_next(.index = .sim$index, .verbose = .verbose)
  
  # Define algorithm inputs
  args     <- .constructor(.sim = .sim, 
                           .datasets = .datasets, 
                           .verbose = .verbose, ...)
  
  # Run algorithm
  error   <- NA_character_
  success <- TRUE
  t1      <- Sys.time()
  pout    <- tryCatch(do.call(.algorithm, args), error = function(e) e)
  t2      <- Sys.time()
  
  # Handle errors
  if (inherits(pout, "error")) {
    success <- FALSE
    error   <- pout$message
    message(error)
  } else {
    time <- secs(t2, t1)
  }
  
  # Collect success statistics
  # * TO DO
  # * ntrial = NA_integer_
  # * success = TRUE even if the algorithm did not converge (in the case of a particle algorithm);
  dout <- data.table(id        = .sim$index, 
                     algorithm = deparse(substitute(algorithm)), 
                     success   = success, 
                     error     = error, 
                     ntrial    = NA_integer_,
                     time      = time)
  
  # (optional) Write outputs
  do_write <- rlang::has_name(.sim, "file_output")
  if (do_write) {
    if (success) {
      .write(pout, .sim$file_output)
    }
    qs::qsave(dout, file.path(dirname(.sim$file_output), "callstats.qs"))
    return(nothing())
  } else {
    return(list(output = pout, callstats = dout))
  }
  
}