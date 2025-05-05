#' @title Workflows
#' @description This internal function is the workhorse of [`cl_lapply_workflow()`]. The function wraps an `algorithm` function.
#' @param .sim,.datasets,.constructor,.algorithm,.trials,.success,.write,.coffee Arguments directly inherited from [`cl_lapply_workflow()`]. `sim` is the `iteration` [`data.table::data.table`] row.
#' @param .verbose A `logical` variable that defines whether or not to send user outputs to the console or a `.txt` file. 
#' @details For details, see the wrapper function [`cl_lapply_workflow()`].
#' @author Edward Lavender
#' @name workflow
#' @keywords internal

workflow <- function(.sim, .datasets, .constructor, ..., 
                     .algorithm, .trials, .success, 
                     .write, .coffee, .verbose) {
  
  # Initialise
  coffee_do(.coffee)
  cat_next(.index = .sim$index, .verbose = .verbose)
  
  # Define algorithm inputs
  args     <- .constructor(.sim = .sim, 
                           .datasets = .datasets, 
                           .verbose = .verbose, ...)
  
  # Run algorithm
  success <- TRUE
  t1      <- Sys.time()
  pout    <- tryCatch(do.call(.algorithm, args), error = function(e) e)
  t2      <- Sys.time()
  
  # Handle errors
  if (inherits(pout, "error")) {
    trials  <- NA_integer_
    success <- FALSE
    error   <- pout$message
    message(error)
  } else {
    trials  <- .trials(pout)
    success <- .success(pout)
    error   <- NA_character_
  }
  time <- secs(t2, t1)
  
  # Collect success statistics
  dout <- data.table(id        = .sim$index, 
                     algorithm = deparse(substitute(.algorithm)), 
                     ntrial    = trials,
                     success   = success, 
                     error     = error, 
                     time      = time)
  
  # (optional) Write outputs
  # If .sim$file_output is specified, we write outputs if !is.na(error) _even if_ success = FALSE
  do_write <- rlang::has_name(.sim, "file_output")
  if (do_write) {
    if (is.na(error)) {
      .write(pout, .sim$file_output)
    }
    qs::qsave(dout, file.path(dirname(.sim$file_output), "callstats.qs"))
    return(nothing())
  } else {
    return(list(output = pout, callstats = dout))
  }
  
}
