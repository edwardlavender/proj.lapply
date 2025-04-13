#' @title Workflows
#' @description This internal function is the workhorse of [`cl_lapply_workflow()`]. The function wraps an `algorithm` function.
#' @param sim,datasets,constructor,algorithm,coffee Arguments directly inherited from [`cl_lapply_workflow()`].
#' @param verbose A `logical` variable that defines whether or not to send user outputs to the console or a `.txt` file. 
#' @details For details, see the wrapper function [`cl_lapply_workflow()`].
#' @author Edward Lavender
#' @name workflow
#' @keywords internal

workflow <- function(sim, datasets, constructor, algorithm, coffee, verbose) {
  
  # Initialise
  coffee_do(coffee)
  cat_next(.index = sim$index, .verbose = verbose)
  
  # Define algorithm inputs
  args     <- constructor(sim = sim, datasets = datasets, verbose = verbose)
  
  # Run algorithm
  error   <- NA_character_
  success <- TRUE
  t1      <- Sys.time()
  pout    <- tryCatch(do.call(algorithm, args), error = function(e) e)
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
  dout <- data.table(id        = sim$index, 
                     algorithm = deparse(substitute(algorithm)), 
                     success   = success, 
                     error     = error, 
                     ntrial    = NA_integer_,
                     time      = time)
  
  # (optional) Write outputs
  write <- rlang::has_name(sim, "folder_output")
  if (write) {
    pfile <- "output.qs"
    dfile <- "callstats.qs"
    if (success) {
      qs::qsave(pout, file.path(sim$folder_output, pfile))
    }
    qs::qsave(dout, file.path(sim$folder_output, dfile))
    return(nothing())
  } else {
    return(list(output = pout, callstats = dout))
  }
  
}