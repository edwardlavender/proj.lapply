#' @title Iterative workflows
#' @description This function iteratively implements a [`workflow`] based on a user-defined .algorithm function.
#' @param .iteration An .iteration [`data.table::data.table`]. 
#' * Each row corresponds to one .algorithm run and is passed to `.constructor()`.
#' * Required columns depend on the `.constructor()` function but generally include:
#'    - `index`: a unique identifier for each row;
#'    - `unit_id`:  a unique identifier for each unit (e.g., individual/time block);
#'    -  run-specific parameters;
#'    - `file_output` (optional): a `character` path to an output file;
#' @param .datasets (optional) A named `list` of .datasets, passed to `.constructor()`. 
#' @param .constructor,... A `.constructor` `function` that constructs a named `list` of arguments for the `.algorithm` function, given the `.iteration` row, `.datasets` and `.verbose`. Additional arguments in `...` are passed to `.constructor`.
#' @param .algorithm A `function` applied to each row of `.iteration`, via [`workflow`].
#' @param .write A `function` used to create `.iteration$file_output`, if specified. This defaults to [`qs::qsave()`]. For pointers (e.g., [`terra::SpatRaster`]s), use specialised methods (e.g., [`terra::writeRaster()`]).
#' @param .coffee [`coffee()`] break options. 
#' * Use `NULL` to suppress .coffee breaks;
#' * Use `list()` for default arguments;
#' * Use a named `list` of arguments, passed to [`coffee()`], to customise coffee breaks;
#' @param .cl,.varlist,.envir,.chunk,.chunk_fun Cluster arguments passed to [`cl_lapply()`]. If `.chunk_fun` is used:
#' * `.chunk_fun` should return a named `list`;
#' * The outputs of `.chunk_fun` are added as named elements to `.datasets`;
#' * The `.constructor` uses `.datasets`, plus `sim = iteration[i, ]`, `...` and `.verbose`, to return a named `list` that is evaluated via `.algorithm`;
#' @param .config (optional) A `function(.cl = cl_cores(.cl))` that controls additional node configuration options. For example, in code that uses `JuliaCall`, it is necessary to setup `Julia` on each socket. 
#' @param .verbose A `logical` variable or a `string` that defines the path to a text file:
#' * `.verbose = FALSE` suppresses user outputs;
#' * `.verbose = TRUE` sends user outputs to the console;
#' * `.verbose = file.path("path", "to", "text", "file.txt")` sends user outputs to a `.txt` file;
#' 
#' @details
#' [`cl_lapply_workflow()`] iterates over rows in `.iteration` via [`cl_lapply()`] and applies the internal function [`workflow()`]. [`workflow()`] accepts the `.iteration` row, `.datasets`, `.constructor`, `.algorithm` and `.verbose` arguments. Using these inputs, [`workflow()`]:
#' * Constructs a named `list` of arguments for an .algorithm function, via a `.constructor`, given information in the supplied arguments;
#' * Passes the arguments to the `.algorithm` function;
#' * Implements error handling;
#' * Records outputs and call statistics (such as computation time);
#' * Optionally writes outputs to disk or returns them in memory;
#' 
#' @return The function returns a `list`, with one element for each `.iteration` row. `List` elements are controlled by [`workflow()`]:

#' * By default, [`workflow()`] returns a named `list` with two elements:
#'    * `output`: The object returned by the `.algorithm()` function;
#'    * `callstats`: A one-row [`data.table::data.table`] of call statistics with the following columns:
#'        * `id`: An integer that defines the row index in `.iteration` (`.iteration$index`);
#'        * `.algorithm`: A `character` label for the .algorithm, defined by `deparse(substitute(.algorithm))`;
#'        * `success`: A `logical` variable that defines whether or not `.algorithm` ran successfully (without errors);
#'        * `error`: A `character` that defines error messages or `NA_character_` otherwise;
#'        * `ntrial`: An `integer` that defines the number of trials, if relevant; 
#'        * `time`: A `double` that defines the time (s) of the `.algorithm` run;
#'        
#' * If `.iteration` contains a `file_output` column, [`workflow()`] returns `invisible(NULL)`. Outputs are instead written to file (reducing memory demand), as:
#'    * `file_output`
#'    * `file.path(dirname(file_output), "callstats.qs")`
#'
#' @example man/examples/example-cl_lapply_workflow.R
#' @author Edward Lavender
#' @name cl_lapply_workflow

#' @rdname cl_lapply_workflow
#' @export

cl_lapply_workflow <- function(.iteration,
                               .datasets, 
                               .constructor, ...,
                               .algorithm, 
                               .write = qs::qsave,
                               .coffee = NULL,
                               .cl = NULL, .varlist = NULL, .envir = .GlobalEnv,
                               .chunk = FALSE, .chunk_fun = NULL, .config = NULL,
                               .verbose = TRUE) {
  
  #### User output control
  # A. Open a sink for use with cat() in downstream functions (convenient)
  # B. Use cat_setup() to provide a startup message (for convenience)
  # C. Set .verbose to TRUE or FALSE and make available to downstream functions
  t1       <- Sys.time()
  log.txt <- NULL
  if (inherits(.verbose, "character")) {
    log.txt <- sink_open(log.txt = .verbose)
    .verbose <- TRUE
  }
  cats    <- cat_setup(.fun = "cl_lapply_workflow", .verbose = .verbose)
  on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)
  on.exit(sink_close(log.txt), add = TRUE)
  
  #### Process .iteration
  # Define index
  .iteration <- copy(.iteration)
  if (!rlang::has_name(.iteration, "index")) {
    index <- NULL
    .iteration[, index := 1:.N]
  }
  # Validate columns
  if (rlang::has_name(.iteration, "file_output")) {
    if (!all(dir.exists(dirname(.iteration$file_output)))) {
      abort("All `dirname(.iteration$file_output)` directories should exist.")
    }
  }
  
  #### Iteratively implement a workflow
  iteration_ls <- split(.iteration, seq_len(nrow(.iteration)))
  out <- cl_lapply(.x = iteration_ls, 
                   .fun = function(.sim, .chunkargs = NULL) {
                     # Parallel configuration
                     if (!is.null(.config)) {
                       .config(.cl = cl_cores(.cl))
                     }
                     # Handle .chunkargs
                     if (any(length(.datasets) > 0L & !is.null(.chunkargs))) {
                       .datasets <- list_merge(.datasets, .chunkargs)
                     }
                     # Run workflow 
                     workflow(.sim         = .sim, 
                              .datasets    = .datasets, 
                              .constructor = .constructor, ..., 
                              .algorithm   = .algorithm, 
                              .write       = .write,
                              .coffee      = .coffee,
                              .verbose     = .verbose)
                   }, 
                   .cl = .cl, .varlist = .varlist, .envir = .envir, 
                   .chunk = .chunk, .chunk_fun = .chunk_fun)
  
  #### Clean up coffeehouse
  coffee_cleanup(.coffee)
  
  #### Return outputs
  out
  
}
