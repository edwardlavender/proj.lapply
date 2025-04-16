library(data.table)

# Define algorithm function
# * This function expects a SpatRaster and runs a complicated routine 
algorithm <- function(x) {
  # For simplicity, we just do something simple here
  # We extract the value of the SpatRaster in a random location
  # This is just a placeholder for a more complicated routine
  # that we'd want to parallelise
  xyz <- terra::spatSample(x, xy = TRUE, size = 1)
  terra::extract(x, cbind(xyz$x, xyz$y))
}

# Define a constructor function
# * This function constructs a _named list_ of arguments for algorithm()
constructor <- function(sim, datasets, verbose, ...) {
  list(x = datasets$x)
}

# Define iteration data table
# * Here we iterate over two units (e.g., individuals) 
iteration <- data.table(unit_id = 1:2)

# Run workflow 
# A) wrap the SpatRaster for parallelisation
w <- 
  "ex/elev.tif" |> 
  system.file(package = "terra") |> 
  terra::rast() |> 
  terra::wrap()
# B) Run workflow (unix)
# * Use a chunk-wise implementation
# * Unwrap the SpatRaster on each chunk
# * Is it incorporated into datasets
cl_lapply_workflow(iteration, 
                   datasets = list(), 
                   constructor = constructor, 
                   algorithm = algorithm, 
                   chunk_fun = function(xi, ...) {
                     # Return a named list
                     list(x = terra::unwrap(w))
                   }, 
                   chunk = TRUE, cl = 2L)

# For extensive examples, see the patter.workflows package. 