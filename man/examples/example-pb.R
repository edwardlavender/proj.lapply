# Define loop
loop <- function() {
  n <- 10L
  pb <- pb_init(.min = 0L, .max = n)
  for (i in seq_len(n)) {
    Sys.sleep(0.1)
    pb_tick(pb, .t = i)
  }
  pb_close(pb)
}

# Show default progress bar
loop()

# Customise default progress bar
pbo <- pbapply::pboptions(type = "txt")
loop()

# Suppress progress bar
pbo <- pbapply::pboptions(type = "none")
loop()

# Reset options
pbapply::pboptions(pbo)