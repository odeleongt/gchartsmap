#------------------------------------------------------------------------------*
# test chart data functions ----
#------------------------------------------------------------------------------*


# set up the tests ----

tmpdir <- gsub("[\\]+", "/", tempdir())
# save original value
oldpath <- Sys.getenv("R_GOOGLE_CHART_CACHE")
Sys.unsetenv("R_GOOGLE_CHART_CACHE")


# use a temporary cache
Sys.setenv(R_GOOGLE_CHART_CACHE = tmpdir)


#------------------------------------------------------------------------------*
# test global data processing ----
#------------------------------------------------------------------------------*

test_that("generates requested countries", {
  expect_equal(
    class(gchart_generate_countries("GT", verbose = FALSE))[1],
    "sf"
  )
})


unlink(file.path(tmpdir, "countries", c("GT.js", "US.js")))
Sys.setenv(R_GOOGLE_CHART_CACHE = oldpath)
