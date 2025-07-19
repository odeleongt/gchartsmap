#------------------------------------------------------------------------------*
# test chart data functions ----
#------------------------------------------------------------------------------*


# set up the tests ----

tmpdir <- gsub("[\\]+", "/", tempdir())
# save original value
oldpath <- Sys.getenv("R_GOOGLE_CHART_CACHE")
Sys.unsetenv("R_GOOGLE_CHART_CACHE")

test_that("error if no cache set", {
  expect_error(
    gchart_get_us_areas(500L),
    regexp = "cache"
  )
})


# use a temporary cache
Sys.setenv(R_GOOGLE_CHART_CACHE = tmpdir)


#------------------------------------------------------------------------------*
# test general geo processing ----
#------------------------------------------------------------------------------*


test_that("succeeds with known country", {
  expect_equal(
    gchart_get_countries("GT"),
    file.path(tmpdir, "countries", "GT.js"),
    ignore_attr = TRUE
  )
})

test_that("processes available countries", {
  expect_equal(
    class(gchart_process_areas(areas = "GT", type = "countries"))[1],
    "sf"
  )
})

test_that("processes US data", {
  gchart_get_countries("US")
  expect_equal(
    class(gchart_process_areas(areas = "US", type = "countries"))[1],
    "sf"
  )
})


unlink(file.path(tmpdir, "countries", c("GT.js", "US.js")))
Sys.setenv(R_GOOGLE_CHART_CACHE = oldpath)
