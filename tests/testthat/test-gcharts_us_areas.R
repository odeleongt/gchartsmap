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
# test GET interface ----
#------------------------------------------------------------------------------*

test_that("expects integers", {
  expect_error(
    gchart_get_us_areas(500),
    regexp = "integer"
  )
})


test_that("expects at least one area for the request", {
  expect_error(
    gchart_get_us_areas(integer(0)),
    regexp = "at least"
  )
})


test_that("respect limit", {
  expect_error(
    gchart_get_us_areas(500L, limit = 0),
    regexp = "fetch more than"
  )
})


test_that("succeeds with known area", {
  expect_equal(
    gchart_get_us_areas(500L),
    file.path(tmpdir, "us-areas", "500.js"),
    ignore_attr = TRUE
  )
})


test_that("succeeds with existing file", {
  expect_equal(
    gchart_get_us_areas(500L),
    file.path(tmpdir, "us-areas", "500.js"),
    ignore_attr = TRUE
  )
  unlink(file.path(tmpdir, "us-areas", "500.js"))
})


test_that("fails with incorrect area", {
  gchart_get_us_areas(-1L)
  path <- gchart_available_areas(type = "us-areas")
  content <- readLines(path[1], n = 1, warn = FALSE)
  expect_equal(
    content,
    "error: 404",
    ignore_attr = TRUE
  )
  unlink(file.path(tmpdir, "us-areas", "-1.js"))
  rm(path, content)
})




#------------------------------------------------------------------------------*
# test geo data processing ----
#------------------------------------------------------------------------------*

gchart_get_us_areas(500L)


test_that("listing available areas needs cache", {
  expect_error(
    gchart_available_areas(cache = "", type = "us-areas"),
    regexp = "Cache should be used"
  )
})


test_that("processing needs cache", {
  expect_error(
    gchart_process_areas(1L, cache = ""),
    regexp = "Cache should be used"
  )
})


test_that("processing expects integers", {
  expect_error(
    gchart_process_areas(500, type = "us-areas"),
    regexp = "integer"
  )
})


test_that("processing expects at least one area for the request", {
  expect_error(
    gchart_process_areas(integer(0)),
    regexp = "at least"
  )
})


test_that("missing areas requested for processing", {
  expect_error(
    gchart_process_areas(-1L),
    regexp = "requested areas should be downloaded"
  )
})


test_that("processes available areas", {
  expect_equal(
    class(gchart_process_areas(areas = 500L, type = "us-areas"))[1],
    "sf"
  )
})



#------------------------------------------------------------------------------*
# test automation ----
#------------------------------------------------------------------------------*

test_that("automated processing expects integers", {
  expect_error(
    gchart_generate_us_areas(500),
    regexp = "integer"
  )
})


test_that("automated processing expects at least one area for the request", {
  expect_error(
    gchart_generate_us_areas(integer(0)),
    regexp = "at least"
  )
})


test_that("automated processing respects limit", {
  expect_error(
    gchart_generate_us_areas(500L, limit = 0),
    regexp = "fetch more than"
  )
})


test_that("automated processing requested areas", {
  expect_equal(
    class(gchart_generate_us_areas(areas = 500L))[1],
    "sf"
  )
})


unlink(file.path(tmpdir, "us-areas", "500.js"))
Sys.setenv(R_GOOGLE_CHART_CACHE = oldpath)
