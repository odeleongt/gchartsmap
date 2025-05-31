
tmpdir <- gsub("[\\]+", "/", tempdir())
unlink(file.path(tmpdir, ".Renviron"))
# save original value
oldpath <- Sys.getenv("R_GOOGLE_CHART_CACHE")
Sys.unsetenv("R_GOOGLE_CHART_CACHE")

test_that("finds the path existing root", {
  expect_equal(
    path_existing_root(tmpdir),
    tmpdir
  )
})


test_that("ignores non-existing dirs", {
  expect_equal(
    path_existing_root(file.path(tmpdir, "non/existent/path")),
    tmpdir
  )
})


test_that("finds home", {
  expect_equal(
    path_existing_root(path = "HOME"),
    gsub("[\\]+", "/", Sys.getenv("HOME"))
  )
})


test_that("finds writeable home", {
  expect_equal(
    gchart_cache_dir(path = "HOME"),
    gsub("[\\]+", "/", Sys.getenv("HOME"))
  )
})


test_that("creates a new path", {
  if(
    expect_true(
      file.exists(gchart_cache_dir(file.path(tmpdir, "gchart/test/path")))
    )
  ){
    # remove the test path
    unlink(file.path(tmpdir, "gchart"), recursive = TRUE)
  }
})


test_that("sets the path in environment variables", {
  expect_equal(
    suppressMessages(gchart_set_cache(path = tmpdir)),
    Sys.getenv("R_GOOGLE_CHART_CACHE")
  )
  Sys.unsetenv("R_GOOGLE_CHART_CACHE")
})


test_that("installs the path", {
  expect_true(
    {
      suppressMessages(
        gchart_set_cache(path = tmpdir, install = TRUE, home = tmpdir)
      )
      grepl("R_GOOGLE_CHART_CACHE", readLines(file.path(tmpdir, ".Renviron")))
    }
  )
  unlink(file.path(tmpdir, ".Renviron"))
})


test_that("warning if path already installed", {
  expect_warning(
    {
      suppressMessages(
        gchart_set_cache(path = tmpdir, install = TRUE, home = tmpdir)
      )
      suppressMessages(
        gchart_set_cache(path = tmpdir, install = TRUE, home = tmpdir)
      )
    }
  )
  unlink(file.path(tmpdir, ".Renviron"))
})


test_that("removes the path", {

  suppressMessages(gchart_set_cache(path = tmpdir))

  gchart_remove_cache_path(remove = TRUE)

  expect_equal(
    Sys.getenv("R_GOOGLE_CHART_CACHE"),
    ""
  )
})


test_that("removes installed path", {
  expect_true(
    {
      suppressMessages(
        gchart_set_cache(path = tmpdir, install = TRUE, home = tmpdir)
      )

      gchart_remove_cache_path(remove = TRUE, path = tmpdir)

      found <- grepl(
        "R_GOOGLE_CHART_CACHE", readLines(file.path(tmpdir, ".Renviron"))
      )

      !found || length(found) == 0
    }
  )
  unlink(file.path(tmpdir, ".Renviron"))
})


test_that("overwrites the installed path", {
  expect_true(
    {
      suppressMessages(
        gchart_set_cache(path = tmpdir, install = TRUE, home = tmpdir)
      )
      suppressMessages(
        gchart_set_cache(
          path = tmpdir, install = TRUE, overwrite = TRUE, home = tmpdir
        )
      )

      grepl("R_GOOGLE_CHART_CACHE", readLines(file.path(tmpdir, ".Renviron")))
    }
  )
  unlink(file.path(tmpdir, ".Renviron"))
})


test_that("gets the saved path", {
  expect_equal(
    gchart_get_cache_path(),
    Sys.getenv("R_GOOGLE_CHART_CACHE")
  )
})


test_that("returns the valid provided path", {
  expect_equal(
    gchart_get_cache_path(path = tmpdir),
    tmpdir
  )
})




# restore original value
Sys.setenv(R_GOOGLE_CHART_CACHE = oldpath)
