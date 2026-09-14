# Autosave writes to one per-user cache directory -- tools::R_user_dir(), the
# same path a real session uses -- so a test run left to itself writes into,
# and clears, the file holding a user's genuinely unrecoverable model. Two
# suites running at once are worse still: each restores the other's
# specification, which surfaces as a handful of failures in test-autosave.R
# that do not reproduce when the same two runs are run one after the other.
#
# Redirecting the cache root is suite-wide rather than local to the autosave
# tests because every file that boots the app server touches that path: the
# server reads the autosave as it initialises and writes one whenever the
# model changes.
#
# tempdir() is per R session, so concurrent runs get different directories.
ctgui_test_cache_dir <- file.path(tempdir(), "ctsemGUI-test-cache")
dir.create(ctgui_test_cache_dir, recursive = TRUE, showWarnings = FALSE)

withr::local_envvar(
  c(R_USER_CACHE_DIR = ctgui_test_cache_dir),
  .local_envir = testthat::teardown_env()
)
withr::defer(
  unlink(ctgui_test_cache_dir, recursive = TRUE),
  envir = testthat::teardown_env()
)
