# Autosave writes to one per-user cache directory -- tools::R_user_dir(), the
# same path a real session uses -- so a test run left to itself writes into,
# and clears, the file holding a user's genuinely unrecoverable model. Two
# suites running at once are worse still: each restores the other's
# specification, which surfaces as a handful of failures in test-autosave.R
# that do not reproduce when the same two runs are run one after the other.
#
# The redirection is suite-wide rather than local to the autosave tests
# because every file that boots the app server touches that path: the server
# reads the autosave as it initialises and writes one whenever the model
# changes.
#
# Only the autosave moves. Redirecting R_USER_CACHE_DIR, as this did, moved
# ctsem's cache with it, which holds the Julia engine's project directory:
# every test run then saw a new engine path and precompiled the engine from
# scratch, about nine minutes, taking one of Julia's few image slots for it
# and evicting an image a real session was using.
#
# tempdir() is per R session, so concurrent runs get different directories.
ctgui_test_cache_dir <- file.path(tempdir(), "ctsemGUI-test-cache")
dir.create(ctgui_test_cache_dir, recursive = TRUE, showWarnings = FALSE)

withr::local_options(
  list(ctsemgui.autosave.dir = ctgui_test_cache_dir),
  .local_envir = testthat::teardown_env()
)
withr::defer(
  unlink(ctgui_test_cache_dir, recursive = TRUE),
  envir = testthat::teardown_env()
)
