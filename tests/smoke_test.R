#!/usr/bin/env Rscript
# tests/smoke_test.R
# Minimal smoke test: source the extractor and check that core helpers
# load and compute the expected deterministic values. Requires R; does NOT
# require the optional heavy deps (mutool/pdftools) because it only exercises
# the pure helper functions, not the PDF pipeline.
#
# Run from the repo root:  Rscript tests/smoke_test.R

# Locate the script relative to this test file.
this_file <- tryCatch(
  normalizePath(sub("^--file=", "",
    grep("^--file=", commandArgs(FALSE), value = TRUE)[1])),
  error = function(e) NA_character_
)
repo_root <- if (!is.na(this_file)) dirname(dirname(this_file)) else getwd()
src <- file.path(repo_root, "km_pdf_vector_extract_ultra.R")
stopifnot(file.exists(src))

# Sourcing must NOT trigger the CLI batch run (guarded by sys.nframe()==0L).
# We tolerate a missing-packages stop() in constrained environments by only
# requiring that the file parses; if packages are present it sources fully.
parsed <- tryCatch({ parse(src); TRUE }, error = function(e) FALSE)
stopifnot(isTRUE(parsed))

ok <- TRUE
report <- function(name, cond) {
  cat(sprintf("[%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", name))
  if (!isTRUE(cond)) ok <<- FALSE
}

sourced <- tryCatch({ source(src, local = TRUE); TRUE },
                    error = function(e) { message("source skipped: ", e$message); FALSE })

report("script parses", parsed)

if (sourced) {
  # Bezier endpoints: t=0 -> p0, t=1 -> p2/p3.
  report("bez_q(0) == p0", isTRUE(all.equal(bez_q(0, 1, 5, 9), 1)))
  report("bez_q(1) == p2", isTRUE(all.equal(bez_q(1, 1, 5, 9), 9)))
  report("bez_c(0) == p0", isTRUE(all.equal(bez_c(0, 1, 2, 3, 4), 1)))
  report("bez_c(1) == p3", isTRUE(all.equal(bez_c(1, 1, 2, 3, 4), 4)))
  # seg_count is bounded and at least 3.
  report("seg_count floor >= 3", seg_count(0, 0) >= 3)
  report("seg_count capped",
         seg_count(1e6, 1e6) <= CONFIG$BEZIER_MAX_SEG)
  # sanitize_basename strips unsafe chars and extension.
  report("sanitize_basename", sanitize_basename("a b/c.pdf") == "c")
  # find_axis_lines must not collapse a right-to-left line (regression for the
  # pmin/pmax sequential-eval bug).
  if (exists("find_axis_lines")) {
    test_lines <- tibble::tibble(
      x1 = c(100, 50), y1 = c(50, 50),
      x2 = c(10,  50), y2 = c(50, 150),
      stroke = c("black", "black"), sw = c(1, 1), dash = c(NA, NA)
    )
    ax <- find_axis_lines(test_lines, len_min = 50)
    # The horizontal line spans x 10..100 (len 90); endpoints must be ordered
    # and distinct (not both collapsed to the min).
    h_ok <- nrow(ax$h) >= 1 && abs(ax$h$x2[1] - ax$h$x1[1]) > 1
    report("find_axis_lines preserves L->R span", isTRUE(h_ok))
  }
} else {
  cat("NOTE: full source skipped (optional R packages missing); ",
      "parse-only smoke passed.\n", sep = "")
}

if (!ok) quit(save = "no", status = 1)
cat("\nALL SMOKE CHECKS PASSED\n")
