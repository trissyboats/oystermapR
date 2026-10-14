# Two synthetic passes over one 40 x 40 grid: `depth_m` is a shared field plus
# small noise, `turbidity` is independent noise on each pass.
make_survey <- function(seed = 1, sd = 0.2, lines = c("A", "B")) {
  set.seed(seed)
  g <- expand.grid(lon = seq(0, 0.01, length.out = 40),
                   lat = seq(50, 50.01, length.out = 40))
  truth <- sin(g$lon * 900) + cos(g$lat * 700)
  do.call(rbind, lapply(lines, function(id) {
    transform(g, line_id = id,
              depth_m   = truth + rnorm(nrow(g), 0, sd),
              turbidity = rnorm(nrow(g)))
  }))
}

test_that("a reproducible layer scores high and noise scores near zero", {
  res <- suppressMessages(layer_reproducibility(make_survey(), cell_size_m = 5))
  expect_s3_class(res, "layer_reproducibility")
  expect_gt(res$reliability[["depth"]], 0.9)
  expect_lt(abs(res$reliability[["turbidity"]]), 0.25)
  expect_true(res$table$established[res$table$layer == "depth"])
})

test_that("layers are named as the scored layer, not the survey column", {
  res <- suppressMessages(layer_reproducibility(make_survey(), cell_size_m = 5))
  expect_true("depth" %in% names(res$reliability))
  expect_false("depth_m" %in% names(res$reliability))
  expect_equal(res$table$column[res$table$layer == "depth"], "depth_m")
})

test_that("result feeds straight into evidence weighting", {
  res <- suppressMessages(layer_reproducibility(make_survey(), cell_size_m = 5))
  expect_true(is.numeric(res$reliability))
  expect_false(is.null(names(res$reliability)))
})

test_that("too few overlapping cells returns NA, not a made-up number", {
  s <- make_survey()
  s$lat[s$line_id == "B"] <- s$lat[s$line_id == "B"] + 1   # no shared ground
  res <- suppressMessages(layer_reproducibility(s, cell_size_m = 5))
  expect_true(all(is.na(res$reliability)))
  expect_false(any(res$table$established))
  expect_match(res$table$note[1], "too few")
})

test_that("a single line is an error", {
  expect_error(layer_reproducibility(make_survey(lines = "A"), cell_size_m = 5),
               "at least two")
})

test_that("a missing line column is an error that names the options", {
  s <- make_survey(); s$line_id <- NULL
  expect_error(layer_reproducibility(s, cell_size_m = 5), "line or pass column")
})

test_that("line column synonyms and explicit line_col work", {
  s <- make_survey(); names(s)[names(s) == "line_id"] <- "Transect"
  expect_s3_class(suppressMessages(layer_reproducibility(s, cell_size_m = 5)),
                  "layer_reproducibility")
  names(s)[names(s) == "Transect"] <- "sweep"
  expect_s3_class(
    suppressMessages(layer_reproducibility(s, line_col = "sweep", cell_size_m = 5)),
    "layer_reproducibility")
})

test_that("automatic cell size is used when none is given", {
  res <- suppressMessages(layer_reproducibility(make_survey()))
  expect_true(is.finite(res$cell_size_m) && res$cell_size_m >= 1)
  expect_gt(res$reliability[["depth"]], 0.8)
})

test_that("duplicate layer mapping is rejected", {
  s <- make_survey(); s$depth <- s$depth_m
  expect_error(layer_reproducibility(s, layers = c("depth", "depth_m"),
                                     cell_size_m = 5), "same layer")
})
