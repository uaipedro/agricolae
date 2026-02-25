data(sweetpotato)
model_sp <- aov(yield ~ virus, data = sweetpotato)
tuk      <- HSD.test(model_sp,    "virus", group = TRUE, console = FALSE)
wal      <- waller.test(model_sp, "virus",               console = FALSE)
sch      <- scheffe.test(model_sp, "virus",              console = FALSE)

# Helper: run plot on a null device and return par("usr")
plot_usr <- function(...) {
  pdf(NULL)
  on.exit(dev.off())
  plot(...)
  par("usr")
}

# ── Acceptance ────────────────────────────────────────────────────────────────

test_that("new variation options are accepted without error", {
  expect_no_error(plot_usr(tuk, variation = "HSD"))
  expect_no_error(plot_usr(wal, variation = "Waller"))
  expect_no_error(plot_usr(sch, variation = "Scheffe"))
})

test_that("invalid variation throws error", {
  expect_error(plot_usr(tuk, variation = "bogus"))
})

# ── colores fix ───────────────────────────────────────────────────────────────

test_that("colores produces no NAs for mixed group labels", {
  # sweetpotato groups include "ab", "bc" which triggered the regression
  groups  <- tuk$groups[, 2]
  colores <- as.factor(groups)
  colores <- as.numeric(colores)
  expect_false(any(is.na(colores)))
  expect_true(all(colores >= 1L))
})

# ── Correct y-limits per variation ───────────────────────────────────────────
# plot.group sets ylim = c(0.8 * min(nivel0), 1.2 * max(nivel1)).
# Passing yaxs = "i" to barplot suppresses the default 4% axis expansion,
# so par("usr")[3:4] should equal the expected limits exactly.

test_that("variation='HSD' y-limits reflect MSD/2", {
  z    <- tuk$means[rownames(tuk$groups), ]
  y    <- z[, 1]
  semi <- tuk$statistics$MSD / 2

  expected <- c(0.8 * min(y - semi), 1.2 * max(y + semi))
  usr      <- plot_usr(tuk, variation = "HSD", yaxs = "i")

  expect_equal(usr[3], expected[1], tolerance = 1e-6)
  expect_equal(usr[4], expected[2], tolerance = 1e-6)
})

test_that("variation='Waller' y-limits reflect CriticalDifference/2", {
  z    <- wal$means[rownames(wal$groups), ]
  y    <- z[, 1]
  semi <- wal$statistics$CriticalDifference / 2

  expected <- c(0.8 * min(y - semi), 1.2 * max(y + semi))
  usr      <- plot_usr(wal, variation = "Waller", yaxs = "i")

  expect_equal(usr[3], expected[1], tolerance = 1e-6)
  expect_equal(usr[4], expected[2], tolerance = 1e-6)
})

test_that("variation='Scheffe' y-limits reflect CriticalDifference/2", {
  z    <- sch$means[rownames(sch$groups), ]
  y    <- z[, 1]
  semi <- sch$statistics$CriticalDifference / 2

  expected <- c(0.8 * min(y - semi), 1.2 * max(y + semi))
  usr      <- plot_usr(sch, variation = "Scheffe", yaxs = "i")

  expect_equal(usr[3], expected[1], tolerance = 1e-6)
  expect_equal(usr[4], expected[2], tolerance = 1e-6)
})

# ── Fallback ──────────────────────────────────────────────────────────────────

test_that("HSD fallback warns when MSD is absent", {
  tuk2 <- tuk
  tuk2$statistics$MSD <- NULL
  expect_warning(plot_usr(tuk2, variation = "HSD"), "MSD not found")
})

test_that("Waller fallback warns when CriticalDifference is absent", {
  wal2 <- wal
  wal2$statistics$CriticalDifference <- NULL
  expect_warning(plot_usr(wal2, variation = "Waller"), "CriticalDifference not found")
})

test_that("Scheffe fallback warns when CriticalDifference is absent", {
  sch2 <- sch
  sch2$statistics$CriticalDifference <- NULL
  expect_warning(plot_usr(sch2, variation = "Scheffe"), "CriticalDifference not found")
})
