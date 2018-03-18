context("test-justmapit.R")

test_that("plots work", {
  vdiffr::expect_doppelganger("world", {
    jmi(world)
  })
})
