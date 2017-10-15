context("local triad closure")
data(scotland1920s)

test_that("specialized and generalized local measures agree", {
  expect_equal(
    mapply(
      triad_closure,
      measure = c("classical", "twomode", "unconnected", "exclusive"),
      MoreArgs = list(graph = scotland1920s, type = "local", method = "wedges")
    ),
    mapply(
      triad_closure,
      measure = c("classical", "twomode", "unconnected", "exclusive"),
      MoreArgs = list(graph = scotland1920s, type = "local", method = "triads")
    )
  )
})
