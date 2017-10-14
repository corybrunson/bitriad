context("global triad closure")
data(scotland1920s)

test_that("direct and census-derived global measures agree", {
  expect_equal(
    mapply(
      triad_closure,
      measure = c("classical", "twomode", "unconnected", "exclusive"),
      MoreArgs = list(graph = scotland1920s, type = "global")
    ),
    mapply(
      triad_closure_from_census,
      measure = c("classical", "twomode", "unconnected", "exclusive"),
      MoreArgs = list(census = triad_census(scotland1920s))
    )
  )
})
