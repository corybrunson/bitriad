context("censuses")
data(women_group)

test_that("full censuses agree", {
  expect_equal(
    triad_census(women_group, method = "proj"),
    triad_census_full(women_group)
  )
})

test_that("difference censuses agree", {
  expect_equal(
    triad_census_difference(women_group, method = "proj"),
    triad_census_difference(women_group),
    project_census(triad_census(women_group))$difference
  )
})

test_that("binary censuses agree", {
  expect_equal(
    triad_census_binary(women_group, method = "proj"),
    triad_census_binary(women_group),
    project_census(triad_census(women_group))$binary,
    project_census(triad_census_difference(women_group))$binary
  )
})

test_that("simple censuses agree", {
  expect_equal(
    simple_triad_census(women_group),
    triad_census(actor_projection(women_group))
  )
})
