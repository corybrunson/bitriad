context("censuses")
data(women_group)

test_that("full censuses agree", {
  all.equal(
    triad_census(women_group, method = "proj"),
    triad_census(women_group)
  )
})

test_that("difference censuses agree", {
  all.equal(
    triad_census_difference(women_group, method = "proj"),
    triad_census_difference(women_group),
    project_census(triad_census(women_group))$difference
  )
})

test_that("binary censuses agree", {
  all.equal(
    triad_census_binary(women_group, method = "proj"),
    triad_census_binary(women_group),
    project_census(triad_census(women_group))$binary,
    project_census(triad_census_difference(women_group))$binary
  )
})
