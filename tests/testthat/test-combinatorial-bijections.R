context("combinatorial-bijections")

test_that("zeros are handled properly", {
  expect_equal(index_subset(0), c(2, 1, 0))
  expect_equal(subset_index(c(2, 1, 0)), 0)
  expect_equal(subset_partition(c(2, 1, 0)), c(0, 0, 0))
  expect_equal(partition_subset(c(0, 0, 0)), c(2, 1, 0))
})

test_that("nonsense inputs generate errors", {
  expect_error(index_subset(-1))
  expect_error(subset_index(c(1, 1, 0)))
  expect_error(subset_index(c(1, 0, -1)))
  expect_error(subset_partition(c(3, 3, 1)))
  expect_error(subset_partition(c(1, 0, -1)))
  expect_error(partition_subset(c(1, 2, 1)))
  expect_error(partition_subset(c(0, 0, -1)))
})
