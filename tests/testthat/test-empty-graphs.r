context("empty graphs")

g <- make_empty_graph(directed = FALSE)

test_that("empty graphs are not all recognized as ANs", {
  expect_false(
    is_an(g),
    is_dynamic_an(g)
  )
})

g <- as_an(g, add.type.attribute = TRUE)

test_that("empty graphs with 'type' attributes are recognized as ANs", {
  expect_true(
    is_an(g),
    is_dynamic_an(g)
  )
})

test_that("triad census functions can handle empty graphs", {
  expect_equal(
    triad_census(g, add.names = FALSE),
    triad_census_an(g, add.names = FALSE),
    matrix(0L, nrow = 1, ncol = 1)
  )
  expect_equal(
    triad_census_difference(g, add.names = FALSE),
    matrix(0L, nrow = 8, ncol = 2)
  )
  expect_equal(
    triad_census_binary(g, add.names = FALSE),
    matrix(0L, nrow = 4, ncol = 2)
  )
})

test_that("global triad closure functions can handle empty graphs", {
  expect_equal(
    triad_closure_watts_strogatz(g),
    triad_closure_opsahl(g),
    triad_closure_liebig_rao_0(g),
    triad_closure_liebig_rao_3(g),
    triad_closure_exclusive(g),
    NaN
  )
})
