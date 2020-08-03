s <- square(1, 1, 5)

test_that("deep squares returns list of squares", {
  i <- 2
  ds <- split_squares_deep(s, i)

  expect_equal(length(ds), 4**(i + 1))
  expect_equal(map_chr(ds, ~class(.x)[1]), rep('square', 4**(i+1)))
})
