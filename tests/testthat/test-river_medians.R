test_that("the median length is calculated correctly in a test case", {

  # calculate values required for the test for length

  ## calculate the medians from the function
  river_medians_val <- river_medians(river, "Missouri|Mississippi")
  river_medians_length <- river_medians_val[[1]]

  ## calculate the medians "by hand"
  raw_vec_length <- unlist(river["length", c("Missouri", "Mississippi")])
  raw_medians_length <- stats::median(raw_vec_length)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_medians_length, raw_medians_length)
})

test_that("the median discharge is calculated correctly in a test case", {

  # calculate values required for the test for length

  ## calculate the medians from the function
  river_medians_val <- river_medians(river, "Missouri|Mississippi")
  river_medians_length <- river_medians_val[[2]]

  ## calculate the medians "by hand"
  raw_vec_length <- unlist(river["discharge", c("Missouri", "Mississippi")])
  raw_medians_length <- stats::median(raw_vec_length)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_medians_length, raw_medians_length)
})

test_that("the median length is calculated correctly in a test case with one match", {

  # calculate values required for the test for length

  ## calculate the medians from the function
  river_medians_val <- river_medians(river, "Missouri")
  river_medians_length <- river_medians_val[[1]]

  ## calculate the medians "by hand"
  raw_vec_length <- unlist(river["length", c("Missouri")])
  raw_medians_length <- stats::median(raw_vec_length)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_medians_length, raw_medians_length)
})

test_that("the median discharge is calculated correctly in a test case with one match", {

  # calculate values required for the test for length

  ## calculate the medians from the function
  river_medians_val <- river_medians(river, "Missouri")
  river_medians_length <- river_medians_val[[2]]

  ## calculate the medians "by hand"
  raw_vec_length <- unlist(river["discharge", c("Missouri")])
  raw_medians_length <- stats::median(raw_vec_length)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_medians_length, raw_medians_length)
})

test_that("an error is returned in a test case with zero matches", {
  # the function should return an error
  expect_error(river_medians(rivers, "ZZZ"))
})
