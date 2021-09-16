test_that("the min length is calculated correctly in a test case", {

  # calculate values required for the test for length

  ## calculate the mins from the function
  river_mins_val <- river_stat(river, "Missouri|Mississippi", min)
  river_mins_length <- river_mins_val[[1]]

  ## calculate the mins "by hand"
  raw_vec_length <- unlist(river["length", c("Missouri", "Mississippi")])
  raw_mins_length <- min(raw_vec_length)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_mins_length, raw_mins_length)
})

test_that("the min discharge is calculated correctly in a test case", {

  # calculate values required for the test for length

  ## calculate the mins from the function
  river_mins_val <- river_stat(river, "Missouri|Mississippi", min)
  river_mins_length <- river_mins_val[[2]]

  ## calculate the mins "by hand"
  raw_vec_length <- unlist(river["discharge", c("Missouri", "Mississippi")])
  raw_mins_length <- min(raw_vec_length)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_mins_length, raw_mins_length)
})

test_that("the max length is calculated correctly in a test case", {

  # calculate values required for the test for length

  ## calculate the maxs from the function
  river_maxs_val <- river_stat(river, "Missouri|Mississippi", max)
  river_maxs_length <- river_maxs_val[[1]]

  ## calculate the maxs "by hand"
  raw_vec_length <- unlist(river["length", c("Missouri", "Mississippi")])
  raw_maxs_length <- max(raw_vec_length)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_maxs_length, raw_maxs_length)
})

test_that("the max discharge is calculated correctly in a test case", {

  # calculate values required for the test for length

  ## calculate the maxs from the function
  river_maxs_val <- river_stat(river, "Missouri|Mississippi", max)
  river_maxs_length <- river_maxs_val[[2]]

  ## calculate the maxs "by hand"
  raw_vec_length <- unlist(river["discharge", c("Missouri", "Mississippi")])
  raw_maxs_length <- max(raw_vec_length)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_maxs_length, raw_maxs_length)
})

test_that("the trimmed mean length is calculated correctly in a test case", {

  # calculate values required for the test for length

  ## calculate the means from the function
  river_means_val <- river_stat(river, "Missouri|Mississippi", mean, trim = 0.5)
  river_means_length <- river_means_val[[1]]

  ## calculate the means "by hand"
  raw_vec_length <- unlist(river["length", c("Missouri", "Mississippi")])
  raw_means_length <- mean(raw_vec_length, trim = 0.5)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_means_length, raw_means_length)
})

test_that("the trimmed mean discharge is calculated correctly in a test case", {

  # calculate values required for the test for length

  ## calculate the means from the function
  river_means_val <- river_stat(river, "Missouri|Mississippi", mean, trim = 0.5)
  river_means_length <- river_means_val[[2]]

  ## calculate the means "by hand"
  raw_vec_length <- unlist(river["discharge", c("Missouri", "Mississippi")])
  raw_means_length <- mean(raw_vec_length, trim = 0.5)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_means_length, raw_means_length)
})

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

test_that("the minimum length is calculated correctly in a test case with one match", {

  # calculate values required for the test for length

  ## calculate the medians from the function
  river_mins_val <- river_stat(river, "Missouri", min)
  river_mins_length <- river_mins_val[[1]]

  ## calculate the medians "by hand"
  raw_vec_length <- unlist(river["length", c("Missouri")])
  raw_mins_length <- min(raw_vec_length)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_mins_length, raw_mins_length)
})

test_that("the minimum length is calculated correctly in a test case with one match", {

  # calculate values required for the test for length

  ## calculate the medians from the function
  river_mins_val <- river_stat(river, "Missouri", min)
  river_mins_length <- river_mins_val[[2]]

  ## calculate the medians "by hand"
  raw_vec_length <- unlist(river["discharge", c("Missouri")])
  raw_mins_length <- min(raw_vec_length)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_mins_length, raw_mins_length)
})

test_that("an error is returned in a test case with zero matches", {
  # the function should return an error
  expect_error(river_stat(rivers, "ZZZ", min))
})
