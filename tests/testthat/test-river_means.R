test_that("the mean length is calculated correctly in a test case", {

  # calculate values required for the test for length

  ## calculate the means from the function
  river_means_val <- river_means(river, "Missouri|Mississippi")
  river_means_length <- river_means_val[[1]]

  ## calculate the means "by hand"
  raw_vec_length <- unlist(river["length", c("Missouri", "Mississippi")])
  raw_means_length <- mean(raw_vec_length)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_means_length, raw_means_length)
})

test_that("the mean discharge is calculated correctly in a test case", {

  # calculate values required for the test for length

  ## calculate the means from the function
  river_means_val <- river_means(river, "Missouri|Mississippi")
  river_means_length <- river_means_val[[2]]

  ## calculate the means "by hand"
  raw_vec_length <- unlist(river["discharge", c("Missouri", "Mississippi")])
  raw_means_length <- mean(raw_vec_length)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_means_length, raw_means_length)
})

test_that("the mean length is calculated correctly in a test case with one match", {

  # calculate values required for the test for length

  ## calculate the means from the function
  river_means_val <- river_means(river, "Missouri")
  river_means_length <- river_means_val[[1]]

  ## calculate the means "by hand"
  raw_vec_length <- unlist(river["length", c("Missouri")])
  raw_means_length <- mean(raw_vec_length)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_means_length, raw_means_length)
})

test_that("the mean discharge is calculated correctly in a test case with one match", {

  # calculate values required for the test for length

  ## calculate the means from the function
  river_means_val <- river_means(river, "Missouri")
  river_means_length <- river_means_val[[2]]

  ## calculate the means "by hand"
  raw_vec_length <- unlist(river["discharge", c("Missouri")])
  raw_means_length <- mean(raw_vec_length)

  # perform the actual test for length

  ## check that the function and "by hand" output matches
  expect_equal(river_means_length, raw_means_length)
})

test_that("an error is returned in a test case with zero matches", {
  # the function should return an error
  expect_error(river_means(rivers, "ZZZ"))
})
