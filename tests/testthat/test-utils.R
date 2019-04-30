test_that("make_field works", {
  expect_equal(make_field(TRUE), 1L)
  expect_equal(make_field(FALSE), 0L)
  expect_equal(make_field(NA), "")
  expect_equal(make_field(NULL), "")
  expect_equal(make_field(105L), "105")
  expect_equal(make_field(105.25), "105.25")
})

test_that("counter works", {
  msg_counter <- counter()
  expect_equal(msg_counter(), 1L)
  expect_equal(msg_counter(), 2L)

  # reset counter
  msg_counter <- counter()
  expect_equal(msg_counter(), 1L)
  expect_equal(msg_counter(), 2L)
})

test_that("decoder works",{
  values <- c("1", "0", "test value", "10", "15.5", "")
  # boolean
  expect_identical(decoder("bool", values[1]), TRUE)
  expect_identical(decoder("bool", values[2]), FALSE)
  # character
  expect_identical(decoder("character", values[3]), "test value")
  # integer
  expect_identical(decoder("integer", values[4]), 10L)
  # numeric
  expect_identical(decoder("numeric", values[5]), 15.5)
  # NA_integer_
  expect_identical(decoder("integer", values[6]), NA_integer_)
  # NA_real_
  expect_identical(decoder("numeric", values[6]), NA_real_)
})
