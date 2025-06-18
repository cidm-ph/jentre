test_that("ID list batching works", {
  expect_equal(split_id_list(1:10, 20), list(1:10))
  expect_equal(split_id_list(1:10, 5), list(1:5, 6:10))
  expect_equal(split_id_list(1, 5), list(1))
  expect_equal(split_id_list(character(), 5), list(character()))
  expect_equal(split_id_list(1:5, 1), list(1, 2, 3, 4, 5))
  expect_error(split_id_list(1:5, 0), regexp = "max_per_batch")
})
