test_that("ID lists are vectors", {
  x <- id_list("biosample", c("A", "B", "C", "D"))
  y <- id_list("biosample", c("E", "F"))

  expect_equal(x[1:2], id_list("biosample", c("A", "B")))
  expect_equal(head(x, n = 2), id_list("biosample", c("A", "B")))
  expect_equal(c(x, y), id_list("biosample", c("A", "B", "C", "D", "E", "F")))
})

test_that("web history caches length", {
  x <- web_history("biosample", "sd7879fdsdf", "1")
  expect_equal(length(x), 1L)
  expect_equal(vctrs::vec_size(x), 1L)
  expect_equal(wh_len_get(x), NA_integer_)

  y <- x
  wh_len_set(x, 100L)
  expect_equal(wh_len_get(x), 100L)
  expect_equal(wh_len_get(y), 100L)
})