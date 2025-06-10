test_that("request has recommended headers and params by default", {
  req <- entrez_request("einfo.fcgi")
  query <- httr2::url_parse(req$url)$query

  expect_match(req$options$useragent, "jentre")
  expect_equal(query$tool, "jentre")
  expect(nchar(query$email) > 0, "email param not set")
})

test_that("only mutiple params are put in request body", {
  req <- entrez_request("efetch.fcgi", id = 1:10, .method = "POST")
  expect_setequal(names(req$body$data), c("id"))
})
