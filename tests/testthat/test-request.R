test_that("request has recommended headers and params by default", {
  req <- entrez_request("einfo.fcgi")
  query <- httr2::url_parse(req$url)$query

  expect_match(req$options$useragent, "jentre")
  expect_equal(query$tool, "jentre")
  expect(nchar(query$email) > 0, "email param not set")
  expect_contains(names(query), c("email", "tool"))
})

test_that("default params can be removed", {
  req <- entrez_request("efetch.fcgi", tool = NULL)
  query <- httr2::url_parse(req$url)$query
  expect(! "tool" %in% names(query), "tool param was unexpectedly set")
})

test_that("only mutiple params are put in request body by default", {
  req <- entrez_request("efetch.fcgi", id = 1:10, .method = "POST")
  expect_setequal(names(req$body$data), c("id"))
})

test_that("specific params can be forced into the body", {
  req <- new_request("efetch.fcgi", list(id = 1), .body_params = c("id", "x"), .method = "POST")
  query <- httr2::url_parse(req$url)$query
  expect_equal(req$method, "POST")
  expect_contains(names(req$body$data), "id")
  expect_true(! "id" %in% names(query))
})

test_that("api key can be set with options", {
  withr::with_options(list(jentre.entrez_key = "abcdef123"), {
    expect_equal(entrez_api_key(), "abcdef123")
  })

  withr::with_options(list(jentre.entrez_key = rlang::zap()), {
    expect_equal(entrez_api_key(), NULL)
  })
})