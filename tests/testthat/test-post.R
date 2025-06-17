test_that("epost converts ID list to web history result", {
  replay_requests("epost_10.xml")
  x <- id_list("biosample", c(
    "48187292", "48187291", "48187290", "48187289", "48187288",
    "48187287", "48187286", "48187285", "48187284", "48187283"
  ))

  y <- epost(x)

  expect_equal(entrez_database(y), entrez_database(x))
  expect_true(is_web_history(y))
  expect_equal(wh_len_get(y), length(x))
})
