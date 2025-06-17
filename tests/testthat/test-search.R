test_that("warnings are raised if present", {
  doc <- resp_doc_get("esearch_demo.xml") |> xml2::read_xml()
  expect_no_condition(check_xml_eSearchResult(doc), message = "PhraseNotFound")

  doc <- resp_doc_get("esearch_no_results.xml") |> xml2::read_xml()
  expect_condition(check_xml_eSearchResult(doc), regexp = "PhraseNotFound")
})

test_that("search to ID list works", {
  replay_requests("esearch_demo.xml")

  x <- esearch("biomol+trna[prop]", "nucleotide", retmax = 20)
  expect_true(is_id_list(x))
  expect_length(x, 20L)
})

test_that("search to history server works", {
  replay_requests("esearch_web_history.xml")

  x <- esearch("Corynebacterium diphtheriae[orgn]", "biosample")
  expect_true(is_web_history(x))
  expect_false(is.na(wh_len_get(x)))
})