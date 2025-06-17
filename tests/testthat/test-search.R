test_that("warnings are raised if present", {
  doc <- resp_doc_get("esearch_demo.xml") |> xml2::read_xml()
  expect_no_warning(check_xml_eSearchResult(doc))

  doc <- resp_doc_get("esearch_no_results.xml") |> xml2::read_xml()
  expect_warning(check_xml_eSearchResult(doc), "PhraseNotFound")
})
