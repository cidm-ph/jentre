test_that("parses 1-to-1 link format", {
  doc <- resp_doc_get("elink_1to1.xml") |> xml2::read_xml()

  output <- jentre:::process_xml_LinkSet_df_one_to_one(doc)
  expect_s3_class(output, "data.frame")
  expect_equal(output$id_to, rep(list("1074779"), 9))
})

test_that("parses many-to-many link format with multiple linknames", {
  doc <- resp_doc_get("elink_manyuid_to_manyln.xml") |> xml2::read_xml()

  output <- jentre:::process_xml_LinkSet_df_many_to_many(doc)
  expect_s3_class(output, "data.frame")
  expect_equal(purrr::map_int(output$id_to, length), c(4, 5, 5))
  expect_equal(purrr::map_int(output$id_from, length), c(50, 50, 50))
})