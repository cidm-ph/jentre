test_that("parses 1-to-1 link format", {
  doc <- resp_doc_get("elink_1to1.xml") |> xml2::read_xml()

  output <- jentre:::process_xml_LinkSet_df_one_to_one(doc)
  expect_s3_class(output, "data.frame")
  expect_equal(output$id_to, rep(list("1074779"), 9))
})

test_that("parses many-to-many link format", {
  # made with: elink(x[1:10], "bioproject", linkname = "biosample_bioproject", .multi = "comma")
  doc <- resp_doc_get("elink_many_to_many.xml") |> xml2::read_xml()

  output <- jentre:::process_xml_LinkSet_df_many_to_many(doc)
  expect_s3_class(output, "data.frame")
  exp <- c("1256016", "1231393", "1229280", "1222596", "1198110", "967744", "224116")
  expect_equal(output$id_to, list(exp))
})

test_that("parses many-to-many link format with multiple linknames", {
  doc <- resp_doc_get("elink_manyuid_to_manyln.xml") |> xml2::read_xml()

  output <- jentre:::process_xml_LinkSet_df_many_to_many(doc)
  expect_s3_class(output, "data.frame")
  expect_equal(lengths(output$id_to), c(4, 5, 5))
  expect_equal(lengths(output$id_from), c(50, 50, 50))
})

test_that("deleted web histories cause an error", {
  replay_requests("elink_error.xml")

  wh <- web_history("biosample", "DOES-NOT-EXIST", "1")
  expect_error(elink(wh, "bioproject"))
})

test_that("parses web history result link format", {
  doc <- resp_doc_get("elink_wh.xml") |> xml2::read_xml()

  output <- jentre:::process_xml_eLinkResult_sets(doc)
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1L)
  expect_true(is_id_list(output$from[[1]]))
  expect_true(is_web_history(output$to[[1]]))
})

test_that("link from WH works", {
  replay_requests("elink_wh.xml")

  wh <- web_history("biosample", "MCID_68491955830fdfaa740f20ac", "1", length = 4124)
  expect_null(wh_ids_get(wh))

  result <- elink(wh, "sra")

  # the call caches the full ID list
  expect_length(wh_ids_get(wh), 4124L)
})