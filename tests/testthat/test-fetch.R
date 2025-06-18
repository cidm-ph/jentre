test_that("uilist lookup returns ID list", {
  replay_requests("efetch_uilist.xml")

  x <- id_list("biosample")
  z <- efetch(x, rettype = "uilist")
  expect_true(is_id_list(z))
  expect_equal(entrez_database(z), entrez_database(x))
  expect_length(z, 20)
})

test_that("fetching full documents works", {
  replay_requests("efetch_full.xml", "efetch_full.xml")

  x <- id_list("biosample", c("45830305", "45830304"))
  y <- efetch(x, rettype = "full")
  expect_equal(xml2::xml_name(y[[1]]), "BioSampleSet")

  y <- efetch(x, rettype = "full", .process = function(doc) {
    xml2::xml_find_all(doc, "//BioSample/Ids/Id[@is_primary='1']") |> xml2::xml_text()
  })
  expect_equal(y, c("SAMN45830305", "SAMN45830304"))
  })