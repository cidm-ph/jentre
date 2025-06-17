resp_doc_get <- function(path) {
  path <- test_path("../responses", path)
  readBin(path, "raw", file.size(path))
}

resp_get <- function(path, status_code = 200, content_type = "application/xml") {
  httr2::response(
    status_code = status_code,
    headers = paste0("Content-Type: ", content_type),
    body = resp_doc_get(path)
  )
}

replay_requests <- function(..., env = rlang::caller_env()) {
  rlang::check_dots_unnamed()
  paths <- rlang::list2(...)
  resps <- Map(resp_get, paths)
  httr2::local_mocked_responses(resps, env = env)
}