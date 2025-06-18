withr::local_options(
  list(
    jentre.verbose = FALSE,
    jentre.silence_api_warning = TRUE,
    jentre.entrez_key = rlang::zap()
  ),
  .local_envir = teardown_env()
)