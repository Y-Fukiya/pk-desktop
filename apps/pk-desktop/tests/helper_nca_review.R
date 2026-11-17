nca_review_markup <- function(output, key) {
  doc <- xml2::read_html(output[[paste0("review_", key)]]$html)
  xml2::xml_find_first(doc, ".//*[@data-review-key]")
}

nca_review_intent <- function(input, output, key) {
  fields <- c(dosing = "dosing_context", blq = "blq", c0 = "c0_method")
  list(key = key, generation = as.integer(xml2::xml_attr(nca_review_markup(output, key), "data-generation")),
    value = if (key == "units") TRUE else input[[fields[[key]]]])
}

nca_confirm_displayed <- function(session, input, output, keys = c("dosing", "blq", "c0", "units"),
    supply_defaults = TRUE) {
  if (supply_defaults) {
    defaults <- list(dosing_context = "ongoing", blq = "exclude")
    if ((!is.null(input$terminal_method) && input$terminal_method != "none") || isTRUE(input$derived_enabled))
      defaults <- c(defaults, list(terminal_min_r_squared = .9, terminal_min_span_ratio = 2, aucinf_max_extrap_pct = 20))
    missing <- defaults[vapply(names(defaults), function(field) is.null(input[[field]]) || identical(input[[field]], ""), logical(1))]
    if (length(missing)) do.call(session$setInputs, missing)
  }
  for (key in keys) {
    markup <- nca_review_markup(output, key)
    if (length(xml2::xml_find_all(markup, ".//button[@data-nca-confirm]")))
      session$setInputs(review_choice = nca_review_intent(input, output, key))
  }
  invisible(NULL)
}
