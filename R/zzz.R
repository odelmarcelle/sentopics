.onLoad <- function(
  libname = find.package("sentopics"),
  pkgname = "sentopics"
) {
  # CRAN note avoidance
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(c(
      # ".", ".N", ".SD", ":=", "V1", "V2", "V3", "i", "i.prob", "value.y",
      # "id", "lexicon", "model", "parent", "prob", "sentiment",
      # "topic", "value", "vocab", "word", "x", "texts", "label",
      # "LoughranMcDonald"
    ))
  }
  invisible()
}

.onUnload <- function(libpath) {
  library.dynam.unload("sentopics", libpath)
}

Rcpp::loadModule("model_module", TRUE)
