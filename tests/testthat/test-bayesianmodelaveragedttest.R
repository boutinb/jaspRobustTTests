context("Model-Averaged Bayesian t-Tests")

test_that("Rows without a group are excluded", {
  # the last row has a score but no group, so only the other 8 rows go into the models
  dataset <- data.frame(y = c(3, 5, 6, 8, 10, 11, 12, 13, 1),
                        g = factor(c("a", "a", "a", "a", "b", "b", "b", "b", NA)))

  jaspFile <- testthat::test_path("..", "..", "examples", "Model-Averaged Bayesian t-Tests.jasp")
  options  <- jaspTools::analysisOptions(jaspFile)[[1]]
  options$dependent <- "y"
  options$group     <- "g"
  options$inferenceConditionalParameterEstimates <- FALSE
  options$inferenceIndividualModels              <- FALSE
  options$inferenceModelsOverview                <- FALSE
  options$mcmcDiagnosticsPlotEffect              <- FALSE
  options$mcmcDiagnosticsPlotUnequalVariances    <- FALSE
  options$plotsPooledEstimatesEffect             <- FALSE
  options$plotsPooledEstimatesUnequalVariances   <- FALSE
  # few samples keep the test fast; the expected values come from a direct RoBTT fit with the same
  # settings and seed in the same session, because Stan samples differ across OS
  options$advancedMcmcChains     <- 2
  options$advancedMcmcAdaptation <- 500
  options$advancedMcmcSamples    <- 500
  options$setSeed <- TRUE
  options$seed    <- 1
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianModelAveragedTTest", dataset, options)
  expect_equal(results$status, "complete")

  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]]
  footnotes <- paste(sapply(table[["footnotes"]], `[[`, "text"), collapse = "\n")
  expect_match(footnotes, "1 observations? were removed")

  # the priors mirror the options of analysis 1 in the example file; the analysis passes group "b" as x1
  grouped <- dataset[!is.na(dataset$g), ]
  capture.output(fit <- suppressWarnings(RoBTT::RoBTT(
    x1 = grouped$y[grouped$g == "b"], x2 = grouped$y[grouped$g == "a"],
    prior_delta = RoBTT::prior("normal", list(mean = 0, sd = 1)),
    prior_rho   = RoBTT::prior("beta", list(alpha = 1, beta = 1)),
    prior_nu    = NULL,
    chains = 2, warmup = 500, iter = 1000, seed = 1, silent = TRUE
  )))
  components <- summary(fit)[["components"]]
  jaspTools::expect_equal_tables(table[["data"]],
    list(components["Effect", "inclusion_BF"], "2/4", components["Effect", "post_prob"], 0.5, "Effect",
         components["Heterogeneity", "inclusion_BF"], "2/4", components["Heterogeneity", "post_prob"], 0.5,
         "Heterogeneity"))
})
