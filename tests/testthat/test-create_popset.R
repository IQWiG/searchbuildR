# test formal correctness of population set object
test_that("population set was created successfully", {
  path <- testthat::test_path(pattern = "fixtures", "testset_ris.txt")
  expect_silent(create_popset(path))
  # test range of expected z-scores for stopwords (the, of, and)
  # test probablities(p) of study words e.g. (randomized)
  # test distribution of probabilities (p)
  # compare z-scores against testsets and check published thresholds
})

test_that("current population set meets the expected thresholds", {
  # test distribution of probabilities (p)
  d <-density(popset$freetext$p[popset$freetext$p < 1e-5]*100)
  plot(d, main="Kernel Density of Probabilities")
  polygon(d, col="black")
  # test range of expected z-scores for stopwords (the, of, and)
  head_pop <- head(popset$freetext, n = 10 )
  barplot(head_pop$Norm.frequency, names.arg = head_pop$feature,
          main = "Top 10 most frequent Terms")
  head_mesh <- popset$MeSH.Terms |>
    arrange(desc(Norm.frequency)) |>
    head(n = 10)
  barplot(head_mesh$Norm.frequency, names.arg = head_mesh$MeSH,
          main = "Top 10 most frequent MeSH Terms")

  # test probablities(p) of study words e.g. (randomized)
  path <- testthat::test_path(pattern = "fixtures", "testset_ris.txt")
  newTestset <- create_testset(path)
  path_oldTestset <- testthat::test_path(pattern = "fixtures", "testsetVersion1.rds")
  oldTestset <- readRDS(path_oldTestset)
  identical(newTestset, oldTestset)
  # compare z-scores against testsets and check published thresholds
})

