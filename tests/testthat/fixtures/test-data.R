# testset.rds
testset <- create_testset(test_path("fixtures/ris", "V21-06B.txt"))
saveRDS(testset, "tests/testthat/fixtures/testset.rds")

#testset_ref.txt
"testset_ref.txt" = "S19-02.txt"
#testset_ref.rds
testset_ref <- read_bibliography(test_path("fixtures/ris", "S19-02.txt"), return_df = F)
saveRDS(testset_ref, "tests/testthat/fixtures/testset_ref.rds")

#testset_MeSH_qualifier_lists.rds
testset_MeSH <- create_testset(test_path("fixtures/ris", "I26.txt"))
testset_MeSH <- create_MeSH_qualifier_lists(testset)
saveRDS(testset_MeSH, "tests/testthat/fixtures/testset_MeSH_qualifier_lists.rds")

#testsetVersion1.rds
#created with the population set from Version 1.0
testsetVersion1 <- create_testset(test_path("fixtures", "testset_ris.txt"))
saveRDS(testsetVersion1, "tests/testthat/fixtures/testsetVersion1.rds")

#corpus.rds
corpus <- create_corpus(readRDS(test_path("fixtures", "testset_ref.rds")))
saveRDS(corpus, "tests/testthat/fixtures/corpus.rds")

