test_that("calculate_z_scores(), Z-scores sucessfully calculated", {
  testset <- readRDS(test_path("fixtures", "testset_MeSH_qualifier_lists.rds"))
  qualifier_table <- calculate_z_scores(testset[["MeSH.Terms"]][["qualifier"]],
                                        popset[["qualifier"]],
                                        key_popset = "MeSH")
  expect_vector(as.list(qualifier_table), size = 15)
})

test_that("calculate_z_scores(), Z-scores sucessfully calculated with (modified) population set from version 1.0", {
  testset <- readRDS(test_path("fixtures", "testset_MeSH_qualifier_lists.rds"))
  qualifier_table <- calculate_z_scores(testset[["MeSH.Terms"]][["qualifier"]],
                                        popset2022[["qualifier"]],
                                        key_popset = "MeSH") # changed column name from "qualifier" to "MeSH" compared to version 1.0
  expect_vector(as.list(qualifier_table), size = 13)
})
