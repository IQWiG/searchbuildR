## code to prepare `popset` dataset goes here
seed <- sample.int(.Machine$integer.max, 1L) # actual seed = 3066560L
png("data-raw/histo-popset.png")
references <- sample_popset(sample_size = 20000, year_range = "1900:2024",
              email = "recherche@iqwig.de",
              max_pmid = 38900000,
              seed = seed)
dev.off()
references <- clean_efetch_result(references)

writeLines(references, "data-raw/references.txt")
# import to Endnote and create RIS-export from Endnote
popset <- create_popset("~/2022_04_01_population_set_Endnote.txt")
# Note from 21.04.2022: Some PubMed references contain a secondary PMID under the RIS-Tag 'SO -' which creates non-existent References, when imported into Endnote. Corrected manually.

usethis::use_data(popset, overwrite = TRUE, internal = TRUE)

#Create a random population set based on random PMIDS
# step 1: create stratified random PMIDS
# step 2: chunk PMIDs for processing with the entrez direct API (max. 100 PMIDs)
# step 3: check for invalid PMIDS
# step 4: write text-File in PubMed Format
#
#library(rentrez)
#
## step 1
#set.seed(67800) # set fixed random engine for reproducibility
#random_PMIDS_to8d3 <- sample(1000:36000000,19500) # sample random PMIDs

#set.seed(67800)
#random_PMIDS_from8d3 <- sample(30000000:36000000,1200) # sample appropriate proportion of highest PMIDs in order to have a sample size of ~400 of the latest 4 years (since 2019)
#random_result <- unique(c(random_PMIDS_to8d3, random_PMIDS_from8d3)) # append all retrieved PMIDs into one vector

# plot PMID disribution
#options(scipen=999)
#plot(density(random_PMIDS_to8d3))
#plot(density(random_PMIDS_from8d3))

#Chunk PMID vector into a list of vector of 100 PMIDs for API-Request
#PMID_list100 <- split(random_result,             # Applying split() function
#                      ceiling(seq_along(random_result) / 100))
#
## Loop efetch request to entrez direct (Pubmed API) and write retrieved references into a text-file in Pubmed-Format
#for (pmids in seq_along(PMID_list100)){
#  temp_id <- PMID_list100[[pmids]]
#  Sys.sleep(0.1)
#  recs <- entrez_fetch(db="pubmed", id = temp_id,
#                       rettype = "medline")
#  recs_cleaned <- gsub("The following PMID is not available","", recs) # delete unavailable PMIDS
#  cat(recs_cleaned, file = "2022_04_01_population_set.txt", append = T)
#  cat("chunk", pmids, "successfully downloaded\r")
#}
#
# Some PubMed references contain false carriage returns in the MeSH terms which creates errors, when parsing MeSH terms
#
#popset <- readLines("2022_04_01_population_set.txt")
#white_lines <- grep("^\\s{6}", popset)
#prev_white_lines <- white_lines -1
#MH_lines <- grep("^MH\\s", popset)
#incomplete_lines <-  intersect(MH_lines, prev_white_lines)
#popset[incomplete_lines]
#completing_lines <-incomplete_lines +1

#complete_MH <- function(){
#  new_popset <- popset
#  for (line in seq_along(incomplete_lines)){
#    incomplete_line <- incomplete_lines[line]
#    completing_line <- completing_lines[line]
#    completed_MH <- paste0(new_popset[incomplete_line], new_popset[completing_line])
#    completed_MH <- sub("      ", " ", completed_MH)
#    new_popset[incomplete_line] <- completed_MH
#  }
#  return(new_popset[-completing_lines])
#}
#new_popset <- complete_MH()
#writeLines(con = "2022_04_01_population_set_complete_MH.txt", new_popset)
# import to Endnote and create RIS-export from Endnote
#popset <- create_popset("~/2022_04_01_population_set_Endnote.txt")
# Note from 21.04.2022: Some PubMed references contain a secondary PMID under the RIS-Tag 'SO -' which creates non-existent References, when imported into Endnote. Corrected manually.

#usethis::use_data(popset, overwrite = TRUE, internal = TRUE)
