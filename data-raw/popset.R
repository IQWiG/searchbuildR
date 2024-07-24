## code to prepare `popset` dataset goes here
seed <- sample.int(.Machine$integer.max, 1L) # actual seed = 3066560L
png("data-raw/histo-popset.png")
references <- sample_popset(sample_size = 20000, year_range = "1900:2024",
              email = "recherche@iqwig.de",
              max_pmid = 38900000,
              seed = seed)
dev.off()
references <- clean_efetch_result(references)

writeLines(references, "dev/references.txt")
download.file("https://nlmpubs.nlm.nih.gov/projects/mesh/MESH_FILES/xmlmesh/desc2024.xml", "dev/desc2024.xml")
mesh_xml <- "dev/desc2024.xml"

download.file("https://nlmpubs.nlm.nih.gov/projects/mesh/MESH_FILES/xmlmesh/qual2024.xml", "dev/qual2024.xml")
qual_xml <- "dev/qual2024.xml"

# import to Endnote and create RIS-export from Endnote
newNorms <- create_popset("dev/PopulationSet2024.txt")
popset <- newNorms$popset
MeSH_Dictionary <- newNorms$newMeSH
Qualifier_Dictionary <- newNorms$newQual
# Note from 19.07.2024: 204 PubMed references are incorrectly processed due to unknown reasons.
# These references are imported as 2 references (one containing MeSh and Epub date, other rest of data) into Endnote.
# Update: New field CON - contains PMID and creates the error.

usethis::use_data(popset, overwrite = TRUE, internal = TRUE)


