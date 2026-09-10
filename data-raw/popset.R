## code to prepare `popset` dataset goes here
seed <- sample.int(.Machine$integer.max, 1L) # actual seed sampled = 3066560L
png("data-raw/histo-popset.png")
references <- sample_popset(sample_size = 20000, year_range = "1950:2024",
              email = "recherche@iqwig.de",
              max_pmid = 39090000,
              seed = 3066560L)
dev.off()
references <- clean_efetch_result(references)

writeLines(references, "dev/references1950-2024.txt")
download.file("https://nlmpubs.nlm.nih.gov/projects/mesh/MESH_FILES/xmlmesh/desc2025.xml", "dev/desc2025.xml")
mesh_xml <- "dev/desc2025.xml"

download.file("https://nlmpubs.nlm.nih.gov/projects/mesh/MESH_FILES/xmlmesh/qual2025.xml", "dev/qual2025.xml")
qual_xml <- "dev/qual2025.xml"

newNorms <- create_popset("dev/references1950-2024.txt", mesh_xml = mesh_xml, qual_xml = qual_xml)
popset <- newNorms$popset
MeSH_Dictionary <- newNorms$newMeSH
Qualifier_Dictionary <- newNorms$newQual

usethis::use_data(popset, MeSH_Dictionary, Qualifier_Dictionary, overwrite = TRUE, internal = TRUE)


