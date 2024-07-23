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
#  MeSHXML <-  xml2::read_xml(mesh_xml) |>
# xml2::as_list()
MeSHTibble <- tibble::tibble(mesh = MeSHXML$DescriptorRecordSet)
new_MeSH <- update_mesh(mesh_xml)
saveRDS(new_MeSH, "dev/new_MeSH.rds")

# import to Endnote and create RIS-export from Endnote
popset <- create_popset("dev/PopulationSet2024.txt")
# Note from 19.07.2024: 204 PubMed references are incorrectly processed due to unknown reasons.
# These references are imported as 2 references (one containing MeSh and Epub date, other rest of data) into Endnote.
# Update: New field CON - contains PMID and creates the error.

usethis::use_data(popset, overwrite = TRUE, internal = TRUE)


