#' Run App
#'
#' @returns a shiny app
#' @import shiny
#' @noRd
app_ui <- fluidPage( title = "searchterms",
                     theme = bs_theme(bootswatch = "sandstone"),

  shinyFeedback::useShinyFeedback(),
  rclipboard::rclipboardSetup(),

      tabsetPanel(
        tabPanel("Data import",
                 h4("Proof of concept version"),
                 sidebarLayout(
                   sidebarPanel(width = 3,
                                style = "background-color :white;",
                                wellPanel(
                                  fileInput("upload", "Choose testset"),
                                  radioButtons("choose_references", "Choose references for analysis:",
                                               choices = c("All references" = "testset",
                                                           "Random development set" = "devset",
                                                           "Development set with the following seed: " = "seedset")),
                                  tabsetPanel(
                                    id = "seedPanel",
                                    type = "hidden",
                                    tabPanelBody("systemSeed",
                                                 ),
                                    tabPanelBody("userSeed",
                                                 numericInput("seedID", "Seed", value = NULL,
                                                              min = 1,
                                                              max = .Machine$integer.max)
                                                 )
                                  ),
                                  actionButton("analyzeButton", "Start text analysis")
                                  ),
                                wellPanel(
                                  textOutput("uploadSuccess")
                                  ),
                                wellPanel(
                                  tabsetPanel(
                                    id = "SidepanelInfos",
                                    type = "hidden",
                                    tabPanelBody("afterUploadPanel","")                                                 ),
                                    tabPanelBody("afterAnalysisPanel",
                                                 htmlOutput("startedAnalysis"),
                                                 htmlOutput("randomSeed")
                                    )
                                  )
                                ),
                   mainPanel(width = 9,
                     tabsetPanel(
                       id = "afterAnalysis",
                       type = "hidden",
                       tabPanelBody("emptyPanel", ""),
                       tabPanelBody("contentPanel",
                                    fluidRow(
                                      column(1,uiOutput("clipPMIDS")),
                                      column(11,htmlOutput("outputPMIDS"))
                                    ),
                                    fluidRow(
                                      column(6,
                                             fluidRow(
                                               column(2,uiOutput("clipPMIDSDev")),
                                               column(10,htmlOutput("outputPMIDSDev"))
                                             )),
                                      column(6,
                                             fluidRow(
                                               column(2,uiOutput("clipPMIDSVal")),
                                               column(10,htmlOutput("outputPMIDSVal"))
                                               ))
                                             ),
                                    fluidRow(
                                      column(6,downloadButton("downloadDevSet", "Download Development Set")),
                                      column(6,downloadButton("downloadValSet", "Download Validation Set"))
                                      )
                                    ),
                     ),
                     reactableOutput("allRefs")
                     )
                   ),
        ),
    tabPanel("Freetext",
             tabsetPanel(
               id = "FreetextTab",
               type = "hidden",
               tabPanelBody("FreetextEmpty", ""),
               tabPanelBody("FreetextContent",
                 sidebarLayout(
                   sidebarPanel(
                     fluidRow(
                       column(12,
                              align = "center",
                              downloadButton("downloadFreetext", "Download")
                       )
                     ),
                     fluidRow(
                     column(12,
                            "The table shows all candidate terms in the chosen test set.
                              The z-ccore of a binomial test against a random representative set of PubMed references is calculated.
                              A positive z-score indicates, that the candidate term occurs more often in the test set,
                              than would be expected based on the representative sample of Pubmed references.
                              A z-score of 10000 is given to all candidate terms,
                              which did not occurr in the representative sample of Pubmed references."
                            )
                     ),fluidRow(
                       column(12,
                              "*Rare terms are defined as occurring in less than 2 or less than 10% of references"
                       )
                     )
                   ),
                   mainPanel(
                     fluidRow(
                       column(2,
                              align = "right",
                              p(""),
                              p(""),
                              checkboxInput("filterFreetext",
                                            "Hide rare terms*",
                                            value = FALSE)
                       ),
                       column(10,
                              style = "overflow-y:scroll;position:relative;max-height:800px",
                              reactableOutput("zScoreFreetext")
                              )
                     ),
                     )
                 )
               )
             )
             ),
    tabPanel("MeSH",
             tabsetPanel(
               id = "MeSHTab",
               type = "hidden",
               tabPanelBody("MeSHEmpty", ""),
               tabPanelBody("MeSHContent",
                            sidebarLayout(
                              sidebarPanel(
                                fluidRow(
                                  column(12,
                                         align = "center",
                                         downloadButton("downloadMeSH", "Download")
                                         )
                                  ),
                                fluidRow(
                                  column(12,
                                         "The table shows all candidate MeSH terms in the chosen test set.
                                         The z-ccore of a binomial test against a random representative set of PubMed references is calculated.
                                         A positive z-score indicates, that the candidate MeSH term occurs more often in the test set,
                                         than would be expected based on the representative sample of Pubmed references.
                                         A z-score of 10000 is given to all candidate MeSH terms,
                                         which did not occurr in the representative sample of Pubmed references."
                                         )
                                  )
                                ),
                              mainPanel(
                                style = "overflow-y:scroll;position:relative;max-height:800px",
                                reactableOutput("zScoreMeSH")
                              )
                            )
                            )
               )
             ),
    tabPanel("Qualifier",
    tabsetPanel(
        id = "QualifierTab",
        type = "hidden",
        tabPanelBody("QualifierEmpty", ""),
        tabPanelBody("QualifierContent",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   column(12,
                          align = "center",
                          downloadButton("downloadQualifier", "Download")
                          )
                   ),
                 fluidRow(
                   column(12,
                          "The table shows all candidate qualifier terms in the chosen test set.
                          The z-ccore of a binomial test against a random representative set of PubMed references is calculated.
                          A positive z-score indicates, that the candidate qualifier term occurs more often in the test set,
                          than would be expected based on the representative sample of Pubmed references.
                          A z-score of 10000 is given to all candidate qualifier terms,
                          which did not occurr in the representative sample of Pubmed references."
                   )
                 )
                 ),
               mainPanel(
                 style = "overflow-y:scroll;position:relative;max-height:800px",
                 reactableOutput("zScoreQualifier")
               )
               )
             )
    )
    ),
    tabPanel("All Keywords",
    tabsetPanel(
      id = "KeywordsTab",
      type = "hidden",
      tabPanelBody("KeywordsEmpty", ""),
      tabPanelBody("KeywordsContent",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   column(12,
                          "In this table all imported keywords and their frequency are listed.
                          All MeSH/qualifier combinations are listed separately.
                          If non-MeSH keywords were imported, they are included in this table only.")
               )
               ),
               mainPanel(
                 style = "overflow-y:scroll;position:relative;max-height:800px",
                 reactableOutput("frequencyKeywords")
                 )
               )
             )
      )
    ),
    tabPanel("Candidate terms in context",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(width = 5,
                   fluidRow(
                     column(6,
                            "Enter a candidate term to see, in what contexts it occurs.
                            Select a specific reference from the list to see the full title and abstract.
                            The candidate term entered is displayed in all uploaded references and not only in the development set."
                            ),
                     column(6,
                            textInput("kwicInput", "Enter term"),
                            sliderInput("kwicSlider", "Select window to display:", 1, 10, 5)
                            )
                   ),
                   wellPanel(
                     style = "background-color :white;",
                     htmlOutput("kwicSelectedRef")
                   )
                   ),
                 mainPanel(width = 7,
                   style = "overflow-y:scroll;position:relative;max-height:800px",
                   reactableOutput("kwicTable")
                   )
               )
             )
             ),
    tabPanel("Phrases",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   column(12,
                          align = "center",
                          sliderInput("phraseSlider", "Select maximum distance in words:", 0, 4, 1)
                   )
                 ),
                 fluidRow(
                   column(12,
                          htmlOutput("infoPhrases")
                   )
                 )
                 ),
               mainPanel(
                 style = "overflow-y:scroll;position:relative;max-height:800px",
                 reactableOutput("phraseTable")
                 )
               )
             )
    )
)

