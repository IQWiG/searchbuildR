#' UI logic for run_app()
#'
#' @returns the UI for a shiny app
#' @import shiny
#' @export
#' @examplesIf interactive()
#' library(shiny)
#' shinyApp(ui = app_ui, server = app_server)
#'

app_ui <- fluidPage( title = "searchbuildR",
                     theme = bs_theme(bootswatch = "sandstone"),

  shinyFeedback::useShinyFeedback(),
  rclipboard::rclipboardSetup(),

      tabsetPanel(
        tabPanel("Data import",
                 sidebarLayout(
                   sidebarPanel(width = 3,
                                style = "background-color :white;",
                                wellPanel(
                                  fileInput("upload", "Upload a testset"),
                                  radioButtons("choose_references", "Choose which references should be analyzed:",
                                               choices = c("Analyze complete testset" = "testset",
                                                           "Create and analyze a random development set" = "devset",
                                                           "Create and analyze a development set with the following seed: " = "seedset")),
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
                                    tabsetPanel(
                                      id ="showPMIDS",
                                      type = "hidden",
                                    tabPanelBody("hideDevsetPanel", ""),
                                      tabPanelBody("devsetPanel",
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
                                      ))
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
                              downloadButton("downloadFreetext", "Download all freetext terms")
                       )
                     ),
                     fluidRow(
                     column(12,
                            HTML("<p>The table shows all freetext terms in the chosen test set.
                            <br> Freetext terms are defined as all words in the title and abstract.</p>
                            <p> Details:
                            <br> All numbers, Unicode symbols and unicode punctuation hase been removed. Hyphenated words have been separated (i.e. 'self-aware' is analyzed as 'self' and 'aware').
                            <br> The z-ccore of a binomial test against a random representative set of PubMed references is calculated.
                              A positive z-score indicates, that the freetext term occurs more often in the test set,
                              than would be expected based on the representative sample of PubMed references.
                              A z-score of 10000 is given to all candidate terms,
                              which did not occurr in the representative sample of PubMed references.</p>
                            *Rare terms are defined as occurring in less than 2 or less than 10% of references")
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
                                         downloadButton("downloadMeSH", "Download all MeSH terms")
                                         )
                                  ),
                                fluidRow(
                                  column(12,
                                         "The table shows all MeSH terms in the chosen test set.
                                         The z-ccore of a binomial test against a random representative set of PubMed references is calculated.
                                         A positive z-score indicates, that the MeSH term occurs more often in the test set,
                                         than would be expected based on the representative sample of PubMed references.
                                         A z-score of 10000 is given to all MeSH terms,
                                         which did not occurr in the representative sample of PubMed references."
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
                          downloadButton("downloadQualifier", "Download all MeSH qualifier")
                          )
                   ),
                 fluidRow(
                   column(12,
                          "The table shows all MeSH qualifier terms in the chosen test set.
                          The z-ccore of a binomial test against a random representative set of PubMed references is calculated.
                          A positive z-score indicates, that the MeSH qualifier term occurs more often in the test set,
                          than would be expected based on the representative sample of PubMed references.
                          A z-score of 10000 is given to all qualifier terms,
                          which did not occurr in the representative sample of PubMed references."
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
                          "In this table all imported keywords and their frequencies are listed.
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
    tabPanel("Freetext terms in context",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(width = 5,
                   fluidRow(
                     column(6,
                            "Enter a freetext term (single word or phrase) to see, in what contexts it occurs.
                            Select a specific reference from the list to see the full title and abstract.
                            The freetext term entered is displayed in all uploaded references and not only in the development set."
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
                   fluidRow(htmlOutput("infoTotalDocumentFreq")),
                   fluidRow(htmlOutput("infoContext")),
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
                          sliderInput("phraseSlider", "Select maximum number of words between 2-word combinations:", 0, 4, 1)
                   )
                 ),
                 fluidRow(
                   column(12,
                          HTML("<p>The table analyzes all 2-word combinations that occur in the uploaded references.
                          The frequency of a 2-word combination is calculated for the selected n-grams.
                          <br>Example: 'benign lung tumour' appear in the table as 2-gram 'benign lung' and 2-gram 'lung tumour' and as 3-gram 'benign tumour' </p>
                          <p>Details:<br>
                          The word entered is analyzed in all uploaded references and not only in the development set.<br>
                          English stopwords are not analyzed as 2-word combinations; i.e. the phrase 'quality of life' would appear as 'quality life' and as 3-gram in the table.<br>
                          The filter function applies automatic right-handside truncation.</p>
                          <p>Definition:<br>
                          An n-gram is a phrase consisting of a specific number of words.
                          A 2-gram contains 2 words (e.g. 'clinical trial'), a 3-gram contains 3 words (e.g. 'quality of life') etc.</p>")
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

