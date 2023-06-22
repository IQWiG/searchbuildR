#' server logic for function run_app()
#'
#' @param input input from server
#' @param output output to UI
#' @param session session ID
#'
#' @returns the function for the server logic
#' @import shiny
#' @export
#' @examplesIf interactive()
#' library(shiny)
#' shinyApp(ui = app_ui, server = app_server)

app_server <- function(input, output, session) {
  #reactive data

  observeEvent(input$upload,{
    updateTabsetPanel(inputId = "afterAnalysis", selected = "emptyPanel")
    updateTabsetPanel(inputId = "SidepanelInfos", selected = "afterUploadPanel")
    updateTabsetPanel(inputId = "FreetextTab", selected = "FreetextEmpty")
    updateTabsetPanel(inputId = "MeSHTab", selected = "MeSHEmpty")
    updateTabsetPanel(inputId = "QualifierTab", selected = "QualifierEmpty")
    updateTabsetPanel(inputId = "KeywordsTab", selected = "KeywordsEmpty")
  })

  observeEvent(input$choose_references,{
    updateTabsetPanel(inputId = "seedPanel", selected = chosenSeed())
               })

  observeEvent(input$analyzeButton,{
    updateTabsetPanel(inputId = "afterAnalysis", selected = "contentPanel")
    updateTabsetPanel(inputId = "showPMIDS", selected = showingPMIDS())
    updateTabsetPanel(inputId = "SidepanelInfos", selected = "afterAnalysisPanel")
    updateTabsetPanel(inputId = "FreetextTab", selected = "FreetextContent")
    updateTabsetPanel(inputId = "MeSHTab", selected = "MeSHContent")
    updateTabsetPanel(inputId = "QualifierTab", selected = "QualifierContent")
    updateTabsetPanel(inputId = "KeywordsTab", selected = "KeywordsContent")
    })

  # reactive variables

  rawdata <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           txt = create_testset(input$upload$datapath),
           ris = create_testset(input$upload$datapath),
           validate("Invalid file: Please upload a risfile exported from Endnote")
    )
  })
    zScoreData <- bindEvent(reactive({
    req(input$upload)
    testset <- create_testset(input$upload$datapath,
                                 dev_set = chosenRefs()[["dev_set"]],
                                 seed = chosenRefs()[["seed"]])
    z_score_analysis <- z_scores(references = testset)
    return(list("testset" = testset,
                "z_score_tables" = z_score_analysis))
  }),input$analyzeButton)

  chosenRefs <- reactive({
    req(input$upload)
    switch(input$choose_references,
           "testset" = list("dev_set" = FALSE, "seed" = NULL),
           "devset" = list("dev_set" = TRUE, "seed" = NULL),
           "seedset" = list("dev_set" = TRUE, "seed" = input$seedID))
  })

  chosenSeed <- reactive({
    req(input$upload)
    switch(input$choose_references,
           "testset" = "systemSeed",
           "devset" = "systemSeed",
           "seedset" = "userSeed")
    })

  showingPMIDS <- reactive({
    req(input$upload)
    switch(input$choose_references,
           "testset" = "hideDevsetPanel",
           "devset" = "devsetPanel",
           "seedset" = "devsetPanel")
  })

  PMIDS_syntax <- reactive({
    map(zScoreData()$testset$PMIDS, paste, collapse = " OR ")
  })

  output$uploadSuccess <- renderText({
    if(!is.null(rawdata())){
      paste(length(rawdata()$reference.list), "references uploaded")
    }
  })

  output$startedAnalysis <- renderText({
    if(!is.null(zScoreData())){
      paste(length(zScoreData()$testset$text_corpus), "references analyzed")
    }
  })


  #Data Import
  output$outputPMIDS <- renderText({
    paste("<b>Testset</b><br>",
          paste0("(",
                 paste(PMIDS_syntax()$testset, collapse = " OR "),
                 ").ui")
          )
    })
  output$clipPMIDS <- renderUI({
    rclipboard::rclipButton(
      inputId = "clibBtn",
      label = "Copy",
      clipText = paste0("(",PMIDS_syntax()$testset, ").ui"),
      icon = icon ("clipboard")
    )
  })
  output$clipPMIDSDev <- renderUI({
    rclipboard::rclipButton(
      inputId = "clibBtnDev",
      label = "Copy",
      clipText = paste0("(",PMIDS_syntax()$development_set, ").ui"),
      icon = icon ("clipboard")
    )
  })
  output$clipPMIDSVal <- renderUI({
    rclipboard::rclipButton(
      inputId = "clibBtnVal",
      label = "Copy",
      clipText = paste0("(",PMIDS_syntax()$validation_set, ").ui"),
      icon = icon ("clipboard")
    )
  })
  output$outputPMIDSDev <- renderText({
    paste("<b>Development set</b><br>",
          paste0("(",
                 PMIDS_syntax()$development_set,
                 ").ui")
          )
    })
  output$outputPMIDSVal <- renderText({
    paste("<b>Validation set</b><br>",
          paste0("(",
                 PMIDS_syntax()$validation_set,
                 ").ui")
          )
    })
  output$randomSeed <- renderText({
    paste("Applied random seed: ", zScoreData()$testset$seed)
  })

  output$downloadDevSet <- downloadHandler(
      filename = function(){
      paste0(input$upload, "devSet.txt")
        },
      content = function(file){
      writeLines(unlist(zScoreData()$testset$development_set), con = file)
        }
      )

  output$downloadValSet <- downloadHandler(
    filename = function(){
      paste0(input$upload, "valSet.txt")
    },
    content = function(file){
      writeLines(unlist(zScoreData()$testset$validation_set), con = file)
    }
  )

  output$allRefs <- renderReactable({
     reactable(as.data.frame(rawdata()$reference.list)[c("accession",
                                             "year",
                                             "title",
                                             "author")],
              columns = list ("accession" = colDef(name = "PMID",
                                                   maxWidth = 100),
                            "author" = colDef(name ="Author"),
                            "year" = colDef(name = "Year",
                                            maxWidth = 100),
                            "title"= colDef(name = "Title")),
              filterable = TRUE,
              showSortable = TRUE,
              striped = TRUE,
              compact = TRUE,
              resizable = TRUE, #fullWidth = FALSE,
              height = "auto",
              #selection = "single",
              highlight = TRUE,
              #onClick = "select",
              paginationType= "jump",
              defaultPageSize = 50,
              showPageSizeOptions = TRUE, pageSizeOptions = c(10, 20, 50, 100, 200)
     )
    })
  # Tab 2 Freetext
  filteredFreetext <- reactive({
    input$filterFreetext &
      zScoreData()$z_score_tables$freetext$docfreq >=2 &
      zScoreData()$z_score_tables$freetext$coverage >= 10 |
      input$filterFreetext == FALSE
  })
  output$zScoreFreetext <- renderReactable({
    reactable(zScoreData()$z_score_tables$freetext[filteredFreetext(),
                                                   c("feature","docfreq", "coverage", "z", "frequency")],
              columns = list( "feature" = colDef(name = "Candidate Term",
                                                 minWidth = 150,
                                                 filterable = TRUE,
                                                 filterMethod = JS("function(rows, columnId, filterValue) {
                                                 const pattern = new RegExp('^' + filterValue, 'i')
                                                 return rows.filter(function(row) {
                                                 return pattern.test(row.values[columnId])
                                                 })
                                                                   }")
                                                 ),
                              "docfreq" = colDef(name = "Documents",
                                                 minWidth = 120),
                              "coverage" = colDef(name = "Documents in %",
                                                  minWidth = 120,
                                                  format = colFormat(suffix = " %",
                                                                     digits = 2)),
                               "z" = colDef(name = "Z-Score",
                                            format = colFormat(digits = 2)),
                              "frequency" = colDef(name ="Term Frequency")
                              ),
              defaultSorted = "z",
              defaultSortOrder = "desc",
              showSortable = TRUE,
              striped = TRUE,
              compact = TRUE,
              resizable = TRUE, fullWidth = FALSE,
              height = "auto",
             #selection = "single",
              highlight = TRUE,
             #onClick = "select",
              paginationType= "jump",
              defaultPageSize = 50,
              showPageSizeOptions = TRUE, pageSizeOptions = c(10, 20, 50, 100, 200)
    )
  })
  output$downloadFreetext <- downloadHandler(
    filename = function(){
      paste0(input$upload,"-freetext.csv")
    },
    content = function(file) {
      output <- zScoreData()$z_score_tables$freetext %>%
        select("feature", "docfreq", "coverage", "frequency", "z", "Norm.docfreq") %>%
        mutate(across(where(is.double), ~ round(.x, digits = 2))) %>%
        arrange(desc(.data$z)) %>%
        rename(`Candidate Terms` = "feature",
               `Absolute Document Frequency` = "docfreq",
               `% Document Frequency` = "coverage",
               `Term frequency` = "frequency",
               `Z-Score` = "z",
               `Absolute Norm Set Document Frequency` = "Norm.docfreq")
      write.csv2(output,
                 file = file, row.names = FALSE,
                 )
    }
  )
  # Tab 3 MeSH
  output$zScoreMeSH <- renderReactable({
    reactable(zScoreData()$z_score_tables$MeSH %>%
                select("MeSH","docfreq", "coverage", "z"),
              columns = list( "MeSH" = colDef(name = "MeSH Heading",
                                               minWidth = 250,
                                               filterable = TRUE,
                                               filterMethod = JS("function(rows, columnId, filterValue) {
                                               const pattern = new RegExp('^' + filterValue, 'i')
                                               return rows.filter(function(row) {
                                               return pattern.test(row.values[columnId])
                                               })
                                                                 }")
                                              ),
              "docfreq" = colDef(name = "Documents",
                                 minWidth = 120),
              "coverage" = colDef(name = "Documents in %",
                                  minWidth = 120,
                                  format = colFormat(suffix = " %",
                                                     digits = 2)),
              "z" = colDef(name = "Z-Score",
                           format = colFormat(digits = 2))),
              defaultSorted = "z",
              defaultSortOrder = "desc",
              showSortable = TRUE,
              striped = TRUE,
              compact = TRUE,
              resizable = TRUE, fullWidth = FALSE,
              height = "auto",
              #selection = "single",
              highlight = TRUE,
              #onClick = "select",
              paginationType= "jump",
              defaultPageSize = 50,
              showPageSizeOptions = TRUE, pageSizeOptions = c(10, 20, 50, 100, 200)
              )
  })
  output$downloadMeSH <- downloadHandler(
    filename = function(){
      paste0(input$upload,"-MeSH.csv")
    },
    content = function(file) {
      output <- zScoreData()$z_score_tables$MeSH %>%
        select("MeSH", "docfreq", "coverage", "z", "Norm.docfreq") %>%
        mutate(across(where(is.double), ~ round(.x, digits = 2))) %>%
        arrange(desc(.data$z)) %>%
        rename(`MeSH Heading` = "MeSH",
               `Absolute Document Frequency` = "docfreq",
               `% Document Frequency` = "coverage",
               `Z-Score` = "z",
               `Absolute Norm Set Document Frequency` = "Norm.docfreq")
       write.csv2(output,
                 file = file, row.names = FALSE)
    }
  )
  # Tab 4 Qualifier
  output$zScoreQualifier <- renderReactable({
    reactable(zScoreData()$z_score_tables$qualifier %>%
                select("MeSH","docfreq", "coverage", "z"),
              columns = list( "MeSH" = colDef(name = "Qualifier",
                                              minWidth = 200,
                                              filterable = TRUE,
                                              filterMethod = JS("function(rows, columnId, filterValue) {
                                               const pattern = new RegExp('^' + filterValue, 'i')
                                               return rows.filter(function(row) {
                                               return pattern.test(row.values[columnId])
                                               })
                                                                 }")
                                              ),
              "docfreq" = colDef(name = "Documents",
                                 minWidth = 120),
              "coverage" = colDef(name = "Documents in %",
                                  minWidth = 120,
                                  format = colFormat(suffix = " %",
                                                     digits = 2)),
              "z" = colDef(name = "Z-Score",
                           format = colFormat(digits = 2))),
              defaultSorted = "z",
              defaultSortOrder = "desc",
              showSortable = TRUE,
              striped = TRUE,
              compact = TRUE,
              resizable = TRUE, fullWidth = FALSE,
              height = "auto",
              #selection = "single",
              highlight = TRUE,
              #onClick = "select",
              paginationType= "jump",
              defaultPageSize = 76,
              showPageSizeOptions = TRUE, pageSizeOptions = c(10, 20, 50, 100)
    )
  })

  output$downloadQualifier <- downloadHandler(
    filename = function(){
      paste0(input$upload,"-qualifier.csv")
    },
    content = function(file) {
      output <- zScoreData()$z_score_tables$qualifier %>%
        select("MeSH", "docfreq", "coverage", "frequency", "z", "Norm.docfreq") %>%
        mutate(across(where(is.double), ~ round(.x, digits = 2))) %>%
        arrange(desc(.data$z)) %>%
        rename(`Candidate Terms` = "MeSH",
               `Absolute Document Frequency` = "docfreq",
               `% Document Frequency` = "coverage",
               `Absolute Qualifier Frequency` = "frequency",
               `Z-Score` = "z",
               `Absolute Norm Set Document Frequency` = "Norm.docfreq")
      write.csv2(output,
                 file = file, row.names = FALSE,
      )
    }
  )

  # Tab 5 All Keywords
  output$frequencyKeywords <- renderReactable({
    reactable(zScoreData()$z_score_tables$all_keywords,
              columns = list("MeSH_qualifier" = colDef(name = "All Keywords",
                                                       minWidth = 250,
                                                       filterable = TRUE,
                                                       filterMethod = JS("function(rows, columnId, filterValue) {
                                                       const pattern = new RegExp('^' + filterValue, 'i')
                                                       return rows.filter(function(row) {
                                                       return pattern.test(row.values[columnId])
                                                       })
                                                                         }")),
                             "docfreq" = colDef(name = "Documents",
                                                minWidth = 120),
                             "coverage" = colDef(name = "Documents in %",
                                                 minWidth = 120,
                                                 format = colFormat(suffix = " %",
                                                                    digits = 2)),
                             "frequency" = colDef(name = "Keyword Frequency")),
              showSortable = TRUE,
              striped = TRUE,
              compact = TRUE,
              resizable = TRUE, fullWidth = FALSE,
              height = "auto",
              #selection = "single",
              highlight = TRUE,
              #onClick = "select",
              paginationType= "jump",
              defaultPageSize = 76,
              showPageSizeOptions = TRUE, pageSizeOptions = c(10, 20, 50, 100)
    )
  })

  # Tab 6 Keywords-in-context
  # raw data or development set?

  kwicTokens <- reactive({
    req(input$upload)
    rawdata()$text_corpus %>%
    tokens()
  })

  kwicTokensMarked <- reactive({
    selected_reference <- kwicTokens()[[selection()]]
    index <- which(tolower(selected_reference) == tolower(input$kwicInput))
    selected_reference[index] <- paste0("<mark>", selected_reference[index], "</mark>")
    return(selected_reference)
  })

  kwicRawTable <- reactive({
    req(input$kwicInput)
      result <- kwicTokens()  %>%
        kwic(input$kwicInput,
             case_insensitive = TRUE,
             window = input$kwicSlider)
      if(nrow(result) == 0){
        validate("Please select a term listed in table `Freetext`")
      }
      return(result)
    })

    selection <-  reactive({
      req(getReactableState("kwicTable", "selected"))
      kwicRawTable()[[getReactableState("kwicTable", "selected"),"docname"]]
    })

    output$kwicTable <- renderReactable({
      reactable( kwicRawTable() %>%
                   as.data.frame() %>%
                   select("docname", "pre", "keyword", "post"),
      columns = list("docname" = colDef(name = "Publication",
                                        minWidth = 150),
                     "pre" = colDef(name = "Before",
                                    align = "right",
                                    minWidth = 300),
                     "keyword" = colDef(name = "Keyword"),
                     "post" = colDef(name = "After",
                                     align = "left",
                                     minWidth = 300)
      ),
      defaultSorted = "post",
      showSortable = TRUE,
      striped = TRUE,
      compact = TRUE,
      resizable = TRUE, fullWidth = FALSE,
      height = "auto",
      selection = "single",
      defaultSelected = 1,
      highlight = TRUE,
      onClick = "select",
      pagination = FALSE,
    #  paginationType= "jump", showPageSizeOptions = TRUE, pageSizeOptions = c(10, 20, 50, 100, 200), defaultPageSize = 200
            )})

    output$kwicSelectedRef <- renderText({
      paste(kwicTokensMarked(), collapse = " ")
      })

  # Tab 7 Phrases

  output$phraseTable <- renderReactable({
    req(input$upload)
    reactable(summarise_adjacency(rawdata()$text_corpus, ngrams = input$phraseSlider+2),
              columns = list( "feature" = colDef(name = "N-grams",
                                                 minWidth = 150,
                                                 filterable = TRUE,
                                                 filterMethod = JS("function(rows, columnId, filterValue) {
                                                 const pattern = new RegExp(filterValue, 'i')
                                                 return rows.filter(function(row) {
                                                 return pattern.test(row.values[columnId])
                                                 })
                                                                   }")
              ),
              "frequency" = colDef(name = "Frequency",
                                   format = colFormat(digits = 0))
              ),
              )
  })
}

