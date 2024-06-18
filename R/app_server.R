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
    req(input$upload)  # Ensure the file is uploaded

    ext <- tools::file_ext(input$upload$name)

    # Ensure ext is a single value and handle invalid file extensions
    if (length(ext) != 1 || ext == "") {
      validate("Please upload a file")
    }

    result <- switch(ext,
                     txt = create_testset(input$upload$datapath),
                     ris = create_testset(input$upload$datapath),
                     validate("Invalid file: Please upload a ris file exported from Endnote or PubMed")
    )

    result
  })
  allRefsTable <- reactive({
    columns <- c("accession", "author", "year", "title")
    table <- purrr::set_names(rep(list(NA_real_),length(columns)), columns) %>% as.data.frame()
    result <- rawdata()$reference.list %>%
      as.data.frame() %>%
      tibble::add_column(table[setdiff(columns, names(.data))])

    result$first_author <- stringr::str_split_i(result$author, " and ", 1)  #only choose first author
    result <- result %>% select("accession", "first_author", "year", "title")
    return(result)
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
     reactable(allRefsTable(),
              columns = list ("accession" = colDef(name = "PMID",
                                              maxWidth = 100),
                            "first_author" = colDef(name ="First Author",
                                                    maxWidth = 150),
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
      paste0(tools::file_path_sans_ext(input$upload),"-freetext.csv")
    },
    content = function(file) {
      output <- zScoreData()$z_score_tables$freetext %>%
        select("feature", "docfreq", "coverage", "frequency", "z", "Norm.docfreq") %>%
        mutate(across(where(is.double), ~ round(.x, digits = 2))) %>%
        arrange(desc(.data$z)) %>%
        rename(`Candidate Terms` = "feature",
               `Documents` = "docfreq",
               `Documents in %` = "coverage",
               `Term frequency` = "frequency",
               `Z-Score` = "z",
               `Population set documents` = "Norm.docfreq")
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
      paste0(tools::file_path_sans_ext(input$upload),"-MeSH.csv")
    },
    content = function(file) {
      output <- zScoreData()$z_score_tables$MeSH %>%
        select("MeSH", "docfreq", "coverage", "z", "Norm.docfreq") %>%
        mutate(across(where(is.double), ~ round(.x, digits = 2))) %>%
        arrange(desc(.data$z)) %>%
        rename(`MeSH Heading` = "MeSH",
               `Documents` = "docfreq",
               `Documents in %` = "coverage",
               `Z-Score` = "z",
               `Population set documents` = "Norm.docfreq")
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
      paste0(tools::file_path_sans_ext(input$upload),"-qualifier.csv")
    },
    content = function(file) {
      output <- zScoreData()$z_score_tables$qualifier %>%
        select("MeSH", "docfreq", "coverage", "frequency", "z", "Norm.docfreq") %>%
        mutate(across(where(is.double), ~ round(.x, digits = 2))) %>%
        arrange(desc(.data$z)) %>%
        rename(`Candidate Terms` = "MeSH",
               `Documents` = "docfreq",
               `Documents in %` = "coverage",
               `Qualifier frequency` = "frequency",
               `Z-Score` = "z",
               `Population set documents` = "Norm.docfreq")
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
                                                       filterable = TRUE),
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

  # extract tokens from text data
  kwicCorpus <- reactive({
    req(input$upload)
    rawdata()$text_corpus
  })

  # mark term of interest
  kwicTokensMarked <- reactive({
    kwicInput_hyphen <-  stringr::str_replace_all(input$kwicInput," ", "-")
    stringr::str_replace_all(kwicCorpus()[[selection()]], stringr::regex(paste0("(\\b)(",input$kwicInput,"|",kwicInput_hyphen,")(\\b)"), ignore_case = T), paste0("<mark>\\2</mark>"))
  })

  #create the raw "keywords-in-context" table
  kwicRawTable <- reactive({
    req(input$kwicInput)
    if(grepl("(\\.)|(\\*)", input$kwicInput)){
      validate("* and . cannot be used in search term entered.")
    }
    if(grepl("-", input$kwicInput)) {
      kwicInput_hyphen <- gsub("-", " - ", input$kwicInput)
      result <- kwicCorpus() %>%
        tokens(split_hyphens = TRUE) %>%
        kwic(phrase(kwicInput_hyphen),
             case_insensitive = TRUE,
             window = input$kwicSlider)
    } else {
      result <- kwicCorpus() %>%
        tokens(split_hyphens = TRUE, remove_punct = TRUE) %>%
        kwic(phrase(input$kwicInput),
             case_insensitive = TRUE,
             window = input$kwicSlider)
    }

    if(nrow(result) == 0){
      validate("Please select a term listed in tab `Freetext`")
    }
    return(result)
  })


  # calculate number of occurrences and number of documents
  context_docfreq <- reactive({
    req(input$kwicInput)
    length(
      attr(
        attr(kwicRawTable(),
             "ntoken"),
        "names")
      )
  })

  context_freq <- reactive({
    req(input$kwicInput)
    nrow(kwicRawTable())
  })
  output$infoTotalDocumentFreq <- renderText({
    paste("Number of documents analyzed:",length(rawdata()$reference.list))
  })

  output$infoContext <- renderText({
    if(!is.null(input$kwicInput)){
      paste("The term occurs",context_freq(),"times in <b>", context_docfreq(), "</b> documents")
    }
  })

  # catch user selection of the interactive table
    selection <-  reactive({
      req(getReactableState("kwicTable", "selected"))
      kwicRawTable()[[getReactableState("kwicTable", "selected"),"docname"]]
    })

  #define UI output
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
      kwicTokensMarked()
      })

  # Tab 7 Phrases

  output$phraseTable <- renderReactable({
    req(input$upload)
    reactable(summarise_adjacency(rawdata()$text_corpus, ngrams = input$phraseSlider+2),
              columns = list( "feature" = colDef(name = "Skip-Grams",
                                                 minWidth = 150,
                                                 filterable = TRUE),
                              "frequency" = colDef(name = "Overall frequency",
                                                   format = colFormat(digits = 0))
              ),
              showSortable = TRUE,
              sortable = TRUE,
              striped = TRUE,
              compact = TRUE,
              highlight = TRUE,
              resizable = TRUE,
              fullWidth = FALSE,
              paginationType= "jump",
              defaultPageSize = 10,
              showPageSizeOptions = TRUE, pageSizeOptions = c(10, 20, 50, 100)
    )
  })
}

