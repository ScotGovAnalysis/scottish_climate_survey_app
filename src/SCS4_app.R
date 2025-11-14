# app.R — Scottish Climate Survey dashboard
# (shows Total N in table, displays all rows, and adds downloads: CSV + JPG)
# --------------------------------------------------------------------------
# install.packages(c(
#   "shiny","readxl","dplyr","tidyr","stringr","forcats",
#   "ggplot2","DT","scales","survey","tibble","openxlsx"
# ))

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(DT)
library(scales)
library(survey)
library(tibble)

options(survey.lonely.psu = "adjust")  # safer defaults for lonely PSUs

# Allow uploads up to 100 MB (tune to your needs)
options(shiny.maxRequestSize = 100 * 1024^2)

ui <- fluidPage(
  titlePanel("Scottish Climate Survey – Weighted proportions by group"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file", "Upload data workbook (.xlsx)",
        accept = ".xlsx", buttonLabel = "Browse...",
        placeholder = "Choose Scottish Climate Survey - 2024 data + labels.xlsx"
      ),
      helpText("Max file size: 10 MB. Expects sheets: Data, Variable Labels, Value Labels."),
      checkboxInput("debug", "Show debug panel", FALSE),
      tags$hr(),
      
      uiOutput("var_select_ui"),
      uiOutput("grp1_select_ui"),
      uiOutput("grp2_select_ui"),
      uiOutput("wgt_select_ui"),
      
      tags$hr(),
      checkboxInput("show_counts", "Show unweighted Ns in header", TRUE),
      checkboxInput("drop_empty_groups", "Drop groups with all-missing responses", TRUE),
      width = 4
    ),
    mainPanel(
      uiOutput("question_header"),
      
      # Plot + download
      fluidRow(
        column(8, plotOutput("prop_plot", height = "600px")),
        column(4,
               h5("Downloads"),
               downloadButton("dl_plot_jpg", "Download plot (JPG)", class = "btn-primary"),
               tags$br(), tags$br(),
               downloadButton("dl_table_csv", "Download table (CSV)")
        )
      ),
      
      tags$hr(),
      
      # Debug panel (optional)
      conditionalPanel(
        condition = "input.debug == true",
        h4("Debug panel"),
        verbatimTextOutput("debug_text", placeholder = TRUE),
        DTOutput("debug_varlab"),
        DTOutput("debug_vallab"),
        DTOutput("debug_data_head"),
        tags$hr()
      ),
      
      # Proportion table (all rows)
      DTOutput("prop_table"),
      width = 8
    )
  )
)

server <- function(input, output, session) {
  
  notify <- function(txt, type = "message", duration = 5) {
    showNotification(txt, type = switch(type,
                                        "error" = "error",
                                        "warning" = "warning",
                                        "message"
    ), duration = duration)
  }
  
  # ---------- Read workbook with validations ----------
  wb <- reactive({
    req(input$file)
    if (input$file$size > 10 * 1024^2) {
      notify("File is larger than 10 MB.", "error")
      validate(need(FALSE, "File is larger than 10 MB."))
    }
    
    path <- input$file$datapath
    sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) {
      notify(paste("Error reading sheets:", e$message), "error", duration = NULL)
      NULL
    })
    validate(need(!is.null(sheets), "Could not read the workbook. Is it a valid .xlsx file?"))
    
    need_sheets <- c("Data","Variable Labels","Value Labels")
    if (!all(need_sheets %in% sheets)) {
      msg <- paste0("Workbook must contain sheets: ", paste(need_sheets, collapse = ", "))
      notify(msg, "error", duration = NULL)
      validate(need(FALSE, msg))
    }
    
    data_tbl <- read_excel(path, sheet = "Data")
    varlab   <- read_excel(path, sheet = "Variable Labels")
    vallab   <- read_excel(path, sheet = "Value Labels")
    
    # Normalise case
    names(varlab) <- tolower(names(varlab))
    names(vallab) <- tolower(names(vallab))
    
    # Expected columns
    validate(
      need(all(c("variable","label") %in% names(varlab)),
           "‘Variable Labels’ must have columns: Variable, Label."),
      need(all(c("variable","value","label") %in% names(vallab)),
           "‘Value Labels’ must have columns: Variable, Value, Label.")
    )
    
    varlab <- varlab %>%
      transmute(variable = as.character(variable), label = as.character(label)) %>%
      distinct()
    
    vallab <- vallab %>%
      transmute(variable = as.character(variable),
                value    = as.character(value),
                label    = as.character(label)) %>%
      distinct()
    
    # Keep only labels present in Data
    varlab <- varlab %>% filter(variable %in% names(data_tbl))
    vallab <- vallab %>% filter(variable %in% names(data_tbl))
    
    list(
      data   = tibble::as_tibble(data_tbl),
      varlab = varlab,
      vallab = vallab,
      sheets = sheets
    )
  })
  
  # ---------- Choices reactive ----------
  
  choices <- reactive({
    req(wb())
    df <- wb()$data
    vl <- wb()$varlab %>% dplyr::filter(variable %in% names(df))
    
    # QUESTIONS: start with @q OR q  -> ^@?q
    q_vars <- vl %>%
      dplyr::filter(stringr::str_detect(variable, "^@?q")) %>%
      dplyr::mutate(choice_lab = dplyr::if_else(is.na(label) | label == "", variable, label)) %>%
      dplyr::arrange(choice_lab)
    
    # GROUPINGS: exclude the weighting variables
    g_vars <- vl %>%
      dplyr::filter(!stringr::str_detect(variable,  "^@weight")) %>%
      dplyr::mutate(choice_lab = dplyr::if_else(is.na(label) | label == "", variable, label)) %>%
      dplyr::arrange(choice_lab)
    
    # WEIGHTS: start with @weight
    w_vars <- vl %>%
      dplyr::filter(stringr::str_detect(variable, "^@weight")) %>%
      dplyr::mutate(choice_lab = dplyr::if_else(is.na(label) | label == "", variable,
                                                paste0(label, " (", variable, ")"))) %>%
      dplyr::arrange(choice_lab)
    
    # (Optional) log counts for debug
    message("[choices] q_vars=", nrow(q_vars), " g_vars=", nrow(g_vars), " w_vars=", nrow(w_vars))
    
    list(q = q_vars, g = g_vars, w = w_vars)
  })
  
  
  # ---------- Dropdown UIs ----------
  output$var_select_ui <- renderUI({
    req(wb())
    qv <- choices()$q
    if (nrow(qv) == 0) {
      return(tagList(h5("Variable of interest (question):"),
                     div(class = "text-danger", "No @q* variables found.")))
    }
    selectInput("var", "Variable of interest (question):",
                choices = setNames(qv$variable, qv$choice_lab),
                selected = qv$variable[1], width = "100%")
  })
  
  output$grp1_select_ui <- renderUI({
    req(wb())
    gv <- choices()$g
    if (nrow(gv) == 0) {
      return(tagList(h5("Primary grouping variable:"),
                     div(class = "text-danger", "No grouping variables found (non-@).")))
    }
    selectInput("grp1", "Primary grouping variable:",
                choices = setNames(gv$variable, gv$choice_lab),
                selected = gv$variable[1], width = "100%")
  })
  
  output$grp2_select_ui <- renderUI({
    req(wb())
    gv <- choices()$g
    if (nrow(gv) == 0) return(NULL)
    selectInput("grp2", "Secondary grouping variable (optional):",
                choices = c("None" = "", setNames(gv$variable, gv$choice_lab)),
                selected = "", width = "100%")
  })
  
  output$wgt_select_ui <- renderUI({
    req(wb())
    wv <- choices()$w
    if (nrow(wv) == 0) {
      return(selectInput("wgt", "Weighting variable:",
                         choices = c("Equal weights (1)" = ""), selected = "", width = "100%"))
    }
    selectInput("wgt", "Weighting variable:",
                choices = c("Equal weights (1)" = "", setNames(wv$variable, wv$choice_lab)),
                selected = "", width = "100%")
  })
  
  # ---------- Value label helper ----------
  make_factor <- function(x, varname, vallab) {
    if (is.numeric(x)) x <- ifelse(x < 0, NA, x)  # treat negative codes as missing
    vl <- vallab %>% filter(variable == varname)
    if (nrow(vl) == 0) {
      return(fct_inorder(as.factor(x)))
    } else {
      lvl_codes <- as.character(vl$value)
      lvl_labs  <- as.character(vl$label)
      return(factor(as.character(x), levels = lvl_codes, labels = lvl_labs))
    }
  }
  
  # ---------- Prepare analysis data ----------
  analysis_data <- reactive({
    req(wb())
    req(input$var, input$grp1)
    
    df     <- wb()$data
    varlab <- wb()$varlab
    vallab <- wb()$vallab
    
    sel <- unique(c(input$var, input$grp1,
                    if (nzchar(input$grp2)) input$grp2 else NULL,
                    if (nzchar(input$wgt))  input$wgt  else NULL))
    sel <- sel[sel %in% names(df)]
    
    validate(need(all(c(input$var, input$grp1) %in% sel),
                  "Selected variables must exist in the Data sheet."))
    
    df <- df %>%
      mutate(id = row_number()) %>%
      select(all_of(c("id", sel)))
    
    df <- df %>%
      mutate(
        .question = make_factor(.data[[input$var]], input$var, vallab),
        .grp1     = make_factor(.data[[input$grp1]], input$grp1, vallab),
        .grp2     = if (nzchar(input$grp2))
          make_factor(.data[[input$grp2]], input$grp2, vallab)
        else factor(NA),
        .w        = if (nzchar(input$wgt) && input$wgt %in% names(df))
          suppressWarnings(as.numeric(.data[[input$wgt]]))
        else 1
      ) %>%
      mutate(.w = ifelse(is.na(.w) | .w < 0, 0, .w)) %>%
      mutate(
        .group = if (nzchar(input$grp2)) {
          interaction(.grp1, .grp2, sep = " × ", drop = TRUE, lex.order = TRUE)
        } else {
          .grp1
        }
      )
    
    if (isTRUE(input$drop_empty_groups)) {
      df <- df %>% group_by(.group) %>% filter(any(!is.na(.question))) %>% ungroup()
    }
    
    q_text <- varlab %>% filter(variable == input$var) %>% pull(label)
    if (length(q_text) == 0 || is.na(q_text)) q_text <- input$var
    
    list(df = df, question_text = q_text)
  })
  
  # ---------- Survey design ----------
  design_re <- reactive({
    a <- analysis_data(); df <- a$df
    need_cols <- c("id",".w",".question",".group")
    validate(need(all(need_cols %in% names(df)),
                  paste("Internal data preparation failed. Missing:",
                        paste(setdiff(need_cols, names(df)), collapse = ", "))))
    svydesign(ids = ~id, weights = ~.w, data = df)
  })
  
  # ---------- Weighted proportions ----------
  prop_df <- reactive({
    dsgn <- design_re()
    tab <- tryCatch(svytable(~ .group + .question, design = dsgn),
                    error = function(e) { showNotification(paste("svytable error:", e$message), type = "error"); NULL })
    validate(need(!is.null(tab) && length(tab) > 0, "No data to summarise for this selection."))
    ptab <- prop.table(tab, margin = 1)
    as.data.frame(ptab) %>%
      as_tibble() %>%
      rename(group = `.group`, response = `.question`, prop = Freq) %>%
      mutate(pct = 100 * prop) %>%
      arrange(group, response)
  })
  
  # ---------- Unweighted Ns (group totals only) ----------
  unweighted_ns <- reactive({
    a <- analysis_data(); df <- a$df
    df %>%
      filter(!is.na(.question)) %>%
      count(.group, name = "N_unweighted") %>%
      arrange(.group)
  })
  
  # ---------- Header ----------
  output$question_header <- renderUI({
    req(analysis_data()$question_text)
    vl <- wb()$varlab
    grp1_lbl <- vl$label[match(input$grp1, vl$variable)]; if (is.na(grp1_lbl)) grp1_lbl <- input$grp1
    grp2_lbl <- if (nzchar(input$grp2)) vl$label[match(input$grp2, vl$variable)] else NULL
    if (!is.null(grp2_lbl) && is.na(grp2_lbl)) grp2_lbl <- input$grp2
    wgt_lbl  <- if (nzchar(input$wgt)) vl$label[match(input$wgt, vl$variable)] else "Equal weights (1)"
    if (is.na(wgt_lbl)) wgt_lbl <- input$wgt
    
    ns_text <- NULL
    if (isTRUE(input$show_counts)) {
      ns <- unweighted_ns()
      if (nrow(ns) > 0) {
        ns_text <- paste0(" | Unweighted Ns: ",
                          paste0(ns$.group, "=", ns$N_unweighted, collapse = "; "))
      }
    }
    
    tagList(
      h4(analysis_data()$question_text),
      p(
        strong("Grouping: "),
        if (nzchar(input$grp2)) paste0(grp1_lbl, " × ", grp2_lbl) else grp1_lbl, br(),
        strong("Weight: "), wgt_lbl,
        if (!is.null(ns_text)) ns_text
      )
    )
  })
  
  # ---------- ggplot object (for render and download) ----------
  prop_plot_obj <- reactive({
    dfp <- prop_df()
    validate(need(nrow(dfp) > 0, "No non-missing data to plot."))
    
    dfp <- dfp %>%
      mutate(
        group    = fct_inorder(as.factor(group)),
        response = fct_inorder(as.factor(response))
      )
    
    ggplot(dfp, aes(x = prop, y = group, fill = response)) +
      geom_col(width = 0.7, colour = "white", size = 0.2) +
      scale_x_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
      scale_fill_brewer(palette = "Set2", name = NULL) +
      labs(x = "Proportion of group", y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical"
      )
  })
  
  output$prop_plot <- renderPlot({
    prop_plot_obj()
  })
  
  # ---------- Wide table (only % columns + Total N) ----------
  wide_table <- reactive({
    dfp <- prop_df()
    # proportions table (prop 0–1 for DT percentage formatting)
    wide_pct <- dfp %>%
      select(group, response, prop) %>%
      arrange(group, response) %>%
      pivot_wider(names_from = response, values_from = prop, values_fill = 0) %>%
      arrange(group)
    
    # Join total N per group (unweighted)
    n_group <- unweighted_ns() %>% rename(group = .group)
    wide_combined <- wide_pct %>%
      left_join(n_group, by = "group") %>%
      relocate(N_unweighted, .after = group)
    
    wide_combined
  })
  
  # ---------- Table (show all rows) ----------
  output$prop_table <- renderDT({
    wide <- wide_table()
    pct_cols <- names(wide)[!(names(wide) %in% c("group", "N_unweighted"))]
    
    datatable(
      wide, rownames = FALSE,
      options = list(
        paging = FALSE,     # show all rows
        dom = "t",          # table only (no search/paging UI)
        scrollX = TRUE
      )
    ) %>%
      formatPercentage(columns = pct_cols, digits = 1) %>%
      formatStyle(1, fontWeight = "bold")
  })
  
  # ---------- Downloads ----------
  # Table CSV
  output$dl_table_csv <- downloadHandler(
    filename = function() {
      # Make a readable file name using selected question label
      q_text <- analysis_data()$question_text
      safe <- gsub("[^A-Za-z0-9_\\-]+", "_", q_text)
      paste0("proportion_table_", safe, ".csv")
    },
    content = function(file) {
      wt <- wide_table()
      # Convert % columns from proportions (0–1) to % numbers with one decimal in the CSV?
      # We'll export as proportions (0–1) to keep it numeric; analysts can format in Excel if needed.
      readr::write_csv(wt, file, na = "")
    }
  )
  
  # Plot JPG
  output$dl_plot_jpg <- downloadHandler(
    filename = function() {
      q_text <- analysis_data()$question_text
      safe <- gsub("[^A-Za-z0-9_\\-]+", "_", q_text)
      paste0("proportion_plot_", safe, ".jpg")
    },
    content = function(file) {
      p <- prop_plot_obj()
      ggplot2::ggsave(
        filename = file, plot = p,
        device = "jpeg", width = 10, height = 7, units = "in", dpi = 300
      )
    }
  )
  
  # ---------- Debug panel ----------
  output$debug_text <- renderText({
    if (is.null(input$file)) return("No file uploaded yet.")
    wbinfo <- wb()
    df <- wbinfo$data
    vl <- wbinfo$varlab
    vv <- wbinfo$vallab
    ch <- choices()
    
    paste0(
      "File: ", input$file$name, "\n",
      "Sheets: ", paste(wbinfo$sheets, collapse = ", "), "\n\n",
      "[Data] rows=", nrow(df), " cols=", ncol(df), "\n",
      "Data columns: ", paste(names(df), collapse = ", "), "\n\n",
      "[Variable Labels] rows=", nrow(vl), " | cols=", paste(names(vl), collapse = ","), "\n",
      "[Value Labels] rows=", nrow(vv), " | cols=", paste(names(vv), collapse = ","), "\n\n",
      "[Classified counts] questions(@q* or q*)=", nrow(ch$q),
      " | groupings(non-@)=", nrow(ch$g),
      " | weights(@weight*)=", nrow(ch$w), "\n"
    )
  })
  
  output$debug_varlab <- renderDT({
    req(wb()); datatable(wb()$varlab %>% head(20), options = list(dom = "tip"), rownames = FALSE)
  })
  output$debug_vallab <- renderDT({
    req(wb()); datatable(wb()$vallab %>% head(20), options = list(dom = "tip"), rownames = FALSE)
  })
  output$debug_data_head <- renderDT({
    req(wb()); datatable(wb()$data %>% head(10), options = list(scrollX = TRUE, dom = "tip"), rownames = FALSE)
  })
}



shinyApp(ui, server)