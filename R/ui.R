# ui.R - User interface definition

ui <- fluidPage(
  titlePanel("Scottish Climate Survey – Weighted proportions by group"),
  
  # Top panel for file upload
  fluidRow(
    column(6,
           fileInput("file", "Upload data workbook (.xlsx)",
                     accept = ".xlsx", buttonLabel = "Browse...",
                     placeholder = "Choose Scottish Climate Survey - 2024 data + labels.xlsx"
           ),
           helpText("Max file size: 10 MB. Expects sheets: Data, Variable Labels, Value Labels.")
    ),
    column(6,
           checkboxInput("debug", "Show debug panel", FALSE)
    )
  ),
  tags$hr(),
  
  # Main tabset
  tabsetPanel(
    # Raw plots tab
    tabPanel("Plots - raw",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("var_select_ui"),
                 uiOutput("grp1_select_ui"),
                 uiOutput("grp2_select_ui"),
                 uiOutput("wgt_select_ui"),
                 tags$hr(),
                 checkboxInput("show_counts", "Show unweighted Ns in header", TRUE),
                 checkboxInput("drop_empty_groups", "Drop groups with all-missing responses", TRUE),
                 tags$hr(),
                 h5("Exclude response levels"),
                 checkboxGroupInput("exclude_levels", NULL,
                                    choices = c("Don't know", "Prefer not to say", "Not stated"),
                                    selected = character(0)
                 ),
                 tags$hr(),
                 selectInput("palette", "Likert colour palette",
                             choices = c(
                               "Red–Yellow–Green (RdYlGn)" = "RdYlGn",
                               "Red–Blue (RdBu)" = "RdBu",
                               "Purple–Orange (PuOr)" = "PuOr",
                               "Brown–Blue-Green (BrBG)" = "BrBG",
                               "Blue–Red (BuRd, reverse RdBu)" = "BuRd"
                             ),
                             selected = "RdYlGn"
                 ),
                 checkboxInput("palette_reverse", "Reverse colours (flip ends)", FALSE),
                 width = 4
               ),
               mainPanel(
                 uiOutput("question_header"),
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
                 DTOutput("prop_table"),
                 width = 8
               )
             )
    ),
    
    # Grouped plots tab
    tabPanel("Plots - grouped",
             sidebarLayout(
               sidebarPanel(
                 h5("Variable of Interest (multi-select question)"),
                 uiOutput("var_select_grouped_ui"),
                 tags$hr(),
                 h5("Primary Grouping Variable"),
                 uiOutput("grp1_select_grouped_ui"),
                 tags$hr(),
                 h5("Weighting Variable"),
                 uiOutput("wgt_select_grouped_ui"),
                 tags$hr(),
                 h5("Exclude response levels"),
                 checkboxGroupInput("exclude_levels_grouped", NULL,
                                    choices = c("Don't know", "Prefer not to say", "Not stated"),
                                    selected = character(0)
                 ),
                 checkboxInput("drop_empty_groups_grouped", "Drop groups with all-missing responses", TRUE),
                 tags$hr(),
                 actionButton("compute_grouped", "Compute Grouped Plot", class = "btn-primary"),
                 width = 4
               ),
               mainPanel(
                 h4("Grouped Options Table"),
                 DTOutput("grouped_table"),
                 tags$hr(),
                 h4("Grouped Plot (coming soon)"),
                 plotOutput("grouped_plot", height = "600px")
               )
             )
    ),
    
    # Statistics tab
    tabPanel("Statistics",
             sidebarLayout(
               sidebarPanel(
                 h5("Exclude response levels"),
                 checkboxGroupInput("exclude_levels_stats", NULL,
                                    choices = c("Don't know", "Prefer not to say", "Not stated"),
                                    selected = character(0)
                 ),
                 tags$hr(),
                 checkboxInput("drop_empty_groups_stats", "Drop groups with all-missing responses", TRUE),
                 tags$hr(),
                 h5("Variable of Interest (Question)"),
                 uiOutput("var_select_stats_ui"),
                 tags$hr(),
                 h5("Group levels for Variable of Interest"),
                 lapply(1:5, function(i) {
                   tagList(
                     textInput(paste0("var_group_name_", i), paste("New level", i, "name:"), ""),
                     uiOutput(paste0("var_group_levels_ui_", i))
                   )
                 }),
                 tags$hr(),
                 h5("Primary Grouping Variable"),
                 uiOutput("grp1_select_stats_ui"),
                 tags$hr(),
                 h5("Group levels for Primary Grouping Variable"),
                 lapply(1:5, function(i) {
                   tagList(
                     textInput(paste0("grp_group_name_", i), paste("New group", i, "name:"), ""),
                     uiOutput(paste0("grp_group_levels_ui_", i))
                   )
                 }),
                 tags$hr(),
                 h5("Weighting Variable"),
                 uiOutput("wgt_select_stats_ui"),
                 tags$hr(),
                 actionButton("compute_stats", "Compute Statistics", class = "btn-primary"),
                 width = 4
               ),
               mainPanel(
                 h4("Chi-squared test results"),
                 verbatimTextOutput("chisq_output"),
                 DTOutput("chisq_table"),
                 tags$hr(),
                 h4("Pairwise comparisons (Bonferroni)"),
                 DTOutput("pairwise_table")
               )
             )
    )
  )
)