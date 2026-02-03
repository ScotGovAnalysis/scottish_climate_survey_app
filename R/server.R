# server.R - Server logic

server <- function(input, output, session) {
  
  # Workbook reactive
  wb <- reactive({
    req(input$file)
    read_workbook(input$file$datapath, input$file$size)
  })
  
  # Choices reactive
  choices <- reactive({
    req(wb())
    classify_variables(wb()$data, wb()$varlab)
  })
  
  # ========== RAW PLOTS TAB ==========
  
  # UI outputs for raw plots
  output$var_select_ui <- renderUI({
    create_var_select_ui(wb(), choices()$q, "var", "Variable of interest (question):")
  })
  
  output$grp1_select_ui <- renderUI({
    create_var_select_ui(wb(), choices()$g, "grp1", "Primary grouping variable:")
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
    create_weight_select_ui(wb(), choices()$w, "wgt")
  })
  
  # Analysis data
  analysis_data <- reactive({
    req(wb(), input$var, input$grp1)
    prepare_analysis_data(
      wb()$data, wb()$varlab, wb()$vallab,
      input$var, input$grp1, input$grp2, input$wgt,
      input$drop_empty_groups
    )
  })
  
  # Survey design
  design_re <- reactive({
    create_survey_design(analysis_data()$df)
  })
  
  design_filtered <- reactive({
    filter_survey_design(design_re(), input$exclude_levels)
  })
  
  # Proportions and counts
  prop_df <- reactive({
    calculate_proportions(design_filtered())
  })
  
  unweighted_ns <- reactive({
    calculate_unweighted_ns(design_filtered())
  })
  
  # Question header
  output$question_header <- renderUI({
    create_question_header(
      analysis_data()$question_text,
      wb()$varlab,
      input$grp1, input$grp2, input$wgt,
      input$show_counts,
      unweighted_ns(),
      input$exclude_levels
    )
  })
  
  # Plot object
  prop_plot_obj <- reactive({
    create_proportion_plot(
      prop_df(),
      input$palette,
      input$palette_reverse
    )
  })
  
  output$prop_plot <- renderPlot({ prop_plot_obj() })
  
  # Wide table
  wide_table <- reactive({
    create_wide_table(prop_df(), unweighted_ns())
  })
  
  output$prop_table <- renderDT({
    render_proportion_table(wide_table())
  })
  
  # Downloads
  output$dl_table_csv <- downloadHandler(
    filename = function() {
      create_safe_filename(analysis_data()$question_text, "proportion_table", "csv")
    },
    content = function(file) {
      readr::write_csv(wide_table(), file, na = "")
    }
  )
  
  output$dl_plot_jpg <- downloadHandler(
    filename = function() {
      create_safe_filename(analysis_data()$question_text, "proportion_plot", "jpg")
    },
    content = function(file) {
      ggplot2::ggsave(
        filename = file, plot = prop_plot_obj(),
        device = "jpeg", width = 10, height = 7, units = "in", dpi = 300
      )
    }
  )
  
  # ========== STATISTICS TAB ==========
  
  output$var_select_stats_ui <- renderUI({
    create_var_select_ui(wb(), choices()$q, "var_stats", "Variable of interest (question):")
  })
  
  output$grp1_select_stats_ui <- renderUI({
    create_var_select_ui(wb(), choices()$g, "grp1_stats", "Primary grouping variable:")
  })
  
  output$wgt_select_stats_ui <- renderUI({
    create_weight_select_ui(wb(), choices()$w, "wgt_stats")
  })
  
  # Variable and group levels
  var_levels <- reactive({
    req(input$var_stats)
    get_variable_levels(input$var_stats, wb()$vallab, wb()$data)
  })
  
  grp_levels <- reactive({
    req(input$grp1_stats)
    get_variable_levels(input$grp1_stats, wb()$vallab, wb()$data)
  })
  
  # Render level selection UIs
  for (i in 1:5) {
    local({
      idx <- i
      output[[paste0("var_group_levels_ui_", idx)]] <- renderUI({
        req(var_levels())
        selectInput(
          paste0("var_group_levels_", idx),
          paste("Select levels for new group", idx),
          choices = var_levels(),
          selected = NULL,
          multiple = TRUE,
          width = "100%"
        )
      })
      
      output[[paste0("grp_group_levels_ui_", idx)]] <- renderUI({
        req(grp_levels())
        selectInput(
          paste0("grp_group_levels_", idx),
          paste("Select levels for new group", idx),
          choices = grp_levels(),
          selected = NULL,
          multiple = TRUE,
          width = "100%"
        )
      })
    })
  }
  
  # Compute statistics
  observeEvent(input$compute_stats, {
    req(wb(), input$var_stats, input$grp1_stats)
    
    # Collect grouping mappings
    var_map <- collect_group_mappings(input, "var_group_name_", "var_group_levels_", 5)
    grp_map <- collect_group_mappings(input, "grp_group_name_", "grp_group_levels_", 5)
    
    # Prepare data and create survey design (returns design object directly)
    dsgn <- prepare_stats_data(
      wb()$data, wb()$vallab,
      input$var_stats, input$grp1_stats, input$wgt_stats,
      var_map, grp_map,
      input$exclude_levels_stats,
      input$drop_empty_groups_stats
    )
    
    # check that there are >1 levels in the design data
    lvls_check <- length(unique(
      dsgn$variables[[input$var_stats]]))>1

  
    # Run chi-square test
    chisq_result <- run_chisq_test(dsgn, lvls_check)
    
    
    # Render chi-square output
    output$chisq_output <- renderText({
      format_chisq_result(chisq_result)
    })
    
    # Render proportion table
    output$chisq_table <- DT::renderDT({
      render_chisq_table(chisq_result$props_wide)
    })
    
    # Run pairwise tests if significant
    if (chisq_result$significant) {
      pairwise_result <- run_pairwise_tests(dsgn, alpha = 0.05)
      
      output$pairwise_table <- DT::renderDT({
        render_pairwise_table(pairwise_result)
      })
      
      output$chisq_output <- renderText({
        paste0(format_chisq_result(chisq_result),
               "\nPairwise Wald tests shown below (Bonferroni corrected).")
      })
    } else {
      output$pairwise_table <- DT::renderDT({
        DT::datatable(
          data.frame(Message = "Overall test not significant at alpha = 0.05; pairwise comparisons not computed."),
          options = list(dom = "t"), rownames = FALSE
        )
      })
    }
  })
  
  
  # ========== GROUPED PLOTS TAB ==========
  
  grouped_prefixes <- reactive({
    req(wb())
    find_grouped_prefixes(wb()$data, wb()$varlab)
  })
  
  output$var_select_grouped_ui <- renderUI({
    pf <- grouped_prefixes()
    if (nrow(pf) == 0) {
      return(tagList(
        h5("Variable of Interest (multi-select question)"),
        div(class = "text-danger", "No '@' multi-select questions found.")
      ))
    }
    selectInput(
      "var_grouped_root", "Variable of Interest (multi-select question):",
      choices = setNames(pf$root, pf$choice_lab),
      selected = pf$root[1],
      width = "100%"
    )
  })
  
  output$grp1_select_grouped_ui <- renderUI({
    create_var_select_ui(wb(), choices()$g, "grp1_grouped", "Primary Grouping Variable:")
  })
  
  output$wgt_select_grouped_ui <- renderUI({
    create_weight_select_ui(wb(), choices()$w, "wgt_grouped")
  })
  
  # Compute grouped analysis
  observeEvent(input$compute_grouped, {
    req(wb(), input$var_grouped_root, input$grp1_grouped)
    
    grouped_result <- compute_grouped_analysis(
      wb()$data, wb()$varlab, wb()$vallab,
      input$var_grouped_root,
      input$grp1_grouped,
      input$wgt_grouped,
      input$exclude_levels_grouped,
      input$drop_empty_groups_grouped
    )
    
    output$grouped_table <- DT::renderDT({
      render_grouped_table(grouped_result$wide_df)
    })
    
    output$grouped_plot <- renderPlot({
      create_grouped_plot(grouped_result$wide_df)
    })
  })
  
  # Debug reactive for variable change
  observeEvent(input$var_grouped_root, {
    req(wb(), input$var_grouped_root)
    debug_grouped_children(input$var_grouped_root, wb()$data)
  })
}