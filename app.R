# app.R — Shiny front end (FULL LIST, NO CHART, Baseline = ALL non-clutch)
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(DT)
  library(ggplot2)
  library(plotly)
  source("02_clutch_analysis.R", local = TRUE)
})

latest_wnba_season <- wehoop:::most_recent_wnba_season()
default_end_season <- min(2025, latest_wnba_season)

ui <- fluidPage(
  titlePanel("Shot Success When It Matters Most: WNBA Clutchness Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("season_min", "Start Season", min = 2006, max = latest_wnba_season, value = 2006, step = 1, sep = ""),
      sliderInput("season_max", "End Season",   min = 2006, max = latest_wnba_season, value = default_end_season, step = 1, sep = ""),
      radioButtons("window_mode", "Clutch Window Mode",
                   choices = c("Q4 + OT last 3:00" = "Q4+OT",
                               "Endgame last N seconds" = "Endgame"),
                   selected = "Q4+OT"),
      numericInput("time_cut", "Time cutoff (seconds)", value = 180, min = 30, max = 600, step = 30),
      numericInput("margin_cut", "Margin cutoff (<= points)", value = 5, min = 1, max = 20, step = 1),
      checkboxInput("fg_only", "Field Goals Only (exclude FTs)", value = FALSE),
      numericInput("min_attempts", "Minimum Late_Tight attempts", value = 0, min = 0, max = 50, step = 1),
      numericInput("label_cut_pp", "Label threshold (+/- percentage points)", value = 5, min = 1, max = 30, step = 1),
      radioButtons("sort_mode", "Sort by",
                   choices = c("Delta pp (desc)" = "delta_desc",
                               "Late_Tight attempts (desc)" = "late_n_desc",
                               "Player (A-Z)" = "name_asc"),
                   selected = "delta_desc"),
      actionButton("run_btn", "Run Analysis", class = "btn-primary")
    ),
    mainPanel(
      wellPanel(
        tags$h4("Project Focus"),
        tags$p("This dashboard explores WNBA shot success in late, close-game situations using play-by-play data from wehoop."),
        tags$p(HTML("<b>Late Tight</b> uses the selected time and score-margin cutoffs. <b>Baseline</b> is all non-Late_Tight shots in the selected seasons.")),
        tags$p(HTML("<b>League clutchness</b> averages each player's <code>100 * (pct_Late_Tight - pct_Baseline)</code> within a season. Players need baseline attempts and must meet the selected minimum Late_Tight attempts.")),
        tags$p(HTML("<b>Data note:</b> this project uses WNBA seasons 2006 onward because those seasons have comparable quarter and clock coding for late-game clutch windows. The 2026 season is selectable if available in wehoop, but it may be incomplete."))
      ),
      tabsetPanel(
        tabPanel(
          "Story",
          br(),
          wellPanel(
            h4("Story Path"),
            tags$ol(
              tags$li("Define a fair clutch window and show how much data is included each season."),
              tags$li("Measure whether the average player performs better or worse in Late Tight moments than in baseline situations."),
              tags$li("Check whether the league's changing 3-point style explains clutch performance."),
              tags$li("Compare shot selection over time and inspect individual players as supporting evidence.")
            )
          ),
          fluidRow(
            column(6, wellPanel(h4("Selected Window"), uiOutput("window_summary"))),
            column(6, wellPanel(h4("Key Takeaway"), uiOutput("takeaway")))
          ),
          uiOutput("load_status"),
          h4("League Clutchness by Season"),
          tags$p("This first view answers the main question: in each season, was the average player better or worse in late, close-game shots than in their normal baseline shots?"),
          plotlyOutput("plot_efficiency", height = "320px"),
          br(),
          h4("League Clutch Sample Size by Season"),
          tags$p("This checks whether the clutch window has enough observations to trust each season-level comparison."),
          plotlyOutput("plot_volume", height = "280px"),
          br(),
          h4("League Clutch 3-Point Attempts and Makes"),
          tags$p("This shows whether the league is taking and making more clutch threes over time."),
          plotlyOutput("plot_three_volume", height = "300px"),
          br(),
          h4("Made Clutch 3s vs. League Clutchness"),
          wellPanel(uiOutput("three_correlation_summary")),
          plotlyOutput("plot_three_correlation", height = "320px"),
          br(),
          h4("Clutch Shot Selection Over Time"),
          tags$p("This final story chart shows how the mix of twos, threes, and free throws changes inside the clutch window."),
          plotlyOutput("plot_mix", height = "320px")
        ),
        tabPanel(
          "Players",
          br(),
          h4("Player Clutch vs. Baseline Efficiency"),
          plotlyOutput("plot_players", height = "420px"),
          br(),
          h4("All Players"),
          DTOutput("tbl_all")
        ),
        tabPanel(
          "Downloads",
          br(),
          h4("Download Results"),
          downloadButton("dl_labels", "Download Labels (CSV)"),
          downloadButton("dl_bytype", "Download By Shot Type (CSV)"),
          downloadButton("dl_league", "Download League Clutchness (CSV)"),
          downloadButton("dl_threes", "Download Clutch 3PT Summary (CSV)")
        )
      ),
      br(),
      tags$small("Data source: wehoop WNBA play-by-play and player box data.")
    )
  )
)

server <- function(input, output, session) {
  params <- eventReactive(input$run_btn, ignoreInit = TRUE, ignoreNULL = TRUE, {
    season_min <- min(input$season_min, input$season_max)
    season_max <- max(input$season_min, input$season_max)
    list(
      seasons = seq(season_min, season_max),
      margin_cut = input$margin_cut,
      time_cut_s = input$time_cut,
      min_attempts = input$min_attempts,
      fg_only = input$fg_only,
      window_mode = input$window_mode,
      label_cut_pp = input$label_cut_pp,
      sort_mode = input$sort_mode
    )
  })

  results <- reactive({
    p <- params()
    req(p)
    withProgress(message = "Running analysis...", value = 0.1, {
      res <- tryCatch(
        run_clutch(
          seasons = p$seasons,
          margin_cut = p$margin_cut,
          time_cut_s = p$time_cut_s,
          min_attempts = p$min_attempts,
          fg_only = p$fg_only,
          window_mode = p$window_mode,
          label_cut_pp = p$label_cut_pp
        ),
        error = function(e) {
          showNotification(
            paste(
              "Could not load one or more wehoop season files.",
              "Try a smaller year range or run again in a minute.",
              conditionMessage(e)
            ),
            type = "error",
            duration = NULL
          )
          NULL
        }
      )
      if (!is.null(res) && !is.null(res$load_status)) {
        failed_pbp <- unlist(res$load_status$failed_pbp_seasons[[1]])
        failed_box <- unlist(res$load_status$failed_box_seasons[[1]])

        if (length(failed_pbp) > 0) {
          showNotification(
            paste("Skipped PBP season(s) after retries:", paste(failed_pbp, collapse = ", ")),
            type = "warning",
            duration = NULL
          )
        }

        if (length(failed_box) > 0) {
          showNotification(
            paste("Player names may be missing for box-score season(s):", paste(failed_box, collapse = ", ")),
            type = "warning",
            duration = NULL
          )
        }
      }
      incProgress(0.8)
      res
    })
  })

  full_fmt <- reactive({
    p <- params()
    req(p)
    res <- results()
    req(res)
    res <- res$labeled_with_names
    validate(need(!is.null(res), "No results yet"))
    df <- res %>%
      mutate(across(starts_with("pct_"), ~round(.x*100, 1)),
             delta_pp = round(delta_pp, 1),
             player = coalesce(player_name, as.character(athlete_id_1))) %>%
      transmute(player,
                attempts_Baseline, pct_Baseline,
                attempts_Late_Tight, pct_Late_Tight,
                delta_pp, label)
    if (p$sort_mode == "delta_desc") {
      df <- df %>% arrange(desc(delta_pp))
    } else if (p$sort_mode == "late_n_desc") {
      df <- df %>% arrange(desc(attempts_Late_Tight))
    } else {
      df <- df %>% arrange(player)
    }
    df
  })

  output$window_summary <- renderUI({
    p <- params()
    req(p)
    tags$ul(
      tags$li(sprintf("Seasons: %s", paste(range(p$seasons), collapse = "-"))),
      tags$li(sprintf("Time cutoff: last %s seconds", p$time_cut_s)),
      tags$li(sprintf("Score margin: %s points or fewer", p$margin_cut)),
      tags$li(sprintf("Window mode: %s", p$window_mode)),
      tags$li(if (p$fg_only) "Field goals only" else "Field goals and free throws")
    )
  })

  output$takeaway <- renderUI({
    res <- results()
    req(res)
    yearly <- res$league_clutchness

    latest <- yearly %>%
      filter(season == max(season, na.rm = TRUE))

    strongest <- yearly %>%
      filter(!is.na(avg_player_delta_pp)) %>%
      slice_max(avg_player_delta_pp, n = 1, with_ties = FALSE)

    weakest <- yearly %>%
      filter(!is.na(avg_player_delta_pp)) %>%
      slice_min(avg_player_delta_pp, n = 1, with_ties = FALSE)

    if (nrow(latest) == 0 || is.na(latest$avg_player_delta_pp)) {
      return(tags$p("Run the analysis to compare league-wide average player clutchness."))
    }

    delta <- latest$avg_player_delta_pp
    direction <- if_else(delta >= 0, "above", "below")
    tags$div(
      tags$p(sprintf(
        "In %s, the average player clutch delta was %.1f percentage points %s baseline across %s included players.",
        latest$season, abs(delta), direction, latest$players_included
      )),
      if (nrow(strongest) > 0 && nrow(weakest) > 0) {
        tags$p(sprintf(
          "Across the selected range, the strongest clutch season was %s (%+.1f pp) and the weakest was %s (%+.1f pp).",
          strongest$season, strongest$avg_player_delta_pp, weakest$season, weakest$avg_player_delta_pp
        ))
      }
    )
  })

  output$load_status <- renderUI({
    res <- results()
    req(res)
    status <- res$load_status
    req(status)

    requested <- unlist(status$requested_seasons[[1]])
    loaded <- unlist(status$loaded_pbp_seasons[[1]])
    failed <- unlist(status$failed_pbp_seasons[[1]])

    tags$div(
      class = "well",
      tags$strong("Loaded seasons: "),
      tags$span(sprintf("%s of %s", length(loaded), length(requested))),
      if (length(failed) > 0) {
        tags$p(sprintf("Skipped after retries: %s", paste(failed, collapse = ", ")))
      } else {
        tags$p("All selected play-by-play seasons loaded successfully.")
      }
    )
  })

  output$plot_efficiency <- renderPlotly({
    res <- results()
    req(res)
    df <- res$league_clutchness %>%
      mutate(
        hover = sprintf(
          "Season: %s<br>Avg player clutch delta: %+.2f pp<br>Median player delta: %+.2f pp<br>Players included: %s<br>Included Late Tight attempts: %s",
          season,
          avg_player_delta_pp,
          median_player_delta_pp,
          players_included,
          total_late_tight_attempts
        )
      )

    p <- ggplot(df, aes(x = season, y = avg_player_delta_pp, group = 1, text = hover)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
      geom_col(aes(fill = avg_player_delta_pp >= 0), width = 0.72, alpha = 0.82) +
      geom_line(linewidth = 0.9, color = "gray25") +
      geom_point(size = 2.2, color = "gray15") +
      scale_x_continuous(breaks = sort(unique(df$season))) +
      scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#d95f0e"), guide = "none") +
      labs(
        x = NULL,
        y = "Average player clutch delta (pp)"
      ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest")
  })

  output$plot_volume <- renderPlotly({
    res <- results()
    req(res)
    df <- res$season_summary %>%
      filter(clutch_group == "Late Tight") %>%
      mutate(
        hover = sprintf(
          "Season: %s<br>Late Tight attempts: %s<br>Makes: %s<br>Shot success: %.1f%%<br>Points per attempt: %.3f",
          season,
          attempts,
          makes,
          100 * fg_pct,
          points_per_attempt
        )
      )

    p <- ggplot(df, aes(x = season, y = attempts, group = 1, text = hover)) +
      geom_col(width = 0.72, fill = "#4d4d4d", alpha = 0.82) +
      geom_line(linewidth = 0.9, color = "gray20") +
      geom_point(size = 2.2, color = "gray10") +
      scale_x_continuous(breaks = sort(unique(df$season))) +
      labs(x = NULL, y = "Late Tight shot attempts") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest")
  })

  output$plot_three_volume <- renderPlotly({
    res <- results()
    req(res)
    source_df <- res$three_point_summary %>%
      mutate(
        attempts_hover = sprintf(
          "Season: %s<br>Late Tight 3PT attempts: %s<br>Late Tight 3PT makes: %s<br>Late Tight 3PT%%: %.1f%%",
          season,
          clutch_3pt_attempts,
          clutch_3pt_makes,
          100 * clutch_3pt_pct
        ),
        makes_hover = sprintf(
          "Season: %s<br>Late Tight 3PT makes: %s<br>Late Tight 3PT attempts: %s<br>Late Tight 3PT%%: %.1f%%",
          season,
          clutch_3pt_makes,
          clutch_3pt_attempts,
          100 * clutch_3pt_pct
        )
      )

    df <- source_df %>%
      select(season, clutch_3pt_attempts, clutch_3pt_makes) %>%
      tidyr::pivot_longer(
        cols = c(clutch_3pt_attempts, clutch_3pt_makes),
        names_to = "metric",
        values_to = "count"
      ) %>%
      mutate(metric = recode(
        metric,
        clutch_3pt_attempts = "3PT attempts",
        clutch_3pt_makes = "3PT makes"
      )) %>%
      left_join(source_df %>% select(season, attempts_hover, makes_hover), by = "season") %>%
      mutate(hover = if_else(metric == "3PT attempts", attempts_hover, makes_hover))

    p <- ggplot(df, aes(x = season, y = count, color = metric, group = metric, text = hover)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.2) +
      scale_x_continuous(breaks = sort(unique(df$season))) +
      labs(x = NULL, y = "Late Tight 3PT shots", color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank())

    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest", legend = list(orientation = "h"))
  })

  output$three_correlation_summary <- renderUI({
    res <- results()
    req(res)
    df <- res$clutchness_three_correlation %>%
      filter(!is.na(avg_player_delta_pp), !is.na(clutch_3pt_makes))

    validate(need(nrow(df) >= 3, "Need at least three seasons with data to estimate a correlation."))

    r <- cor(df$clutch_3pt_makes, df$avg_player_delta_pp, use = "complete.obs")
    direction <- case_when(
      r >= 0.5 ~ "strong positive",
      r >= 0.2 ~ "weak positive",
      r <= -0.5 ~ "strong negative",
      r <= -0.2 ~ "weak negative",
      TRUE ~ "little linear"
    )

    tags$p(sprintf(
      "Across %s seasons, the season-level correlation between made clutch 3s and average player clutchness is r = %.2f, suggesting %s relationship. This is exploratory, not causal.",
      nrow(df), r, direction
    ))
  })

  output$plot_three_correlation <- renderPlotly({
    res <- results()
    req(res)
    df <- res$clutchness_three_correlation %>%
      filter(!is.na(avg_player_delta_pp), !is.na(clutch_3pt_makes)) %>%
      mutate(
        hover = sprintf(
          "Season: %s<br>Made Late Tight 3s: %s<br>Attempted Late Tight 3s: %s<br>3PT%%: %.1f%%<br>Avg player clutch delta: %+.2f pp",
          season,
          clutch_3pt_makes,
          clutch_3pt_attempts,
          100 * clutch_3pt_pct,
          avg_player_delta_pp
        )
      )

    validate(need(nrow(df) >= 2, "Need at least two seasons with data to draw the correlation plot."))

    model <- lm(avg_player_delta_pp ~ clutch_3pt_makes, data = df)
    line_df <- tibble(
      clutch_3pt_makes = seq(min(df$clutch_3pt_makes), max(df$clutch_3pt_makes), length.out = 100)
    ) %>%
      mutate(
        avg_player_delta_pp = predict(model, newdata = .),
        hover = sprintf(
          "Fitted correlation line<br>Made Late Tight 3s: %.1f<br>Predicted clutch delta: %+.2f pp",
          clutch_3pt_makes,
          avg_player_delta_pp
        )
      )

    zero_line <- list(
      type = "line",
      x0 = min(df$clutch_3pt_makes),
      x1 = max(df$clutch_3pt_makes),
      y0 = 0,
      y1 = 0,
      line = list(color = "gray", dash = "dash")
    )

    plot_ly() %>%
      add_lines(
        data = line_df,
        x = ~clutch_3pt_makes,
        y = ~avg_player_delta_pp,
        text = ~hover,
        hoverinfo = "text",
        name = "Fitted correlation line",
        line = list(color = "#d95f0e", width = 4)
      ) %>%
      add_markers(
        data = df,
        x = ~clutch_3pt_makes,
        y = ~avg_player_delta_pp,
        text = ~hover,
        hoverinfo = "text",
        name = "Season",
        marker = list(color = "#2b8cbe", size = 10, opacity = 0.85)
      ) %>%
      add_text(
        data = df,
        x = ~clutch_3pt_makes,
        y = ~avg_player_delta_pp,
        text = ~season,
        textposition = "top center",
        textfont = list(size = 11, color = "gray25"),
        hoverinfo = "skip",
        showlegend = FALSE
      ) %>%
      layout(
        hovermode = "closest",
        shapes = list(zero_line),
        xaxis = list(title = "Made Late Tight 3PT shots"),
        yaxis = list(title = "Average player clutch delta (pp)"),
        legend = list(orientation = "h")
      )
  })

  output$plot_mix <- renderPlotly({
    res <- results()
    req(res)
    df <- res$shot_mix %>%
      filter(clutch_group == "Late Tight") %>%
      mutate(
        hover = sprintf(
          "Season: %s<br>Shot type: %s<br>Attempts: %s<br>Share of Late Tight attempts: %.1f%%",
          season,
          shot_type,
          attempts,
          100 * share
        )
      )

    p <- ggplot(df, aes(x = factor(season), y = share, fill = shot_type, text = hover)) +
      geom_col(width = 0.72) +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
      labs(x = NULL, y = "Share of Late Tight attempts", fill = "Shot type") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")

    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest", legend = list(orientation = "h"))
  })

  output$plot_players <- renderPlotly({
    df <- full_fmt() %>%
      filter(attempts_Late_Tight >= input$min_attempts) %>%
      mutate(
        label = factor(label, levels = c("Clutch", "Steady", "Anti-clutch", "Low sample")),
        hover = sprintf(
          "Player: %s<br>Baseline success: %.1f%%<br>Late Tight success: %.1f%%<br>Delta: %+.1f pp<br>Late Tight attempts: %s<br>Label: %s",
          player,
          pct_Baseline,
          pct_Late_Tight,
          delta_pp,
          attempts_Late_Tight,
          label
        )
      )

    validate(need(nrow(df) > 0, "No players match the current filters."))

    p <- ggplot(df, aes(x = pct_Baseline, y = pct_Late_Tight, color = label, size = attempts_Late_Tight, text = hover)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray55") +
      geom_point(alpha = 0.75) +
      scale_x_continuous(labels = function(x) paste0(round(x), "%")) +
      scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
      labs(
        x = "Baseline shot success",
        y = "Late Tight shot success",
        color = "Label",
        size = "Late Tight attempts"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")

    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest", legend = list(orientation = "h"))
  })

  output$tbl_all <- renderDT({
    df <- full_fmt()
    datatable(
      df,
      options = list(
        pageLength = 50,
        lengthMenu = c(25, 50, 100, 200, 500),
        scrollX = TRUE,
        dom = 'Bfrtip'
      )
    )
  })

  output$dl_labels <- downloadHandler(
    filename = function() sprintf("player_clutch_labels_with_names.csv"),
    content = function(file) {
      res <- results()$labeled_with_names
      readr::write_csv(res, file)
    }
  )

  output$dl_bytype <- downloadHandler(
    filename = function() sprintf("player_clutch_by_type_with_names.csv"),
    content = function(file) {
      res <- results()$by_type_with_names
      readr::write_csv(res, file)
    }
  )

  output$dl_league <- downloadHandler(
    filename = function() sprintf("league_clutchness_by_season.csv"),
    content = function(file) {
      res <- results()$league_clutchness
      readr::write_csv(res, file)
    }
  )

  output$dl_threes <- downloadHandler(
    filename = function() sprintf("league_clutch_3pt_by_season.csv"),
    content = function(file) {
      res <- results()$three_point_summary
      readr::write_csv(res, file)
    }
  )
}

shinyApp(ui, server)
