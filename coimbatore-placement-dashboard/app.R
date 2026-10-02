# =============================================================================
# app.R - Coimbatore Higher Education Placement Insights Dashboard
# Run with: shiny::runApp()
# =============================================================================

if (!exists("placement")) source("global.R")

app_theme <- bs_theme(
  version   = 5,
  primary   = "#4f46e5",
  secondary = "#64748b",
  success   = "#10b981",
  info      = "#0ea5e9",
  warning   = "#f59e0b",
  bg        = "#f1f5f9",
  fg        = "#0f172a"
)

nav_label <- function(ic, txt) tagList(icon(ic), span(txt))

# ---- UI ---------------------------------------------------------------------
ui <- page_sidebar(
  title    = "Coimbatore Higher Education Placement Insights Dashboard",
  theme    = app_theme,
  fillable = FALSE,

  sidebar = sidebar(
    width = 320, bg = "#0f172a", fg = "#e2e8f0",

    div(class = "brand",
        div(class = "brand-mark", icon("graduation-cap")),
        div(div(class = "brand-title", "PlacementIQ"),
            div(class = "brand-sub", "Coimbatore Higher Education"))),

    div(class = "side-label", "Navigate"),
    radioButtons(
      "nav", NULL,
      choiceNames = list(
        nav_label("chart-pie", "Executive Summary"),
        nav_label("chart-line", "Placement Insights"),
        nav_label("users", "College Comparison"),
        nav_label("trophy", "Top Performers")
      ),
      choiceValues = c("summary", "insights", "compare", "top"),
      selected = "summary"
    ),

    div(class = "side-label", "Global Filters"),
    sliderInput("years", "Placement years", min = min(YEARS), max = max(YEARS),
                value = c(min(YEARS), max(YEARS)), step = 1, sep = ""),
    checkboxGroupInput("types", "Institution type", choices = TYPES, selected = TYPES),
    checkboxGroupInput("streams", "Stream", choices = STREAMS, selected = STREAMS),

    div(class = "side-actions",
        actionButton("reset", "Reset filters", class = "btn btn-sm btn-outline-light w-100"),
        downloadButton("dl_data", "Export filtered data", class = "btn btn-sm btn-primary w-100 mt-2")),

    div(class = "side-foot",
        "All figures are simulated for demonstration and do not represent official college statistics.")
  ),

  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),

  navset_hidden(
    id = "main_tabs",

    # ---- Tab 1: Executive Summary ------------------------------------------
    nav_panel_hidden(
      "summary",
      div(class = "page-head",
          h2("Executive Summary"),
          p("High-level placement performance across the filtered colleges, streams and years.")),
      uiOutput("kpis"),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Average package trend by stream (LPA)"),
             card_body(plotlyOutput("plot_trend", height = "340px"))),
        card(card_header("Placement rate by institution type"),
             card_body(plotlyOutput("plot_rate_tier", height = "340px")))
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Top 5 colleges by average package"),
             card_body(plotlyOutput("plot_top5", height = "340px"))),
        card(card_header("Share of placed students by stream"),
             card_body(plotlyOutput("plot_mix", height = "340px")))
      )
    ),

    # ---- Tab 2: Placement Insights -----------------------------------------
    nav_panel_hidden(
      "insights",
      div(class = "page-head",
          h2("Placement Insights"),
          p("Salary package distribution across streams, built from simulated individual offers.")),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Package spread by stream (boxplot, LPA)"),
             card_body(plotlyOutput("plot_box", height = "380px"))),
        card(card_header("Package distribution curves (density)"),
             card_body(plotlyOutput("plot_density", height = "380px")))
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Stream salary summary"),
             card_body(DTOutput("tbl_stream"))),
        card(card_header("Average package by stream and tier (LPA)"),
             card_body(plotlyOutput("plot_tier_stream", height = "300px")))
      )
    ),

    # ---- Tab 3: College Comparison -----------------------------------------
    nav_panel_hidden(
      "compare",
      div(class = "page-head",
          h2("College Comparison Engine"),
          p("Pick 2 to 3 colleges to compare side by side. Sidebar filters still apply.")),
      card(
        card_body(
          selectizeInput(
            "cmp_colleges", "Colleges to compare (2 to 3)",
            choices = ALL_COLLEGES, selected = DEFAULT_COMPARE, multiple = TRUE,
            width = "100%",
            options = list(maxItems = 3, placeholder = "Choose 2 to 3 colleges")
          )
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Placement rate by year"),
             card_body(plotlyOutput("cmp_rate_year", height = "340px"))),
        card(card_header("Placement rate by stream"),
             card_body(plotlyOutput("cmp_rate_stream", height = "340px")))
      ),
      layout_columns(
        col_widths = c(5, 7),
        card(card_header("Average vs highest package (LPA)"),
             card_body(plotlyOutput("cmp_pkg", height = "340px"))),
        card(card_header("Side-by-side scorecard"),
             card_body(DTOutput("cmp_table")))
      )
    ),

    # ---- Tab 4: Top Performers ---------------------------------------------
    nav_panel_hidden(
      "top",
      div(class = "page-head",
          h2("Top Performers"),
          p("Rank colleges with custom budget, stream (sidebar) and placement-rate filters.")),
      card(
        card_body(
          layout_column_wrap(
            width = 1/4,
            selectInput("top_n", "Show", choices = c("Top 5" = 5, "Top 10" = 10), selected = 10),
            selectInput("rank_by", "Rank by",
                        choices = c("Average package" = "pkg",
                                    "Placement rate" = "rate",
                                    "Highest package" = "high"),
                        selected = "pkg"),
            sliderInput("min_rate", "Minimum placement rate (%)", min = 0, max = 100,
                        value = 60, step = 1),
            sliderInput("max_fee", paste0("Maximum annual fee (", RS, " lakh)"),
                        min = 0.4, max = 3.5, value = 3.5, step = 0.05)
          )
        )
      ),
      card(card_header("Leaderboard"),
           card_body(DTOutput("tbl_top"))),
      card(card_header(paste0("Value map: annual fee vs average package (bubble size = students)")),
           card_body(plotlyOutput("plot_value", height = "380px")))
    )
  )
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Sidebar navigation -> hidden tab set
  observeEvent(input$nav, {
    nav_select("main_tabs", selected = input$nav)
  })

  observeEvent(input$reset, {
    updateSliderInput(session, "years", value = c(min(YEARS), max(YEARS)))
    updateCheckboxGroupInput(session, "types", selected = TYPES)
    updateCheckboxGroupInput(session, "streams", selected = STREAMS)
  })

  # Core filtered data (reacts to sidebar filters)
  filtered <- reactive({
    validate(
      need(length(input$types) > 0, "Select at least one institution type in the sidebar."),
      need(length(input$streams) > 0, "Select at least one stream in the sidebar.")
    )
    d <- placement %>%
      filter(year >= input$years[1], year <= input$years[2],
             institution_type %in% input$types,
             stream %in% input$streams)
    validate(need(nrow(d) > 0, "No records match the current filters."))
    d
  })

  offers_f <- reactive({
    offers %>% filter(record_id %in% filtered()$record_id)
  })

  output$dl_data <- downloadHandler(
    filename = function() paste0("placement_filtered_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(filtered(), file, row.names = FALSE)
  )

  # ===========================================================================
  # Tab 1: Executive Summary
  # ===========================================================================
  output$kpis <- renderUI({
    d <- filtered()
    avg_pkg <- wavg(d$avg_package, d$placed)
    rate    <- sum(d$placed) / sum(d$students) * 100
    by_stream <- d %>%
      group_by(stream) %>%
      summarise(pkg = wavg(avg_package, placed), .groups = "drop") %>%
      arrange(desc(pkg))
    top1 <- as.character(by_stream$stream[1])
    runner <- if (nrow(by_stream) > 1)
      sprintf("#2 %s (%s%.2f LPA)", as.character(by_stream$stream[2]), RS, by_stream$pkg[2])
    else "Only one stream selected"
    n_col <- n_distinct(d$college)

    layout_column_wrap(
      width = 1/4,
      value_box(
        title = "Average Package",
        value = sprintf("%s%.2f LPA", RS, avg_pkg),
        p(sprintf("Placement-weighted, %d-%d", input$years[1], input$years[2])),
        showcase = icon("money-bill-wave"),
        theme = value_box_theme(bg = "#4f46e5", fg = "#ffffff")
      ),
      value_box(
        title = "Overall Placement Rate",
        value = sprintf("%.1f%%", rate),
        p(sprintf("%s of %s students placed",
                  format(sum(d$placed), big.mark = ","), format(sum(d$students), big.mark = ","))),
        showcase = icon("graduation-cap"),
        theme = value_box_theme(bg = "#0ea5e9", fg = "#ffffff")
      ),
      value_box(
        title = "Top Performing Stream",
        value = top1,
        p(sprintf("%s%.2f LPA avg", RS, by_stream$pkg[1])),
        p(runner),
        showcase = icon("trophy"),
        theme = value_box_theme(bg = "#10b981", fg = "#ffffff")
      ),
      value_box(
        title = "Total Sampled Colleges",
        value = n_col,
        p(sprintf("of %d in the dataset", n_distinct(placement$college))),
        showcase = icon("building"),
        theme = value_box_theme(bg = "#f59e0b", fg = "#ffffff")
      )
    )
  })

  output$plot_trend <- renderPlotly({
    d <- filtered() %>%
      group_by(year, stream) %>%
      summarise(pkg = wavg(avg_package, placed), .groups = "drop")
    plot_ly(d, x = ~year, y = ~pkg, color = ~stream, colors = STREAM_COLORS,
            type = "scatter", mode = "lines+markers",
            line = list(width = 3), marker = list(size = 9),
            hovertemplate = paste0("<b>%{fullData.name}</b><br>%{x}: ", RS, "%{y:.2f} LPA<extra></extra>")) %>%
      layout(xaxis = list(title = "", dtick = 1, tickformat = "d"),
             yaxis = list(title = "Weighted average package (LPA)")) %>%
      style_plot()
  })

  output$plot_rate_tier <- renderPlotly({
    d <- filtered() %>%
      group_by(year, institution_type) %>%
      summarise(rate = sum(placed) / sum(students) * 100, .groups = "drop")
    plot_ly(d, x = ~factor(year), y = ~rate, color = ~institution_type, colors = TIER_COLORS,
            type = "bar",
            hovertemplate = "<b>%{fullData.name}</b><br>%{x}: %{y:.1f}%<extra></extra>") %>%
      layout(barmode = "group",
             xaxis = list(title = ""),
             yaxis = list(title = "Placement rate", range = c(0, 100), ticksuffix = "%")) %>%
      style_plot()
  })

  output$plot_top5 <- renderPlotly({
    d <- filtered() %>%
      group_by(short_name) %>%
      summarise(pkg = wavg(avg_package, placed), rate = sum(placed) / sum(students) * 100,
                .groups = "drop") %>%
      arrange(desc(pkg)) %>%
      head(5)
    d$short_name <- factor(d$short_name, levels = rev(d$short_name))
    plot_ly(d, x = ~pkg, y = ~short_name, type = "bar", orientation = "h",
            marker = list(color = "#4f46e5"),
            text = ~sprintf("%s%.2f LPA", RS, pkg), textposition = "outside",
            hovertext = ~sprintf("<b>%s</b><br>Avg package: %s%.2f LPA<br>Placement rate: %.1f%%",
                                 short_name, RS, pkg, rate),
            hoverinfo = "text") %>%
      layout(xaxis = list(title = "Weighted average package (LPA)", rangemode = "tozero"),
             yaxis = list(title = ""),
             showlegend = FALSE) %>%
      style_plot()
  })

  output$plot_mix <- renderPlotly({
    d <- filtered() %>%
      group_by(stream) %>%
      summarise(placed = sum(placed), .groups = "drop") %>%
      mutate(stream = as.character(stream))
    plot_ly(d, labels = ~stream, values = ~placed, type = "pie", hole = 0.6,
            marker = list(colors = unname(STREAM_COLORS[d$stream]),
                          line = list(color = "#ffffff", width = 2)),
            textinfo = "percent+label",
            hovertemplate = "<b>%{label}</b><br>%{value:,} placed students<extra></extra>") %>%
      layout(showlegend = FALSE) %>%
      style_plot()
  })

  # ===========================================================================
  # Tab 2: Placement Insights
  # ===========================================================================
  output$plot_box <- renderPlotly({
    o <- offers_f()
    plot_ly(o, x = ~stream, y = ~package, color = ~stream, colors = STREAM_COLORS,
            type = "box", boxmean = TRUE, boxpoints = "outliers",
            marker = list(opacity = 0.5, size = 4)) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Package (LPA)"),
             showlegend = FALSE) %>%
      style_plot()
  })

  output$plot_density <- renderPlotly({
    o <- offers_f()
    xmax <- max(o$package) * 1.05
    p <- plot_ly()
    drawn <- 0
    for (s in intersect(STREAMS, unique(as.character(o$stream)))) {
      x <- o$package[o$stream == s]
      if (length(x) < 5) next
      dn <- density(x, adjust = 1.1, from = 0, to = xmax)
      p <- add_trace(p, x = dn$x, y = dn$y, type = "scatter", mode = "lines",
                     fill = "tozeroy", name = s,
                     line = list(color = STREAM_COLORS[[s]], width = 2.5),
                     fillcolor = alpha_col(STREAM_COLORS[[s]], 0.25),
                     hovertemplate = paste0("<b>", s, "</b><br>", RS, "%{x:.1f} LPA<extra></extra>"))
      drawn <- drawn + 1
    }
    validate(need(drawn > 0, "Not enough offers to draw distribution curves for these filters."))
    p %>%
      layout(xaxis = list(title = "Package (LPA)", rangemode = "tozero"),
             yaxis = list(title = "Density", showticklabels = FALSE)) %>%
      style_plot()
  })

  output$tbl_stream <- renderDT({
    o <- offers_f()
    d <- filtered()
    s <- o %>%
      group_by(Stream = stream) %>%
      summarise(Offers = n(), Median = median(package), Mean = mean(package),
                P90 = unname(quantile(package, 0.9)), .groups = "drop")
    h <- d %>%
      group_by(Stream = stream) %>%
      summarise(Highest = max(highest_package),
                Rate = sum(placed) / sum(students) * 100, .groups = "drop")
    df <- left_join(s, h, by = "Stream") %>%
      mutate(Stream = as.character(Stream)) %>%
      transmute(Stream,
                `Sampled Offers` = Offers,
                `Median (LPA)` = Median,
                `Mean (LPA)` = Mean,
                `P90 (LPA)` = P90,
                `Highest (LPA)` = Highest,
                `Placement %` = Rate)
    datatable(df, rownames = FALSE, selection = "none", class = "stripe hover compact",
              options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE)) %>%
      formatRound(c("Median (LPA)", "Mean (LPA)", "P90 (LPA)", "Highest (LPA)"), 2) %>%
      formatRound("Placement %", 1) %>%
      formatRound("Sampled Offers", 0, mark = ",")
  })

  output$plot_tier_stream <- renderPlotly({
    d <- filtered() %>%
      group_by(stream, institution_type) %>%
      summarise(pkg = wavg(avg_package, placed), .groups = "drop")
    plot_ly(d, x = ~stream, y = ~pkg, color = ~institution_type, colors = TIER_COLORS,
            type = "bar",
            hovertemplate = paste0("<b>%{fullData.name}</b><br>%{x}: ", RS, "%{y:.2f} LPA<extra></extra>")) %>%
      layout(barmode = "group",
             xaxis = list(title = ""),
             yaxis = list(title = "Average package (LPA)")) %>%
      style_plot()
  })

  # ===========================================================================
  # Tab 3: College Comparison
  # ===========================================================================
  cmp <- reactive({
    sel <- input$cmp_colleges
    validate(
      need(length(sel) >= 2, "Select at least 2 colleges (maximum 3) to compare."),
      need(length(sel) <= 3, "Please select no more than 3 colleges.")
    )
    d <- filtered() %>% filter(college %in% sel)
    validate(need(nrow(d) > 0,
                  "None of the selected colleges have records for the current sidebar filters."))
    lv <- COLLEGES$short[match(sel, COLLEGES$college)]
    d$short_name <- factor(d$short_name, levels = lv)
    d
  })

  cmp_palette <- reactive({
    lv <- levels(cmp()$short_name)
    setNames(COMPARE_COLORS[seq_along(lv)], lv)
  })

  output$cmp_rate_year <- renderPlotly({
    d <- cmp()
    g <- d %>%
      group_by(short_name, year) %>%
      summarise(rate = sum(placed) / sum(students) * 100, .groups = "drop")
    plot_ly(g, x = ~factor(year), y = ~rate, color = ~short_name, colors = cmp_palette(),
            type = "bar", text = ~sprintf("%.1f%%", rate), textposition = "outside",
            hovertemplate = "<b>%{fullData.name}</b><br>%{x}: %{y:.1f}%<extra></extra>") %>%
      layout(barmode = "group", xaxis = list(title = ""),
             yaxis = list(title = "Placement rate", range = c(0, 108), ticksuffix = "%")) %>%
      style_plot()
  })

  output$cmp_rate_stream <- renderPlotly({
    d <- cmp()
    g <- d %>%
      group_by(short_name, stream) %>%
      summarise(rate = sum(placed) / sum(students) * 100, .groups = "drop") %>%
      mutate(stream = as.character(stream))
    plot_ly(g, x = ~stream, y = ~rate, color = ~short_name, colors = cmp_palette(),
            type = "bar", text = ~sprintf("%.1f%%", rate), textposition = "outside",
            hovertemplate = "<b>%{fullData.name}</b><br>%{x}: %{y:.1f}%<extra></extra>") %>%
      layout(barmode = "group", xaxis = list(title = ""),
             yaxis = list(title = "Placement rate", range = c(0, 108), ticksuffix = "%")) %>%
      style_plot()
  })

  output$cmp_pkg <- renderPlotly({
    d <- cmp()
    g <- d %>%
      group_by(short_name) %>%
      summarise(avg = wavg(avg_package, placed), high = max(highest_package), .groups = "drop") %>%
      mutate(short_name = as.character(short_name))
    plot_ly(g, x = ~short_name, y = ~avg, type = "bar", name = "Average package",
            marker = list(color = "#6366f1"),
            hovertemplate = paste0("<b>%{x}</b><br>Average: ", RS, "%{y:.2f} LPA<extra></extra>")) %>%
      add_trace(y = ~high, name = "Highest package", marker = list(color = "#f59e0b"),
                hovertemplate = paste0("<b>%{x}</b><br>Highest: ", RS, "%{y:.2f} LPA<extra></extra>")) %>%
      layout(barmode = "group", xaxis = list(title = "", categoryorder = "array",
                                             categoryarray = levels(d$short_name)),
             yaxis = list(title = "Package (LPA)")) %>%
      style_plot()
  })

  output$cmp_table <- renderDT({
    sel <- input$cmp_colleges
    req(length(sel) >= 2, length(sel) <= 3)
    d <- filtered()
    metrics <- c("Institution type", "Streams in view", "Students sampled", "Students placed",
                 "Placement rate", "Average package", "Highest package", "Annual fee")
    cols <- lapply(sel, function(cn) {
      x    <- d[d$college == cn, ]
      meta <- COLLEGES[COLLEGES$college == cn, ]
      fee  <- sprintf("%s%.2f lakh", RS, meta$fee)
      if (nrow(x) == 0) {
        return(c(meta$institution_type, rep("No data", 6), fee))
      }
      c(meta$institution_type,
        paste(STREAMS[STREAMS %in% as.character(x$stream)], collapse = ", "),
        format(sum(x$students), big.mark = ","),
        format(sum(x$placed), big.mark = ","),
        sprintf("%.1f%%", sum(x$placed) / sum(x$students) * 100),
        sprintf("%s%.2f LPA", RS, wavg(x$avg_package, x$placed)),
        sprintf("%s%.2f LPA", RS, max(x$highest_package)),
        fee)
    })
    df <- as.data.frame(cols, stringsAsFactors = FALSE)
    names(df) <- COLLEGES$short[match(sel, COLLEGES$college)]
    df <- cbind(Metric = metrics, df, stringsAsFactors = FALSE)
    datatable(df, rownames = FALSE, selection = "none", class = "stripe hover compact",
              options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE)) %>%
      formatStyle("Metric", fontWeight = "600", color = "#334155")
  })

  # ===========================================================================
  # Tab 4: Top Performers
  # ===========================================================================
  top_tbl <- reactive({
    d <- filtered() %>%
      group_by(college, short_name, institution_type, annual_fee_lakh) %>%
      summarise(
        streams  = paste(STREAMS[STREAMS %in% as.character(stream)], collapse = ", "),
        students = sum(students),
        placed   = sum(placed),
        rate     = sum(placed) / sum(students) * 100,
        pkg      = wavg(avg_package, placed),
        high     = max(highest_package),
        .groups  = "drop"
      ) %>%
      filter(rate >= input$min_rate, annual_fee_lakh <= input$max_fee)
    validate(need(nrow(d) > 0,
                  "No colleges meet the current budget and placement-rate filters. Try relaxing them."))
    d %>%
      arrange(desc(.data[[input$rank_by]])) %>%
      head(as.integer(input$top_n)) %>%
      mutate(rank = row_number())
  })

  output$tbl_top <- renderDT({
    d <- top_tbl()
    df <- d %>% transmute(
      Rank = rank,
      College = college,
      Type = institution_type,
      Streams = streams,
      Students = students,
      Placed = placed,
      `Placement Rate (%)` = rate,
      `Avg Package (LPA)` = pkg,
      `Highest Package (LPA)` = high,
      `Annual Fee (INR lakh)` = annual_fee_lakh
    )
    datatable(df, rownames = FALSE, selection = "none", class = "stripe hover compact",
              options = list(dom = "t", paging = FALSE, ordering = TRUE, scrollX = TRUE,
                             columnDefs = list(list(className = "dt-center", targets = c(0, 2))))) %>%
      formatRound(c("Students", "Placed"), 0, mark = ",") %>%
      formatRound("Placement Rate (%)", 1) %>%
      formatRound(c("Avg Package (LPA)", "Highest Package (LPA)", "Annual Fee (INR lakh)"), 2) %>%
      formatStyle("Placement Rate (%)",
                  background = styleColorBar(c(0, 100), "#c7d2fe"),
                  backgroundSize = "100% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Type",
                  backgroundColor = styleEqual(c("Tier-1", "Tier-2"), c("#e0e7ff", "#cffafe")),
                  fontWeight = "600") %>%
      formatStyle("Rank", fontWeight = "700", color = "#4f46e5")
  })

  output$plot_value <- renderPlotly({
    d <- top_tbl()
    plot_ly(d, x = ~annual_fee_lakh, y = ~pkg, size = ~students,
            color = ~institution_type, colors = TIER_COLORS,
            type = "scatter", mode = "markers+text", text = ~short_name,
            textposition = "top center", sizes = c(14, 46),
            marker = list(sizemode = "diameter", opacity = 0.8,
                          line = list(width = 1, color = "#ffffff")),
            hoverinfo = "text",
            hovertext = ~sprintf("<b>%s</b><br>Fee: %s%.2f lakh<br>Avg package: %s%.2f LPA<br>Placement rate: %.1f%%",
                                 college, RS, annual_fee_lakh, RS, pkg, rate)) %>%
      layout(xaxis = list(title = paste0("Annual fee (", RS, " lakh)")),
             yaxis = list(title = "Weighted average package (LPA)")) %>%
      style_plot()
  })
}

shinyApp(ui, server)