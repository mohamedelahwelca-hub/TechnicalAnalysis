# Mohamed Elahwel | BDA400 Assignment 6
# Portfolio Technical Analysis Dashboard
# Run from this folder with: shiny::runApp()

required_packages <- c("shiny", "ggplot2", "quantmod", "TTR", "xts", "zoo")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    paste(
      "Please install the following packages first:",
      paste(missing_packages, collapse = ", "),
      "\nRun install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    )
  )
}

library(shiny)
library(ggplot2)

available_symbols <- c("AAPL", "MSFT", "NVDA", "AMZN", "GOOGL", "TSLA")

fetch_stock_data <- function(symbol, from, to, time_frame = "Daily") {
  raw <- tryCatch(
    suppressWarnings(
      quantmod::getSymbols(
        symbol, src = "yahoo", from = as.Date(from),
        to = as.Date(to) + 1, auto.assign = FALSE
      )
    ),
    error = function(e) {
      stop(paste("Yahoo Finance data could not be loaded for", symbol, ":", e$message))
    }
  )

  if (NROW(raw) < 2) stop("Not enough market observations were returned for this date range.")

  # Aggregate daily Yahoo Finance observations using xts::to.period.
  # quantmod does not export to.weekly() in all package versions.
  if (time_frame == "Weekly") {
    raw <- xts::to.period(raw, period = "weeks", indexAt = "lastof", OHLC = TRUE)
  } else if (time_frame == "Monthly") {
    raw <- xts::to.period(raw, period = "months", indexAt = "lastof", OHLC = TRUE)
  }

  out <- data.frame(
    Date = as.Date(zoo::index(raw)),
    Open = as.numeric(quantmod::Op(raw)),
    High = as.numeric(quantmod::Hi(raw)),
    Low = as.numeric(quantmod::Lo(raw)),
    Close = as.numeric(quantmod::Cl(raw)),
    Volume = as.numeric(quantmod::Vo(raw))
  )
  out <- out[complete.cases(out[, c("Date", "Open", "High", "Low", "Close")]), ]
  out <- out[order(out$Date), ]
  if (nrow(out) < 2) stop("Not enough valid observations after cleaning the data.")
  out
}

add_indicators <- function(df) {
  close <- df$Close
  df$MA20 <- as.numeric(TTR::SMA(close, n = 20))
  df$MA50 <- as.numeric(TTR::SMA(close, n = 50))
  df$RSI <- as.numeric(TTR::RSI(close, n = 14))
  macd_values <- TTR::MACD(close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
  df$MACD <- as.numeric(macd_values[, 1])
  df$MACD_Signal <- as.numeric(macd_values[, 2])

  df$Signal <- "Hold"
  valid <- which(!is.na(df$MA20) & !is.na(df$MA50))
  if (length(valid) >= 2) {
    for (j in 2:length(valid)) {
      i <- valid[j]
      previous <- valid[j - 1]
      if (df$MA20[i] > df$MA50[i] && df$MA20[previous] <= df$MA50[previous]) {
        df$Signal[i] <- "Buy"
      } else if (df$MA20[i] < df$MA50[i] && df$MA20[previous] >= df$MA50[previous]) {
        df$Signal[i] <- "Sell"
      }
    }
  }
  df
}

ui <- fluidPage(
  titlePanel("Portfolio Technical Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("symbol", "Select Stock:", choices = available_symbols, selected = "AAPL"),
      dateRangeInput(
        "date_range", "Select Date Range:",
        start = Sys.Date() - 365 * 3, end = Sys.Date(),
        min = "2000-01-01", max = Sys.Date()
      ),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly"), selected = "Daily"),
      radioButtons("chart_type", "Stock Chart Type:",
                   choices = c("Line", "Area", "Candlestick"), selected = "Line"),
      checkboxGroupInput(
        "indicators", "Technical Indicators:",
        choices = c("Moving Average 20" = "MA20",
                    "Moving Average 50" = "MA50",
                    "RSI" = "RSI", "MACD" = "MACD"),
        selected = c("MA20", "MA50", "RSI", "MACD")
      ),
      actionButton("refresh", "Refresh Market Data"),
      helpText("Market data is retrieved from Yahoo Finance when the selected stock is loaded.")
    ),
    mainPanel(
      h3(textOutput("chart_heading")),
      plotOutput("stock_chart", height = "480px"),
      h3("Technical Indicator Panel"),
      plotOutput("rsi_chart", height = "190px"),
      plotOutput("macd_chart", height = "190px"),
      h3("Latest Trading Signal"),
      tableOutput("latest_signal"),
      p("Trading signals are educational examples based on 20-day and 50-day simple moving averages. They are not investment advice.")
    )
  )
)

server <- function(input, output, session) {
  stock_data <- reactive({
    # Depend on the refresh button as well as the controls. The dashboard
    # reloads when a stock, date range, or time frame changes, and also
    # when the user presses Refresh Market Data.
    input$refresh
    req(input$symbol, input$date_range, input$time_frame)
    validate(
      need(length(input$date_range) == 2, "Please select both start and end dates."),
      need(input$date_range[1] < input$date_range[2],
           "Please select a valid date range.")
    )
    withProgress(message = "Loading market data...", value = 0.2, {
      data <- fetch_stock_data(
        input$symbol, input$date_range[1], input$date_range[2], input$time_frame
      )
      incProgress(0.5, detail = "Calculating technical indicators")
      add_indicators(data)
    })
  })

  output$chart_heading <- renderText({
    paste(input$symbol, "Stock Price -", input$time_frame)
  })

  output$stock_chart <- renderPlot({
    df <- stock_data()
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = Date))

    if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Close), fill = "grey75", alpha = 0.8) +
        geom_line(aes(y = Close), linewidth = 0.35)
    } else if (input$chart_type == "Candlestick") {
      candle_width <- switch(
        input$time_frame,
        "Daily" = 0.7,
        "Weekly" = 4,
        "Monthly" = 18
      )
      p <- p +
        geom_segment(aes(y = Low, yend = High), linewidth = 0.35) +
        geom_rect(
          aes(
            xmin = Date - candle_width / 2,
            xmax = Date + candle_width / 2,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            fill = Close >= Open
          ),
          color = "black", linewidth = 0.2
        ) +
        scale_x_date()
    } else {
      p <- p + geom_line(aes(y = Close), linewidth = 0.45)
    }

    if ("MA20" %in% input$indicators) {
      p <- p + geom_line(aes(y = MA20), linetype = "dashed",
                         linewidth = 0.65, na.rm = TRUE)
    }
    if ("MA50" %in% input$indicators) {
      p <- p + geom_line(aes(y = MA50), linetype = "dotted",
                         linewidth = 0.75, na.rm = TRUE)
    }

    signals <- df[df$Signal %in% c("Buy", "Sell"), ]
    if (nrow(signals) > 0) {
      p <- p +
        geom_point(data = signals, aes(y = Close), size = 2) +
        geom_text(data = signals, aes(y = Close, label = Signal),
                  vjust = -0.8, size = 3)
    }

    p + labs(
      x = "Date", y = "Price (USD)",
      title = paste(input$symbol, "with Moving Averages and Trading Signals")
    ) + theme_minimal() + theme(legend.position = "none")
  })

  output$rsi_chart <- renderPlot({
    df <- stock_data()
    if (!("RSI" %in% input$indicators)) {
      plot.new(); text(0.5, 0.5, "RSI is turned off"); return()
    }
    ggplot(df, aes(Date, RSI)) +
      geom_line(linewidth = 0.4) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed") +
      coord_cartesian(ylim = c(0, 100)) +
      labs(x = "Date", y = "RSI (0-100)") + theme_minimal()
  })

  output$macd_chart <- renderPlot({
    df <- stock_data()
    if (!("MACD" %in% input$indicators)) {
      plot.new(); text(0.5, 0.5, "MACD is turned off"); return()
    }
    ggplot(df, aes(Date)) +
      geom_col(aes(y = MACD - MACD_Signal), alpha = 0.35) +
      geom_line(aes(y = MACD), linewidth = 0.4) +
      geom_line(aes(y = MACD_Signal), linetype = "dashed", linewidth = 0.4) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      labs(x = "Date", y = "MACD") + theme_minimal()
  })

  output$latest_signal <- renderTable({
    df <- stock_data()
    latest <- tail(df, 1)
    data.frame(
      Symbol = input$symbol,
      Date = as.character(latest$Date),
      Closing_Price_USD = round(latest$Close, 2),
      MA20 = round(latest$MA20, 2),
      MA50 = round(latest$MA50, 2),
      Signal = latest$Signal
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

shinyApp(ui = ui, server = server)
