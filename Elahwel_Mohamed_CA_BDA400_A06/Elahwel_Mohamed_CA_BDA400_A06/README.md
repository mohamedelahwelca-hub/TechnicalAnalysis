# Mohamed Elahwel — CA_BDA400_A06
## Assignment 6: Technical Analysis using R, Visualization Phase

**Student:** Mohamed Elahwel  
**Course:** BDA400 — Data Science Tools and Techniques  
**Assignment:** 6 — Technical Analysis using R, Visualization Phase (15%)

## Project overview
This project is an interactive portfolio technical-analysis dashboard built with R Shiny. It retrieves historical stock prices from Yahoo Finance and lets the user explore selected stocks over a chosen date range and time frame.

## Features
- Stock selection: AAPL, MSFT, NVDA, AMZN, GOOGL, and TSLA.
- Date-range filtering.
- Daily, weekly, and monthly views.
- Line, area, and candlestick chart types.
- Simple moving averages (SMA 20 and SMA 50) with independent visibility controls.
- RSI (14) and MACD (12, 26, 9) indicator panels with visibility controls.
- Buy/Sell annotations based on SMA 20 and SMA 50 crossovers; Hold otherwise.
- Latest trading-signal summary table.
- Automatic data refresh when the stock, date range, or time frame changes, plus a manual refresh button.

## Requirements
R 4.2 or later and an internet connection for Yahoo Finance data. Install the packages once:

```r
install.packages(c("shiny", "ggplot2", "quantmod", "TTR", "xts", "zoo"))
```

## Run the dashboard
1. Extract the ZIP file.
2. Open the `Elahwel_Mohamed_CA_BDA400_A06` folder in RStudio.
3. Open `app.R` and click **Run App**, or run:

```r
shiny::runApp()
```

## Technical implementation note
Weekly and monthly aggregation use `xts::to.period()` with `period = "weeks"` or `period = "months"`. This avoids calling `quantmod::to.weekly()`, which caused the namespace error observed during testing. The app also refreshes market data when the stock, date range, or time frame changes. Candlestick widths are adjusted for daily, weekly, and monthly views.

## Trading rule
- **Buy:** SMA 20 crosses above SMA 50.
- **Sell:** SMA 20 crosses below SMA 50.
- **Hold:** no new crossover on the current observation.

Signals are educational examples and are not investment advice.

## Project files
- `app.R` — Shiny application entry point.
- `Elahwel_Mohamed_CA_BDA400_A06.R` — named standalone R script for assignment submission (same application code).
- `R/` — supporting indicator/helper scripts retained from the project work.
- `evidence/` — screenshots from the user's app testing.
- `COVER_PAGE.pdf` — assignment cover page.

## GitHub repository
https://github.com/mohamedelahwelca-hub/TechnicalAnalysis

## Data source
Historical stock data is retrieved from Yahoo Finance using `quantmod::getSymbols()`. An internet connection is required when loading or refreshing a symbol.
