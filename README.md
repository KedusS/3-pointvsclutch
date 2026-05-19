# WNBA Clutchness Explorer

This Shiny app explores WNBA shot success in late, close-game situations using `wehoop` play-by-play and player box data.

## Story

The project asks whether league-wide clutch performance changes over time, and whether made clutch 3-pointers explain that change.

The app:

1. Defines a Late Tight clutch window.
2. Compares each player's Late Tight shot success to their baseline shot success.
3. Averages player-level clutch deltas by season to estimate league clutchness.
4. Shows Late Tight sample size by season.
5. Compares made clutch 3s with league clutchness using a correlation chart.
6. Shows how clutch shot selection changes over time.

## Data

Data comes from the `wehoop` R package. The default range is 2006-2025 because those seasons have comparable quarter and clock coding for late-game analysis. The app can select 2026 if available, but it may be incomplete.

## How To Run

Open R or RStudio in this folder and install the needed packages:

```r
install.packages(c("shiny", "dplyr", "tidyr", "readr", "DT", "ggplot2", "plotly", "wehoop", "data.table"))
```

Then run:

```r
shiny::runApp()
```

The app will load WNBA data from `wehoop`, so it needs an internet connection the first time it runs.

## Files

- `app.R`: Shiny dashboard and interactive visualizations.
- `02_clutch_analysis.R`: Data loading, cleaning, clutch metric calculation, and summaries.

