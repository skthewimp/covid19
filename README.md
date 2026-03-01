# COVID-19 India Analysis

A collection of analyses I did during the first wave of COVID-19 in India (March-July 2020). Some of this was used for newspaper articles, some was shared with government contacts, some was just me trying to make sense of the data in real time.

## The analyses

### Epidemic simulator (`covid transmission simulator.Rmd`)

How dangerous is a shopping trip versus an office meeting versus a conference? This models COVID transmission as a Poisson process - given an assumed transmission rate (parameterised so that 24 hours of continuous contact gives a 50% infection probability), it simulates various real-world scenarios: grocery trips, office days, school, conferences, dinner parties. The function-based version (`covid transmission simulator function.R`) wraps this up for reuse.

The accompanying Shiny app (`app.R`) lets you play with the parameters interactively.

### Rt estimation (`rt_estimation.Rmd`)

An R implementation of Kevin Systrom's Bayesian Rt estimation method (from [rt.live](https://rt.live)). Rt is the effective reproduction number - above 1 means the epidemic is growing, below 1 means it's shrinking. The implementation uses a Poisson likelihood model with a 7-day rolling window, producing posterior mean and 95% credible intervals.

Computed for all-India, state-wise (for states with 500+ cases), and for Karnataka specifically (filtering out travel-related cases using keyword detection in the notes field - "travel", "return", etc.).

### COVID tracker series (`covidTracker.Rmd`, `covidTracker2.Rmd`, `covidTracker3.Rmd`)

Three iterations of a state-wise tracking dashboard. The core metric is doubling time, computed from 3-day compounded daily growth rates. Log-scale cumulative plots, daily growth rates, and state-wise doubling time comparisons. Each iteration added more states and refined the visualisations as the pandemic evolved.

### Geographic segmentation (`covid_segmentation.Rmd`)

An interesting algorithmic problem - how do you automatically pick the most meaningful geographic units to report COVID cases by? India has states, districts, and cities, and some cities (Mumbai, Delhi) need their own segment while most districts can be rolled up into their state.

The algorithm takes the top N geographic units by case count at all levels, then resolves parent-child conflicts - if both "Maharashtra" and "Mumbai" appear in the top N, it creates "Maharashtra - {Mumbai}" (i.e. Maharashtra minus Mumbai) and reports Mumbai separately. Always includes a "Rest of India" residual.

### Government briefing graphs (`covidGraphsForGovt.R`)

The most production-grade script in the set. Generates multi-page PDFs with national doubling times, state-level trajectories colour-coded by growth rate, district-level analysis, testing trends, 14-day growth projections with traffic-light colouring (red if doubling in under a week, green if over a month), and a Karnataka choropleth map.

### Other pieces

- `quarantinedInKarnataka.Rmd` - Scrapes Karnataka government data on quarantined travellers: arrival dates, districts, origin ports
- `overtonwindows.Rmd` - A simulated illustration of diverging Overton windows (political polarisation concept), used for a blog post
- `covidGraphsForTwitter.R` - Quick charts for sharing on Twitter
- `covidtestingWorldwide.R` - Cross-country testing comparison
- `covid graphs for the paper.Rmd` - Publication-quality charts for an article co-authored with Suprio Guha Thakurta

## Data

Data files aren't included. All the analysis used the [covid19india.org](https://www.covid19india.org/) API (JSON and CSV endpoints), which was the canonical community-maintained dataset during the pandemic. The API is no longer active, but archived data may be available on their GitHub.

District shapefiles for India (Census 2011) are included in the repo.

## Dependencies

R, with `tidyverse`, `sf`, `jsonlite`, `scales`, `patchwork`, `rvest`, `readxl`.
