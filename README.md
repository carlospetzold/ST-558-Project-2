# ST 558 Project 2: UEFA Champions League Explorer

## Project Overview

This repository contains my second project for ST 558 at North Carolina State University.

The goal of this project is to develop an interactive R Shiny application that allows users to explore player and team performance during the 2025–26 UEFA Champions League season.

The application provides interactive data filtering, statistical summaries, data visualizations, and downloadable datasets.

## Data

The project uses player-level match statistics from the 2025–26 UEFA Champions League season, collected from FotMob.

Variables include:

- Player and team names
- Starter or substitute status
- Match ratings
- Goals and assists
- Distance covered
- Chances created

**Data Source:** [FotMob](https://www.fotmob.com/)

## Shiny Application Features

### 1. Interactive Data Filtering

Users can filter the dataset by team and appearance type (starter or substitute). They can also select two numerical variables and optionally restrict observations to specified ranges.

Filters are applied using an **Apply Filters** button.

### 2. About Tab

Provides an overview of the project, information about the dataset, and instructions for navigating the application.

### 3. Data Download Tab

Allows users to:

- View the filtered dataset in an interactive table.
- Search and navigate through observations.
- Download the filtered data as a CSV file.

### 4. Data Exploration Tab

Allows users to produce statistical summaries and visualizations using their selected data.

**Categorical Summaries**
- One-way contingency tables
- Two-way contingency tables

**Numerical Summaries**
- Sample sizes
- Means and medians
- Standard deviations
- Minimum and maximum values

**Graphical Summaries**
- Bar charts
- Faceted bar charts
- Boxplots
- Dumbbell plots
- Scatterplots
- Heatmaps

Users can customize the variables used in these analyses.

## Project Files

- `app.R` — R Shiny application containing the user interface and server logic.
- `ST_558_Project2_Petzold.qmd` — Static exploratory data analysis using Quarto.
- `data/` — Datasets used by the application.
- `www/` — Images used by the application.

## Running the Application

The application was developed using R and the following packages:

```r
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
```

To run the application locally, open the project in RStudio and execute:

```r
shiny::runApp()
```

## Author

Carlos Petzold

ST 558 — North Carolina State University
