# 📊 Tuberculosis Case Projection App

This Shiny app projects tuberculosis (TB) case trends based on historical data. It applies various time series models and statistical tools to produce robust projections and visualize trends. Additionally, it offers the ability to generate detailed PDF reports containing summary tables and plots.

## 🔧 Features

- 📈 **Exponential Smoothing Models**: Simple Exponential Smoothing (SES) and Holt's Linear Trend Model for forecasting.
- 🧪 **Break Point Analysis**: Chow test to detect structural changes in the time series.
- 📉 **Kernel Smoothing**: Gaussian kernel applied to smooth the growth rate of TB cases over time.
- 🎯 **Confidence Intervals**: Forecast intervals based on residual-based estimation.
- 📑 **PDF Report Generation**: Automatic generation of downloadable PDF reports with plots, tables, and forecast summaries.
- 📊 **Interactive Visualizations**: Dynamic graphs using Plotly for improved data exploration.

## 🧠 Methodology

This application implements the following forecasting strategies:

1. **Exponential Smoothing**  
   Both Simple Exponential Smoothing (SES) and Holt’s method are applied to the TB case time series to estimate future trends. Holt’s method accounts for linear trends over time.

2. **Structural Break Detection**  
   The Chow test is used to determine whether a significant change (break point) has occurred in the trend. The date of the most probable break is identified and shown in the forecast plot.

3. **Kernel-Based Smoothing of Growth Rate**  
   A non-parametric approach using a Gaussian kernel smooths the year-over-year growth rate, offering a complementary trend perspective, especially for nonlinear patterns.

4. **Forecast Intervals**  
   Residuals from the model are used to compute 80% and 95% prediction intervals to account for forecast uncertainty.

5. **Report Generation**  
   The app can generate a downloadable PDF that includes:
   - Input parameters
   - Forecast plots
   - Structural break diagnostics
   - Summary statistics

## 🖥️ Live App

The app is already deployed and available online at:

👉 **[https://inerconi.anlis.gob.ar/shiny/appProyeccion/](https://inerconi.anlis.gob.ar/shiny/appProyeccion/)**

> Note: The hosted version is in **Spanish** and freely accessible for public use.

---

## 🚀 Local Installation

This repository is intended for local installations or for users who wish to adapt or extend the app for other contexts.

### Prerequisites

Make sure you have the following installed:

- R (≥ 4.2.0)
- RStudio (recommended)
- Required R packages (listed below)

### Installation Steps

1. Clone this repository:
   ```bash
   git clone https://github.com/sanchisivan/TBprojections.git
   cd TBprojections
   ```

2. Install the required packages in R:
   ```r
   install.packages(c(
     "shiny", "shinydashboard", "plotly", "dplyr", "tidyr", 
     "lubridate", "readxl", "zoo", "broom", "ggplot2", "rmarkdown"
   ))
   ```

3. Run the app:
   ```r
   shiny::runApp()
   ```

### Example Dataset

The repository includes an example Excel file:

📄 `Tendencia_Total_ARG_2009_2024.xlsx`

You can upload this file in the app to test its features.  
Alternatively, you may upload your own dataset (in the same format) to project TB case trends from your own data.

---

## 🏢 About the Project

This application is part of a broader public health initiative developed by the [Instituto Nacional de Enfermedades Respiratorias Dr. Emilio Coni (INER)](https://www.argentina.gob.ar/instituto-nacional-de-enfermedades-respiratorias-dr-emilio-coni), Argentina.

The project was developed in collaboration with:

- **Gustavo Armando**
- **Juan Carlos Bossio**

Further information about the institutional framework and contributors is available in the **Contact** section of the app.

## 📁 Folder Structure

```
TBprojections/
├── www/                            # Custom CSS or image files
├── Tendencia_Total_ARG_2009_2024.xlsx   # Example dataset
├── server.R                        # Shiny server logic
├── ui.R                            # Shiny UI definition
├── global.R                        # Shared global variables
├── report_template.Rmd                      # PDF report template
├── README.md                       # Project description
└── Instrucciones.md                # Usage instructions (in Spanish)
├── Contacto.md                     # Project contact info 
```

## 📄 License

This project is released under the MIT License.

---

For questions or feedback, please open an issue or reach out via the **Contact** tab in the app.