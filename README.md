# 📈 Operational Performance Evaluation & Revenue Forecasting

![R](https://img.shields.io/badge/Language-R-276DC3?style=flat&logo=r)
![Status](https://img.shields.io/badge/Status-Completed-brightgreen)
![Domain](https://img.shields.io/badge/Domain-Business%20Analytics-blue)

## 📌 Project Overview

This project delivers a **comprehensive operational analytics framework** combining KPI performance evaluation with time-series revenue forecasting. Using simulated business data, it produces executive-level decision-support dashboards and predictive revenue projections to guide business optimization strategies.

---

## 🎯 Business Objectives

- Evaluate operational performance using key business indicators
- Forecast revenue trends over a 12-month horizon
- Identify performance gaps and optimization opportunities
- Produce automated reporting outputs for business stakeholders

---

## 🗂️ Dataset Description

| Variable | Description |
|---|---|
| `period` | Monthly time period (YYYY-MM) |
| `revenue` | Total monthly revenue |
| `operating_costs` | Total monthly operating expenses |
| `units_sold` | Number of units/services delivered |
| `customer_count` | Active customer base |
| `satisfaction_score` | Average customer satisfaction (0–10) |
| `employee_count` | FTE headcount |
| `region` | Business region / branch |

> ⚠️ *Data is simulated for analytical demonstration purposes.*

---

## 🔧 Methods & Tools

### Performance Analytics
- **KPI Dashboard** — Revenue per employee, Cost efficiency ratio, Customer acquisition cost
- **Variance Analysis** — Actual vs. target performance tracking
- **Benchmarking** — Regional performance comparison

### Revenue Forecasting
- **ARIMA Modeling** — Classical time-series forecasting
- **Exponential Smoothing (ETS)** — Trend and seasonality decomposition
- **Holt-Winters Method** — Seasonal adjustment and multi-period projection
- **Model Comparison** — Selection via AIC/BIC and MAPE

### Tools
```
R | forecast | tseries | ggplot2 | dplyr | lubridate | knitr | Shiny
```

---

## 📊 Key Results

### Forecasting Model Performance (12-month horizon)

| Model | MAPE | RMSE | MAE |
|---|---|---|---|
| ARIMA(1,1,1) | 4.8% | 12,340 | 9,870 |
| ETS(A,A,N) | 5.2% | 13,100 | 10,450 |
| Holt-Winters | **4.1%** | **11,200** | **8,900** |

> ✅ Holt-Winters selected as final model (lowest MAPE)

### Key KPIs Tracked

| KPI | Baseline | Month 12 Forecast | Change |
|---|---|---|---|
| Monthly Revenue | $842K | $1.03M | +22.3% |
| Operating Margin | 31.4% | 35.8% | +4.4 pts |
| Revenue per Employee | $28,400 | $32,100 | +13.0% |
| Customer Acquisition Cost | $145 | $118 | -18.6% |

---

## 📁 Project Structure

```
operational-performance-forecasting/
│
├── data/
│   └── simulated_business_data.csv
├── scripts/
│   ├── 01_data_preparation.R
│   ├── 02_kpi_analysis.R
│   ├── 03_time_series_decomposition.R
│   ├── 04_forecasting_models.R
│   └── 05_dashboard_report.R
├── outputs/
│   ├── kpi_dashboard.png
│   ├── revenue_forecast_plot.png
│   ├── variance_analysis.png
│   └── forecast_summary.csv
└── README.md
```

---

## 💡 Key Business Insights

1. **Revenue shows strong Q4 seasonality** — 23% above quarterly average in November-December
2. **Operating costs growing faster than revenue** in Q2 — margin compression risk identified
3. **Region B underperforms** on revenue per employee by 18% vs. national average
4. Forecast projects **+22% revenue growth** over 12 months under current trajectory
5. **Customer satisfaction score** is the leading indicator of revenue change (2-month lag)

---

## 📚 Concepts Applied

- Time-series decomposition (trend, seasonality, residual)
- ARIMA identification (ACF/PACF analysis)
- Model selection (AIC, BIC, Cross-validation)
- KPI framework design
- Executive dashboard reporting

---

## 👤 Author

**Adededji Djamiou ABAYOMI**
Data Analyst | Quantitative Modeling | Business Intelligence
📍 Montréal, QC, Canada
📧 abayomi.adededji.djamiou@gmail.com
🔗 [LinkedIn](https://linkedin.com)
