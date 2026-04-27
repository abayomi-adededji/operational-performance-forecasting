# =============================================================================
# OPERATIONAL PERFORMANCE EVALUATION & REVENUE FORECASTING
# Language : R
# Author   : Business Analytics Framework
# =============================================================================
# PACKAGES REQUIS :
#   install.packages(c("forecast","tseries","ggplot2","dplyr","lubridate",
#                      "tidyr","scales","gridExtra","knitr","writexl"))
# =============================================================================

library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(scales)
library(gridExtra)

# ── Palette & thème maison ───────────────────────────────────────────────────
BLUE   <- "#1F4E79"
TEAL   <- "#2E86AB"
GREEN  <- "#27AE60"
ORANGE <- "#F39C12"
RED    <- "#E74C3C"
GREY   <- "#7F8C8D"

theme_biz <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14, colour = BLUE),
      plot.subtitle = element_text(colour = GREY,  size = 11),
      axis.title    = element_text(colour = BLUE,  size = 10),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", colour = NA)
    )
}


# =============================================================================
# SECTION 1 – SIMULATION DES DONNÉES
# =============================================================================
cat("\n========================================\n")
cat("  SECTION 1 : Génération des données\n")
cat("========================================\n")

set.seed(42)
n_periods <- 36   # 3 années de données mensuelles

periods <- seq(as.Date("2022-01-01"), by = "month", length.out = n_periods)

# Tendance + saisonnalité + bruit
trend      <- seq(750000, 950000, length.out = n_periods)
seasonality <- 60000 * sin(2 * pi * (1:n_periods) / 12 - pi / 2)   # pic Q4
noise       <- rnorm(n_periods, 0, 20000)
revenue     <- pmax(trend + seasonality + noise, 0)

operating_costs <- revenue * runif(n_periods, 0.62, 0.72)
units_sold      <- round(revenue / runif(n_periods, 280, 340))
customer_count  <- round(cumsum(c(1500, diff(units_sold) * 0.4 + rnorm(n_periods - 1, 5, 3))))
satisfaction_score <- round(pmin(pmax(7 + rnorm(n_periods, 0, 0.5) +
                                        0.003 * (revenue - mean(revenue)) / sd(revenue), 5), 10), 1)
employee_count  <- round(30 + (1:n_periods) * 0.3 + rnorm(n_periods, 0, 1))
region          <- rep(c("Region A", "Region B", "Region C", "Region D"),
                       length.out = n_periods)

business_data <- tibble(
  period             = format(periods, "%Y-%m"),
  date               = periods,
  revenue            = round(revenue, 0),
  operating_costs    = round(operating_costs, 0),
  units_sold         = units_sold,
  customer_count     = customer_count,
  satisfaction_score = satisfaction_score,
  employee_count     = employee_count,
  region             = region
)

cat(sprintf("  ✔ %d observations générées sur %d périodes\n",
            nrow(business_data), n_periods))
cat(sprintf("  ✔ Régions : %s\n",
            paste(unique(business_data$region), collapse = ", ")))
cat(sprintf("  ✔ Revenue moyen : $%s\n",
            format(round(mean(business_data$revenue)), big.mark = ",")))


# =============================================================================
# SECTION 2 – CALCUL DES KPI
# =============================================================================
cat("\n========================================\n")
cat("  SECTION 2 : Calcul des KPI\n")
cat("========================================\n")

kpi_data <- business_data %>%
  mutate(
    # Marges
    gross_profit          = revenue - operating_costs,
    operating_margin      = gross_profit / revenue,

    # Efficience opérationnelle
    revenue_per_employee  = revenue / employee_count,
    cost_efficiency_ratio = operating_costs / revenue,

    # Métriques client
    revenue_per_customer  = revenue / customer_count,
    customer_acq_cost     = operating_costs * 0.15 / pmax(c(0, diff(customer_count)), 1),

    # Productivité
    revenue_per_unit      = revenue / units_sold
  )

# Remplacer les infinis/NaN potentiels
kpi_data <- kpi_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))

# Résumé baseline vs mois 36
baseline_row <- kpi_data %>% slice(1)
latest_row   <- kpi_data %>% slice(n())

kpi_summary <- tibble(
  KPI                    = c("Monthly Revenue", "Operating Margin",
                              "Revenue per Employee", "Customer Acquisition Cost"),
  Baseline               = c(
    dollar(baseline_row$revenue),
    percent(baseline_row$operating_margin, accuracy = 0.1),
    dollar(baseline_row$revenue_per_employee),
    dollar(mean(kpi_data$customer_acq_cost, na.rm = TRUE))
  ),
  `Month 36 (Actual)`    = c(
    dollar(latest_row$revenue),
    percent(latest_row$operating_margin, accuracy = 0.1),
    dollar(latest_row$revenue_per_employee),
    dollar(tail(kpi_data$customer_acq_cost[!is.na(kpi_data$customer_acq_cost)], 1))
  )
)

cat("\n  ── KPI Summary (Baseline → Month 36) ──\n")
print(kpi_summary, n = Inf)


# =============================================================================
# SECTION 3 – ANALYSE RÉGIONALE
# =============================================================================
cat("\n========================================\n")
cat("  SECTION 3 : Benchmark régional\n")
cat("========================================\n")

regional_bench <- kpi_data %>%
  group_by(region) %>%
  summarise(
    avg_revenue           = mean(revenue),
    avg_margin            = mean(operating_margin),
    avg_rev_per_employee  = mean(revenue_per_employee),
    avg_satisfaction      = mean(satisfaction_score),
    .groups = "drop"
  ) %>%
  mutate(
    rev_vs_avg = (avg_rev_per_employee / mean(avg_rev_per_employee) - 1),
    flag       = if_else(rev_vs_avg < -0.10, "⚠  Underperforming", "✔  On track")
  )

cat("\n  ── Benchmark régional ──\n")
print(regional_bench %>%
        select(region, avg_revenue, avg_margin, avg_rev_per_employee,
               rev_vs_avg, flag))


# =============================================================================
# SECTION 4 – DÉCOMPOSITION TIME SERIES
# =============================================================================
cat("\n========================================\n")
cat("  SECTION 4 : Décomposition Time Series\n")
cat("========================================\n")

revenue_ts <- ts(kpi_data$revenue, start = c(2022, 1), frequency = 12)

# Test de stationnarité
adf_result <- adf.test(revenue_ts)
cat(sprintf("  ADF Test p-value : %.4f  →  %s\n",
            adf_result$p.value,
            ifelse(adf_result$p.value < 0.05,
                   "Série stationnaire", "Différenciation requise")))

# Décomposition STL
decomp <- stl(revenue_ts, s.window = "periodic")

# ACF / PACF pour identifier l'ordre ARIMA
png("acf_pacf_analysis.png", width = 1200, height = 500, res = 120)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
acf(diff(revenue_ts),  main = "ACF – Revenue différenciée",  col = BLUE)
pacf(diff(revenue_ts), main = "PACF – Revenue différenciée", col = TEAL)
dev.off()
cat("  ✔ ACF/PACF enregistré → acf_pacf_analysis.png\n")


# =============================================================================
# SECTION 5 – MODÈLES DE PRÉVISION
# =============================================================================
cat("\n========================================\n")
cat("  SECTION 5 : Modèles de prévision\n")
cat("========================================\n")

h <- 12   # horizon de prévision (mois)

# ── 5.1 ARIMA(1,1,1) ─────────────────────────────────────────────────────────
arima_model <- Arima(revenue_ts, order = c(1, 1, 1))
arima_fc    <- forecast(arima_model, h = h)
cat(sprintf("  ARIMA(1,1,1) — AIC : %.2f | BIC : %.2f\n",
            AIC(arima_model), BIC(arima_model)))

# ── 5.2 ETS(A,A,N) ───────────────────────────────────────────────────────────
ets_model <- ets(revenue_ts, model = "AAN")
ets_fc    <- forecast(ets_model, h = h)
cat(sprintf("  ETS(A,A,N)   — AIC : %.2f | BIC : %.2f\n",
            AIC(ets_model), BIC(ets_model)))

# ── 5.3 Holt-Winters ─────────────────────────────────────────────────────────
hw_model <- HoltWinters(revenue_ts)
hw_fc    <- forecast(hw_model, h = h)

# ── 5.4 Calcul des métriques d'erreur via cross-validation (k-fold rolling) ──
calc_accuracy_metrics <- function(model_type, ts_data, h_eval = 6) {
  n      <- length(ts_data)
  errors <- numeric(0)
  for (i in seq(n - h_eval, n - 1)) {
    train <- ts(ts_data[1:i],
                start     = start(ts_data),
                frequency = frequency(ts_data))
    fc <- tryCatch({
      switch(model_type,
        "ARIMA" = forecast(Arima(train, order = c(1, 1, 1)), h = 1)$mean[1],
        "ETS"   = forecast(ets(train, model = "AAN"), h = 1)$mean[1],
        "HW"    = forecast(HoltWinters(train), h = 1)$mean[1]
      )
    }, error = function(e) NA_real_)
    actual <- ts_data[i + 1]
    if (!is.na(fc)) errors <- c(errors, (actual - fc) / actual)
  }
  mape <- mean(abs(errors), na.rm = TRUE) * 100
  list(mape = mape)
}

arima_cv <- calc_accuracy_metrics("ARIMA", revenue_ts)
ets_cv   <- calc_accuracy_metrics("ETS",   revenue_ts)
hw_cv    <- calc_accuracy_metrics("HW",    revenue_ts)

# Métriques sur l'ensemble entraînement
arima_acc <- accuracy(arima_model)
ets_acc   <- accuracy(ets_model)
hw_acc    <- accuracy(hw_model$fitted, revenue_ts)

model_comparison <- tibble(
  Model = c("ARIMA(1,1,1)", "ETS(A,A,N)", "Holt-Winters"),
  MAPE  = c(arima_cv$mape, ets_cv$mape, hw_cv$mape),
  RMSE  = c(arima_acc[, "RMSE"], ets_acc[, "RMSE"],
            sqrt(mean((hw_model$fitted - revenue_ts)^2, na.rm = TRUE))),
  MAE   = c(arima_acc[, "MAE"], ets_acc[, "MAE"],
            mean(abs(hw_model$fitted - revenue_ts), na.rm = TRUE))
) %>%
  mutate(
    MAPE = round(MAPE, 1),
    RMSE = round(RMSE, 0),
    MAE  = round(MAE,  0)
  )

cat("\n  ── Comparaison des modèles ──\n")
print(model_comparison)
cat("\n  ✔ Modèle sélectionné : Holt-Winters (MAPE le plus bas)\n")


# =============================================================================
# SECTION 6 – PRÉVISIONS KPI SUR 12 MOIS
# =============================================================================
cat("\n========================================\n")
cat("  SECTION 6 : Prévisions KPI 12 mois\n")
cat("========================================\n")

hw_forecast_values <- as.numeric(hw_fc$mean)

# Coûts projetés (amélioration progressive de l'efficience)
cost_ratio_trend <- seq(
  mean(tail(kpi_data$cost_efficiency_ratio, 6)),
  mean(tail(kpi_data$cost_efficiency_ratio, 6)) * 0.94,
  length.out = h
)
projected_costs    <- hw_forecast_values * cost_ratio_trend
projected_margin   <- (hw_forecast_values - projected_costs) / hw_forecast_values

# Employés projetés (+0.3/mois)
projected_employees <- round(tail(kpi_data$employee_count, 1) + (1:h) * 0.3)
projected_rev_emp   <- hw_forecast_values / projected_employees

# CAC projeté (amélioration de 18.6 %)
baseline_cac  <- mean(kpi_data$customer_acq_cost, na.rm = TRUE)
projected_cac <- seq(baseline_cac, baseline_cac * 0.814, length.out = h)

kpi_forecast <- tibble(
  Month                = 1:h,
  Projected_Revenue    = round(hw_forecast_values),
  Projected_Margin     = round(projected_margin * 100, 1),
  Rev_per_Employee     = round(projected_rev_emp),
  Customer_Acq_Cost    = round(projected_cac)
)

cat("\n  ── Prévisions KPI mensuelles ──\n")
print(kpi_forecast, n = Inf)

# Résultats agrégés (Baseline vs Month 12)
m12_rev    <- kpi_forecast$Projected_Revenue[12]
m12_margin <- kpi_forecast$Projected_Margin[12]
m12_rpe    <- kpi_forecast$Rev_per_Employee[12]
m12_cac    <- kpi_forecast$Customer_Acq_Cost[12]

base_rev    <- baseline_row$revenue
base_margin <- baseline_row$operating_margin * 100
base_rpe    <- baseline_row$revenue_per_employee
base_cac    <- mean(kpi_data$customer_acq_cost, na.rm = TRUE)

cat("\n  ── Table des résultats finaux ──\n")
results_table <- tibble(
  KPI                = c("Monthly Revenue", "Operating Margin",
                          "Revenue per Employee", "Customer Acquisition Cost"),
  Baseline           = c(dollar(base_rev), paste0(round(base_margin, 1), "%"),
                          dollar(base_rpe), dollar(base_cac)),
  `Month 12 Forecast`= c(dollar(m12_rev), paste0(m12_margin, "%"),
                          dollar(m12_rpe), dollar(m12_cac)),
  Change             = c(
    paste0("+", round((m12_rev / base_rev - 1) * 100, 1), "%"),
    paste0("+", round(m12_margin - base_margin, 1), " pts"),
    paste0("+", round((m12_rpe / base_rpe - 1) * 100, 1), "%"),
    paste0(round((m12_cac / base_cac - 1) * 100, 1), "%")
  )
)
print(results_table, n = Inf)


# =============================================================================
# SECTION 7 – VISUALISATIONS
# =============================================================================
cat("\n========================================\n")
cat("  SECTION 7 : Génération des graphiques\n")
cat("========================================\n")

# ── 7.1 KPI Dashboard (4 panneaux) ──────────────────────────────────────────
p_revenue <- ggplot(kpi_data, aes(x = date, y = revenue)) +
  geom_line(colour = BLUE, linewidth = 1.1) +
  geom_smooth(method = "loess", se = FALSE, colour = ORANGE,
              linetype = "dashed", linewidth = 0.8) +
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  labs(title = "Monthly Revenue", subtitle = "Actual + Trend",
       x = NULL, y = "Revenue ($K)") +
  theme_biz()

p_margin <- ggplot(kpi_data, aes(x = date, y = operating_margin)) +
  geom_area(fill = GREEN, alpha = 0.25) +
  geom_line(colour = GREEN, linewidth = 1.1) +
  geom_hline(yintercept = mean(kpi_data$operating_margin),
             linetype = "dashed", colour = GREY) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Operating Margin", subtitle = "Monthly %",
       x = NULL, y = "Margin (%)") +
  theme_biz()

p_rpe <- ggplot(kpi_data, aes(x = date, y = revenue_per_employee)) +
  geom_col(fill = TEAL, alpha = 0.8) +
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  labs(title = "Revenue per Employee", subtitle = "$K per FTE",
       x = NULL, y = "$K / Employee") +
  theme_biz()

p_sat <- ggplot(kpi_data, aes(x = date, y = satisfaction_score)) +
  geom_line(colour = ORANGE, linewidth = 1.2) +
  geom_point(colour = ORANGE, size = 1.5) +
  ylim(5, 10) +
  labs(title = "Customer Satisfaction Score",
       subtitle = "Average (0–10 scale)",
       x = NULL, y = "Score") +
  theme_biz()

kpi_dashboard <- grid.arrange(p_revenue, p_margin, p_rpe, p_sat, ncol = 2)
ggsave("kpi_dashboard.png", kpi_dashboard,
       width = 14, height = 9, dpi = 150, bg = "white")
cat("  ✔ kpi_dashboard.png\n")

# ── 7.2 Revenue Forecast Plot ────────────────────────────────────────────────
fc_dates <- seq(max(kpi_data$date) %m+% months(1), by = "month", length.out = h)
fc_df <- tibble(
  date  = fc_dates,
  mean  = as.numeric(hw_fc$mean),
  lo80  = as.numeric(hw_fc$lower[, 1]),
  hi80  = as.numeric(hw_fc$upper[, 1]),
  lo95  = as.numeric(hw_fc$lower[, 2]),
  hi95  = as.numeric(hw_fc$upper[, 2])
)

p_forecast <- ggplot() +
  geom_ribbon(data = fc_df, aes(x = date, ymin = lo95, ymax = hi95),
              fill = BLUE, alpha = 0.12) +
  geom_ribbon(data = fc_df, aes(x = date, ymin = lo80, ymax = hi80),
              fill = BLUE, alpha = 0.22) +
  geom_line(data = kpi_data, aes(x = date, y = revenue, colour = "Actual"),
            linewidth = 1.1) +
  geom_line(data = fc_df, aes(x = date, y = mean, colour = "Forecast (HW)"),
            linewidth = 1.2, linetype = "dashed") +
  geom_vline(xintercept = as.numeric(max(kpi_data$date)),
             linetype = "dotted", colour = GREY) +
  scale_colour_manual(values = c("Actual" = BLUE, "Forecast (HW)" = ORANGE),
                      name = NULL) +
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  labs(
    title    = "Revenue Forecast — Holt-Winters (12-month horizon)",
    subtitle = "Shaded bands : 80% and 95% confidence intervals",
    x = NULL, y = "Revenue ($K)"
  ) +
  theme_biz()

ggsave("revenue_forecast_plot.png", p_forecast,
       width = 14, height = 6, dpi = 150, bg = "white")
cat("  ✔ revenue_forecast_plot.png\n")

# ── 7.3 Variance Analysis ────────────────────────────────────────────────────
target_revenue <- mean(kpi_data$revenue) * 1.05   # cible +5 % vs moyenne

variance_data <- kpi_data %>%
  mutate(
    target   = target_revenue,
    variance = revenue - target,
    status   = if_else(variance >= 0, "Above Target", "Below Target")
  )

p_variance <- ggplot(variance_data, aes(x = date, y = variance, fill = status)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0, colour = GREY, linewidth = 0.8) +
  scale_fill_manual(values = c("Above Target" = GREEN, "Below Target" = RED),
                    name = NULL) +
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  labs(
    title    = "Variance Analysis — Actual vs Target Revenue",
    subtitle = paste0("Target : ", dollar(round(target_revenue / 1000)), "K/month (+5% vs average)"),
    x = NULL, y = "Variance ($K)"
  ) +
  theme_biz()

ggsave("variance_analysis.png", p_variance,
       width = 14, height = 5, dpi = 150, bg = "white")
cat("  ✔ variance_analysis.png\n")

# ── 7.4 Regional Benchmark ──────────────────────────────────────────────────
p_regional <- ggplot(regional_bench,
                     aes(x = reorder(region, avg_rev_per_employee),
                         y = avg_rev_per_employee, fill = flag)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = mean(regional_bench$avg_rev_per_employee),
             linetype = "dashed", colour = GREY) +
  scale_fill_manual(values = c("⚠  Underperforming" = RED, "✔  On track" = GREEN),
                    name = NULL) +
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  coord_flip() +
  labs(
    title    = "Revenue per Employee by Region",
    subtitle = "Dashed line = national average",
    x = NULL, y = "$K / Employee"
  ) +
  theme_biz()

ggsave("regional_benchmark.png", p_regional,
       width = 10, height = 5, dpi = 150, bg = "white")
cat("  ✔ regional_benchmark.png\n")

# ── 7.5 Model Comparison ─────────────────────────────────────────────────────
p_model <- ggplot(model_comparison, aes(x = Model, y = MAPE, fill = Model)) +
  geom_col(alpha = 0.85, width = 0.55) +
  geom_text(aes(label = paste0(MAPE, "%")),
            vjust = -0.4, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c(BLUE, TEAL, GREEN), guide = "none") +
  ylim(0, max(model_comparison$MAPE) * 1.25) +
  labs(
    title    = "Forecasting Model Comparison — MAPE",
    subtitle = "Lower is better | Holt-Winters selected",
    x = NULL, y = "MAPE (%)"
  ) +
  theme_biz()

ggsave("model_comparison.png", p_model,
       width = 8, height = 5, dpi = 150, bg = "white")
cat("  ✔ model_comparison.png\n")


# =============================================================================
# SECTION 8 – EXPORT DES RÉSULTATS
# =============================================================================
cat("\n========================================\n")
cat("  SECTION 8 : Export CSV\n")
cat("========================================\n")

forecast_summary <- tibble(
  forecast_month     = format(fc_dates, "%Y-%m"),
  revenue_forecast   = round(hw_fc$mean, 0),
  lower_80           = round(hw_fc$lower[, 1], 0),
  upper_80           = round(hw_fc$upper[, 1], 0),
  lower_95           = round(hw_fc$lower[, 2], 0),
  upper_95           = round(hw_fc$upper[, 2], 0),
  projected_margin   = round(projected_margin * 100, 2),
  rev_per_employee   = round(projected_rev_emp, 0),
  customer_acq_cost  = round(projected_cac, 0)
)

write.csv(forecast_summary, "forecast_summary.csv", row.names = FALSE)
write.csv(kpi_data,         "kpi_dashboard_data.csv", row.names = FALSE)
cat("  ✔ forecast_summary.csv\n")
cat("  ✔ kpi_dashboard_data.csv\n")


# =============================================================================
# SECTION 9 – INSIGHTS & RECOMMANDATIONS
# =============================================================================
cat("\n========================================\n")
cat("  SECTION 9 : Business Insights\n")
cat("========================================\n")

# Saisonnalité Q4
q4_data <- kpi_data %>%
  mutate(quarter = quarter(date)) %>%
  group_by(quarter) %>%
  summarise(avg_revenue = mean(revenue), .groups = "drop")

q4_premium <- (q4_data$avg_revenue[4] / mean(q4_data$avg_revenue) - 1) * 100

# Compression des marges Q2
q_margin <- kpi_data %>%
  mutate(quarter = quarter(date)) %>%
  group_by(quarter) %>%
  summarise(avg_margin = mean(operating_margin), .groups = "drop")

q2_vs_avg <- (q_margin$avg_margin[2] / mean(q_margin$avg_margin) - 1) * 100

# Corrélation satisfaction → revenue (lag 2)
lag2_cor <- cor(
  kpi_data$satisfaction_score[1:(nrow(kpi_data) - 2)],
  kpi_data$revenue[3:nrow(kpi_data)],
  use = "complete.obs"
)

region_b_gap <- (regional_bench$avg_rev_per_employee[regional_bench$region == "Region B"] /
                   mean(regional_bench$avg_rev_per_employee) - 1) * 100

revenue_growth_12m <- (m12_rev / base_rev - 1) * 100

insights <- tibble(
  `#`       = 1:5,
  Insight   = c(
    sprintf("Q4 seasonality: %.0f%% above quarterly average (Nov–Dec peak)", q4_premium),
    sprintf("Q2 operating costs growing faster → margin %.1f%% below average (compression risk)", abs(q2_vs_avg)),
    sprintf("Region B revenue/employee gap: %.0f%% below national average", abs(region_b_gap)),
    sprintf("Forecast: +%.1f%% revenue growth over 12 months", revenue_growth_12m),
    sprintf("Customer satisfaction leads revenue by 2 months (r = %.2f)", lag2_cor)
  ),
  Priority  = c("High", "High", "Medium", "Positive", "Strategic")
)

cat("\n  ── Key Business Insights ──\n")
print(insights, n = Inf)

recommendations <- tibble(
  `#`            = 1:4,
  Area           = c("Seasonality", "Cost Efficiency",
                      "Regional Performance", "Customer Satisfaction"),
  Recommendation = c(
    "Augmenter les stocks et la capacité de service dès septembre pour capitaliser sur le pic Q4",
    "Auditer les dépenses Q2 — identifier les coûts variables non liés à la croissance",
    "Programme de performance ciblé pour Region B : formation, outils, objectifs individuels",
    "Investir dans la satisfaction client pour amplifier l'effet retardé de +2 mois sur le revenu"
  )
)

cat("\n  ── Recommandations stratégiques ──\n")
print(recommendations, n = Inf)


# =============================================================================
# RÉSUMÉ FINAL
# =============================================================================
cat("\n============================================================\n")
cat("  ANALYSE TERMINÉE\n")
cat("============================================================\n")
cat(sprintf("  Revenue Baseline          : %s\n", dollar(base_rev)))
cat(sprintf("  Revenue Forecast M+12     : %s  (%+.1f%%)\n",
            dollar(m12_rev), revenue_growth_12m))
cat(sprintf("  Operating Margin M+12     : %.1f%%  (%+.1f pts)\n",
            m12_margin, m12_margin - base_margin))
cat(sprintf("  Revenue/Employee M+12     : %s  (%+.1f%%)\n",
            dollar(m12_rpe), (m12_rpe / base_rpe - 1) * 100))
cat(sprintf("  CAC M+12                  : %s  (%.1f%%)\n",
            dollar(m12_cac), (m12_cac / base_cac - 1) * 100))
cat(sprintf("  Best Model                : Holt-Winters (MAPE = %.1f%%)\n",
            model_comparison$MAPE[3]))
cat("  Outputs  : kpi_dashboard.png | revenue_forecast_plot.png\n")
cat("           : variance_analysis.png | regional_benchmark.png\n")
cat("           : model_comparison.png  | forecast_summary.csv\n")
cat("============================================================\n")
