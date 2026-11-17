# Read all data tables from M_capensis_hake_extract.xlsx into R data frames
# Requires: install.packages("readxl")

library(readxl)

xlsx <- "M_capensis_hake_extract.xlsx"   # edit path as needed

rd <- function(sheet, range, names) {
  df <- as.data.frame(read_excel(xlsx, sheet = sheet, range = range,
                                 col_names = names, na = c("", "-")))
  df[rowSums(!is.na(df)) > 0, , drop = FALSE]   # drop any fully empty rows
}

# 1. M. capensis catches by fleet (thousand t), 1917-2023
#    1917-1977 from the 2019 RC spec (Table App.A.1a); 1978-2023 from the 2024 RS update (Table 1).
#    Blank in source ("-") -> NA. Total columns are the workbook formulas' values.
catches <- rd("Catches_Mcapensis", "A5:K111",
  c("Year", "Offshore_WC", "Offshore_SC", "Inshore_SC", "Longline_WC",
    "Longline_SC", "Handline_SC", "Offshore_total", "Longline_total", "All_total",
    "Source"))

#    Convert to kg (1 thousand t = 1e6 kg) and record missing catches as zero
catch_cols <- setdiff(names(catches), c("Year", "Source"))
catches[catch_cols] <- lapply(catches[catch_cols], function(x) {
  x[is.na(x)] <- 0
  x * 1e6
})

# 2. M. capensis survey biomass and s.e. (thousand t), Table 3b, 1985-2024
survey <- rd("Survey_Mcapensis", "A5:I44",
  c("Year", "WC_Summer", "WC_Summer_se", "WC_Winter", "WC_Winter_se",
    "SC_Spring", "SC_Spring_se", "SC_Autumn", "SC_Autumn_se"))

# 3. Figure 2a panels 11/14/17 (RS02, Ricker, central year 1958), digitized, 1917-2024
#    Bsp_K is NA for 1917-1925 (line hidden behind the panel label).
fig2a <- rd("Fig2a_RS02_digitized", "A5:D112",
  c("Year", "Bsp", "Bsp_K", "Bsp_BMSY"))

#    Cross-check of digitized values against Table 4b
fig2a_check <- rd("Fig2a_RS02_digitized", "A116:D120",
  c("Quantity", "Table4b", "Digitized", "Difference"))

# 4. Figure B.9 M. capensis female length-at-age (RS03 orange curve), digitized
growth_int  <- rd("FigB9_growth_Mcap_F", "A5:B20",  c("Age", "Length_cm"))   # ages 0-15
growth_grid <- rd("FigB9_growth_Mcap_F", "D5:E155", c("Age", "Length_cm"))   # 0.1-yr grid

# 5. Length-weight parameters (2019 RC spec, Table 7). Source: W(g) = alpha * L(cm)^beta.
#    Converted to W(kg) = alpha * L(cm)^beta: since W(kg) = W(g) / 1000, alpha is divided by 1000.
lw_params <- rd("LengthWeight", "A5:D8", c("Species", "Sex", "alpha", "beta"))
lw_params$alpha <- lw_params$alpha / 1000

#    Weight-at-age for M. capensis females from the Figure B.9 lengths
weight_at_age <- rd("LengthWeight", "A13:D28",
  c("Age", "Length_cm", "Weight_g", "Weight_kg"))

# 6. Natural mortality-at-age, ages 0-15 (15 = plus group), 2019 RC spec section 4.1.1
M_at_age <- rd("M_at_age", "A5:C20", c("Age", "M_capensis", "M_paradoxus"))

# 7. Commercial selectivity-at-age by fleet (digitized, Fig B.7b), ages 0-15 in rows,
#    one column per fleet / sex / period (column names taken from the sheet header)
sel_at_age <- as.data.frame(read_excel(xlsx, sheet = "Selectivity_at_age", range = "A4:U20"))

# 8a. Absolute recruitment (millions of fish) for the Ricker models, 1979-2024,
#     digitized from Figure B.2b panel A(ii)
rec_abs <- rd("Recruitment_Ricker", "A5:D50", c("Year", "RS01", "RS02", "RS03"))

# 8b. Stock-recruitment residuals (log space) for the Ricker models, 1985-2023
#    (2024 RS Table B.1a "Zeta"); not estimated (= 0) before 1985
rec_devs <- rd("RecDevs_Ricker", "A5:D43", c("Year", "RS01", "RS02", "RS03"))

# 9. Female maturity: logistic ogive parameters, f(l) = 1/(1+exp((l50-l)/delta)),
#    and approximate M. capensis maturity-at-age from mean length-at-age (ages 0-15)
mat_params <- rd("Maturity", "A5:C6", c("Species", "l50", "delta"))
mat_at_age <- rd("Maturity", "A11:C26", c("Age", "Length_cm", "Mature"))

#    ogive function for any length (cm), M. capensis females
q <- subset(mat_params, Species == "M. capensis")
len2mat <- function(L) 1 / (1 + exp((q$l50 - L) / q$delta))

# Quick checks
str(catches); str(survey); str(fig2a); str(growth_int); str(lw_params)

# Example: convert any length (cm) to weight (kg) for M. capensis females
p <- subset(lw_params, Species == "M. capensis" & Sex == "Female")
len2wt <- function(L) p$alpha * L^p$beta
len2wt(c(30, 50, 70))   # approx. 0.21, 1.0, 2.8 kg
