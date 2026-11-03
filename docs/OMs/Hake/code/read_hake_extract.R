# Read all data tables from M_capensis_hake_extract.xlsx into R data frames
# Requires: install.packages("readxl")

library(readxl)

xlsx <- "M_capensis_hake_extract.xlsx"   # edit path as needed

rd <- function(sheet, range, names) {
  df <- as.data.frame(read_excel(xlsx, sheet = sheet, range = range,
                                 col_names = names, na = c("", "-")))
  df[rowSums(!is.na(df)) > 0, , drop = FALSE]   # drop any fully empty rows
}

# 1. M. capensis catches by fleet (thousand t), Table 1, 1978-2023
#    Blank in source ("-") -> NA. Total columns are the workbook formulas' values.
catches <- rd("Catches_Mcapensis", "A5:J50",
  c("Year", "Offshore_WC", "Offshore_SC", "Inshore_SC", "Longline_WC",
    "Longline_SC", "Handline_SC", "Offshore_total", "Longline_total", "All_total"))

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

# 5. Length-weight parameters, W(g) = alpha * L(cm)^beta (2019 RC spec, Table 7)
lw_params <- rd("LengthWeight", "A5:D8", c("Species", "Sex", "alpha", "beta"))

#    Weight-at-age for M. capensis females from the Figure B.9 lengths
weight_at_age <- rd("LengthWeight", "A13:D28",
  c("Age", "Length_cm", "Weight_g", "Weight_kg"))

# Quick checks
str(catches); str(survey); str(fig2a); str(growth_int); str(lw_params)

# Example: convert any length (cm) to weight (g) for M. capensis females
p <- subset(lw_params, Species == "M. capensis" & Sex == "Female")
len2wt <- function(L) p$alpha * L^p$beta
len2wt(c(30, 50, 70))
