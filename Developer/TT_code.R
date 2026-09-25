
# ====== Get the tailored teat going ==================================================

library(openMSE)
library(ClimateTest)
#setup()

load(file = "C:/GitHub/ClimateTest/data/SAS.rda") # SAS class om
load(file = "C:/GitHub/ClimateTest/data/TT.rda")  # TT 6D array

OM_list = list(SAS)       # A list of operating models
Hist_list = CT_1_prep(OM_list)     # Same OMs but without climate impacts and inclu

hist = Simulate(SAS)
hist1 = do_all_v2(hist)
proj = Project(hist1,"IndexRate")



Data = hist@Data$`1`$`South African Sardine`
IndexMPs(Data)
