library("this.path")
 
#Set working directory to script path
setwd(here())

#Run these scripts!!

USE_MEASURED_UPTAKES <- TRUE
source("3. RC_KP_Hanninghof_run.r")
source("9. RC_PK_Njoroge dataset_run.r")
source("14. RC_KP_Broadbalk_run.r")
source("20. NPK ratios.r")

USE_MEASURED_UPTAKES <- FALSE
source("3. RC_KP_Hanninghof_run.r")
source("9. RC_PK_Njoroge dataset_run.r")
source("14. RC_KP_Broadbalk_run.r")
source("16. Broadbalk Olsen P vs pools.r")
source("16a. Broadbalk P total vs pools.r")
source("20. NPK ratios.r")
stop("Runs completed")



##To run all from scratch

#Prepare input data
source("1. HahnHof experimental data processing.r")
source("2. Hanninghof data analysis.r")
source("3. RC_KP_Hanninghof_run.r")

USE_MEASURED_UPTAKES <- TRUE
source("4. RC_PK_Hanninghof_calibration.r")
USE_MEASURED_UPTAKES <- FALSE
source("3. RC_KP_Hanninghof_run.r")
source("5. Hanninghof soil analysis vs pools.r")

source("6. Njoroge experimental data processing.r")
source("6a. Njoroge Estimate soil N supply.r")
source("9. RC_PK_Njoroge dataset_run.r")

USE_MEASURED_UPTAKES <- TRUE
source("10. RC_PK_Njoroge_calibration.r")
USE_MEASURED_UPTAKES <- FALSE

source("11. Broadbalk experimental data processing.r")
source("13. Broadbalk data analysis.r")
source("14. RC_KP_Broadbalk_run.r")

USE_MEASURED_UPTAKES <- TRUE
source("15. RC_PK_Broadbalk_calibration.r")
USE_MEASURED_UPTAKES <- FALSE
source("14. RC_KP_Broadbalk_run.r")

source("17. Combined accumulation dilution curves.r")
source("18. Plot_measured_vs_predicted_combined_Siaya_Hanninghof_Broadbalk.r")

source("20. NPK ratios.r")

