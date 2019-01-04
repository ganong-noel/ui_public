# Warning --
# We use "checkpoint" package to ensure the results are reproducible
# even when new versions of packages are released. You may see packages 
# installed when running the code. They are stored in a temp directory 
# and do not affect the packages installed in the normal path.

data_path <- paste0(getwd(), "/data/")
out_path <- paste0(getwd(), "/out/")

source("prelim.R")
source("1_chase_state_map.R")
source("2_scf.R") 
source("3_sipp_extract_cols.R")
source("4_sipp_build_ui_event_study.R")
source("5_sipp_who_are_ui_recipients.R")
source("6_spending_crosswalk.R")
source("7_dcpc_benchmark.R")
source("8_ui_benefits.R")