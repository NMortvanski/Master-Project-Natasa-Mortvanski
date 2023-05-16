source("05_functions/make_reports.R")

report("02_R_scripts/AGP_data_preparation.Rmd", n_file = "1")
report("02_R_scripts/Disease_data_preparation.Rmd", n_file = "2")
report("02_R_scripts/Healthy_data_analysis.Rmd", "3")
report("02_R_scripts/Compare_datasets.Rmd", "4")
report("02_R_scripts/choice_of_alpha.Rmd", "5")
report("02_R_scripts/Hospital_Clinic_data_analysis.Rmd", "6")

