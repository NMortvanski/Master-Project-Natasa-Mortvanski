# Master-Project-Natasa-Mortvanski
Master project repository - Natasa Mortvanski (MSc Bioinformatics for Health Sciences 2021/2023) 

This is a repository containing the R project with the following directories: 

* 00_raw_data: 

  Contains all raw data files used for the analysis. The data from choosen studies was downloaded 
  from Qiita repository (https://qiita.ucsd.edu/). For each analysed study preparation file was 
  downloaded (metadata table) as well as results of alpha diversity analysis in a form of tsv file.

  Following studies were used as source of samples of different pathological conditions that affect 
  alpha diversity:

    1. Subset of the American Gut Project dataset -> [dataset link](https://qiita.ucsd.edu/study/description/10317)
    2. Multi-omics of the gut microbial ecosystem in inflammatory bowel diseases -> 177 samples, [dataset link](https://qiita.ucsd.edu/study/description/11484)
    3. Metaomics Reveals Microbiome Based Proteolysis as a Driver of Ulcerative Colitis Severity -> 96 samples, [dataset link](https://qiita.ucsd.edu/study/description/11549)
    4. Guiding longitudinal sampling in inflammatory bowel diseases cohorts -> ...samples , [dataset link](https://qiita.ucsd.edu/study/description/2538#)
    4. Gut microbiome predictors of treatment response and recurrence in primary Clostridium difficile infection -> BioProject study accession: PRJNA342347
    5. Dynamic changes in short- and long-term bacterial composition following fecal microbiota transplantation for recurrent Clostridium difficile infection -> 95 samples, [dataset link](https://qiita.ucsd.edu/study/description/1924)
    6. Changes in Microbial Ecology after Fecal Microbiota Transplantation for recurrent C. difficile Infection Affected by Underlying Inflammatory Bowel Disease -> 95 samples, [dataset link](https://qiita.ucsd.edu/study/description/10057)
    7. Hospital Clinic's CDI and donor data (only alpha diversity metrics files and metadata)

  For all of the studies above, following alpha metrics wese computed:
  
  * Shannon's entropy
  * Chao1 index
  * Menhinick's richness index
  * Margalef's richness index
  * Fisher's index
  * Simpson's index
  * Gini index
  * Strong's dominance index
  * Pielou's evenness
  * Faith's Phylogenetic Diversity

* 01_tidy_data: 

  Contains processed tables with choosen metadata variables (columns) and all computed alpha metrics that
  are used in further analysis.

* 02_R_scripts: 

  Contains all Rmd files for the analysis. 

* 03_plots_and_tables: 

  Contains plots and tables generated in the analysis.
  
* 04_reports: 

  Contains HTML reports generated from Rmd files.

* 05_functions: 

  Contains make_reports.R - this script defines function *report* that makes HTML reports out of Rmd scripts 
    and stores them in 04_reports directory.

* 06_shiny_app: 

  Contains R script (app.R) for running Shiny app that compares new sample with healthy donor population from Hospital
  Clínic, stores the results and provides reviews of patients alpha diversity progress trough time.

* run_all.r:

  Script that utilize the make_reports.R function and creates HTML reports for all Rmd files in the project.
  
* Supplementary material
