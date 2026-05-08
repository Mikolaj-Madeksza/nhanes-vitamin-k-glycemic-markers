# Dietary Vitamin K Intake and Insulin Resistance Markers in U.S. Adults: NHANES 2001–2018
This repository contains the full R code used to reproduce the analyses presented in:

add citation once published


# Study Overview
Using survey-weighted analyses of 23,247 adults from NHANES 2001–2018, this project evaluated associations between energy-adjusted dietary vitamin K intake and:
- fasting insulin
- HOMA-IR
- fasting glucose
- HbA1c

Analyses included:
- multivariable survey-weighted linear regression
- trend analyses
- continuous exposure models
- sensitivity analyses
- restricted cubic spline analyses
- effect modification analyses


# Repository Contents

```text
.
├── .Rprofile
├── .gitignore
├── LICENSE
├── README.md
├── renv.lock                # Reproducible package versions
├── vitK_nhanes_analysis.R        # Main analysis script
├── vitK_nhanes_analysis.Rproj
│
├── renv/
│   ├── activate.R
│   └── settings.json
│
├── results/
│   └── sessionInfo.txt
│
└── data_raw/                # User-supplied NHANES data files
```

# Data Sources
NHANES datasets are publicly available from the CDC National Center for Health Statistics (NCHS): 
https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx

Healthy Eating Index (HEI-2020) scores were calculated using the dietaryindex R package:
https://jamesjiadazhan.github.io/dietaryindex_manual/articles/dietaryindex.html

The FPED files required for HEI-2020 calculation were obtained using the instructions provided in the dietaryindex documentation.


# Required Data Files
Users must manually download all required NHANES .xpt files and FPED .sas7bdat files into: 
```text
data_raw/
```
The analysis script assumes the exact original NHANES filenames.

Required file groups include:
- NHANES demographic files:
`DEMO_*.xpt`
- Dietary intake files:
`DRXTOT_B.xpt`,
`DR1TOT_*.xpt`
- Diabetes questionnaire files:
`DIQ_*.xpt`
- Glycemic laboratory files:
`L10_B.xpt`,
`L10_C.xpt`,
`GHB_*.xpt`,
`L10AM_B.xpt`,
`L10AM_C.xpt`,
`GLU_*.xpt`,
`INS_*.xpt`,
- Anthropometric files:
`BMX_*.xpt`
- Lipid laboratory files:
`L40_B.xpt`,
`L40_C.xpt`,
`BIOPRO_*.xpt`
- Lifestyle questionnaire files:
`SMQ_*.xpt`,
`ALQ_*.xpt`,
`PAQ_*.xpt`
- FPED files for HEI-2020 calculation:
`fped_dr1tot_0506.sas7bdat`,
`fped_dr1tot_0708.sas7bdat`,
`fped_dr1tot_0910.sas7bdat`,
`fped_dr1tot_1112.sas7bdat`,
`fped_dr1tot_1314.sas7bdat`,
`fped_dr1tot_1516.sas7bdat`,
`fped_dr1tot_1718.sas7bdat`,

# Example Required File Structure
The following image shows the expected contents of the data_raw/ directory.
<img width="2232" height="1588" alt="data_raw_example" src="https://github.com/user-attachments/assets/72b74e76-ed0b-4e6e-ad80-665ac6b93e6f" />

# Reproducibility
This repository uses renv for reproducible package management.

To restore the exact R package environment used in the analyses:
```text
install.packages("renv")
renv::restore()
```

# Running the Analysis
1. Clone the repository
2. Download all required NHANES and FPED files into data_raw/
3. Restore the R environment:
```text
renv::restore()
```
4. Run:
```text
source("nhanes_analysis.R")
```
All output tables will be generated automatically in:
```text
results/
```

# Statistical Notes
Analyses used:
- NHANES fasting subsample weights (`WTSAF2YR`)
- pooled survey-weight adjustment according to NHANES analytic guidelines
- Taylor series linearization variance estimation
- survey-weighted regression using the `survey` R package
To account for singleton strata after subsetting, analyses used:
```text
options(survey.lonely.psu = "adjust")
```

# Software Environment
The analysis was performed in R using a reproducible renv environment.
Detailed package/session information is available in:
```text
results/sessionInfo.txt
```

# License
This project is licensed under the MIT License.




