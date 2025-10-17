# Descriptive Statistics & Data Analysis Tool
An interactive R Shiny application for descriptive statistics and data analysis, providing comprehensive data exploration, visualization, statistical analysis, and missingness analysis capabilities.

## Key Features
1. Data Upload & Variable Selection
* CSV file upload support - Easy import of your data files
* Automatic variable type detection - Intelligent classification into numeric, factor, and string types
* Interactive variable type adjustment - Manual override for custom type assignments
* Data preview functionality - Quick glimpse of your dataset structure

2. Histogram Visualization
* Customized histograms - Create detailed visualizations for numeric variables
* Adjustable parameters - Control bins, colors, and density curves
* Log scale transformation - Handle skewed distributions effectively
* Automatic Shapiro-Wilk testing - Built-in normality assessment with detailed results

3. TableOne Statistical Analysis
* Professional baseline tables - Generate publication-ready characteristic tables
* Grouped analysis support - Compare variables across different groups
* Smart test selection - Automatic choice of appropriate statistical methods:
* Normal variables: t-test/ANOVA
* Skewed variables: Wilcoxon/Kruskal-Wallis tests
* Categorical variables: Chi-square/Fisher's exact tests
* SPSS mode - Optional homogeneity of variance testing for comprehensive analysis

4. Missingness Analysis
* Comprehensive missing data assessment - Count and proportion calculations
* Visual missing patterns - Graphical representation of data completeness
* Exportable reports - Download detailed missingness analysis

## Installation & Requirements
Prerequisites
* R (version 4.0.0 or higher)
* RStudio (recommended)

Required R Packages
The application automatically checks and installs all required dependencies:

```
required_packages <- c(
  "shiny", "writexl", "DT", "dplyr", "table1", 
  "readr", "tableone", "ggplot2", "tidyr", 
  "shinythemes", "shinycssloaders", "colourpicker", 
  "data.table", "rmarkdown", "shinydashboard"
)

```
Installation Steps
* Clone or download this repository
* Open the app.R file in RStudio
* Run the application - all required packages will be automatically installed if missing

## Quick Start Guide
Step 1: Upload Your Data
* Navigate to the "Data Upload & Variable Selection" tab
* Click "Upload CSV File" to select your dataset
* Ensure your CSV file includes headers with unique column names
* View the data preview to verify successful loading

Step 2: Define Variables and Types
* Select variables for analysis using the multi-select dropdown
* Click "Show Variable Types" to display automatic classifications
* Review and adjust variable types using the interactive radio buttons:
* Normal Numeric: For normally distributed continuous variables
* Skewed Numeric: For non-normal continuous variables
* Factor: For categorical variables
* String: For text variables (excluded from statistical analysis)

Step 3: Explore with Visualizations
* Switch to the "Histogram Visualization" tab
* Select numeric variables from available options
* Customize your plots:
* Adjust bin numbers for optimal resolution
* Choose colors using the color picker
* Add density curves for distribution analysis
* Apply log transformations when needed
* Review Shapiro-Wilk normality test results in the accompanying table

Step 4: Perform Statistical Analysis
* Access the "Table One" tab for comprehensive analysis
* Select specific variables for your analysis
* Optionally choose a grouping variable for comparative statistics
* Configure output preferences:
* Set decimal places for statistics and p-values
* Enable SPSS mode for variance homogeneity testing
* Toggle ratio display for categorical variables
* Click "Run Analysis" to generate your TableOne
* Export results to Excel for reporting

Step 5: Assess Data Quality
* Visit the "Missingness" tab for data completeness analysis
* Select variables to examine missing patterns
* Review the missing data summary table
* Analyze the visual representation of missing proportions
* Download comprehensive missingness reports

## Statistical Methodology
Variable Classification & Analysis
|Variable Type	 |  Descriptive Statistics 	|  Statistical Tests         |
| ------------- | -------------------------  | -------------------------- |
|Normal Numeric |	Mean ± Standard Deviation  |	Student's t-test, ANOVA   |
|Skewed Numeric |	Median (IQR)	            | Wilcoxon, Kruskal-Wallis   |
|Categorical	 | Frequency (Percentage)	   | Chi-square, Fisher's exact |

Advanced Features
* SPSS Compatibility Mode: Performs preliminary variance equality testing
* Automatic Test Selection: Intelligently chooses appropriate statistical methods based on data characteristics
* Comprehensive Output: Includes both descriptive statistics and inferential test results

## Data Requirements
File Specifications
* Format: CSV (comma-separated values)
* Encoding: UTF-8 recommended
* Structure: First row should contain unique column headers
* Data Types: Mixed data types supported with automatic detection

Variable Specifications
* Grouping Variables: Should be binary (0/1) for optimal statistical testing
* Missing Data: Handled appropriately in all analyses
* Special Characters: Avoid special characters in column names

## Output and Export
The application supports multiple export formats for enhanced workflow integration:
* Variable Types Table: CSV format for documentation
* Statistical Results: Excel format for TableOne outputs
* Missingness Reports: CSV format for data quality assessment
* Visualizations: High-quality plots for presentations

## Version History
### v3.0 (Current)
Enhanced user interface with improved navigation\
Advanced statistical testing capabilities\
Comprehensive missingness analysis module\
Automated package installation and dependency management\
Export functionality for all analysis components\

### v2.0
Basic statistical analysis implementation\
Initial visualization capabilities\
Foundation for TableOne generation\

### v1.0
Core application framework\
Basic data upload and management\
Fundamental variable type handling\


