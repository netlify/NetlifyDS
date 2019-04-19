The `NetlifyDS` package contains functions to streamline Netlify's data science project.  It espouses the philosophy: Anything that can be automated should be automated. Do as much as possible with functions. 

Functions in this package are for the following purposes:

- Build data pipeline
- Predictive analytics (complementary to caret package)
- Market research analysis (psychometric models and segmentation)
- Unstructured data analysis (text mining, web scraping)

`NetlifyDS` utilizes a number of R packages.  It loads packages as needed and assumes that they are installed. Install the package using:

```r
# if you haven't installed devtools
# install.packages("devtools")
devtools::install_github("netlify/NetlifyDS")
library("NetlifyDS")
```
