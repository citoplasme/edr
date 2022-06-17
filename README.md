## edr

A package to analyze Error Distribution Rules

### Error Distribution Rules

Error Distribution Rules (EDRs) are a combination between Distribution Rules and Error Dependence Plots. These are model agnostic and a drill-down technique to evaluate regression models, which consider multivariate interactions between features. EDRs uncover regions of the input space with deviating performance providing an interpretable description of these regions. They can be regarded as a complementary tool to the standard reporting of the expected average predictive performance. Moreover, by providing interpretable descriptions of these specific regions, EDRs allow end users to understand the dangers of using regression tools for some specific cases that fall on these regions, e.g., they improve the accountability of models. This package contains multiple methods to graphically visualize data frames composed of such rules.

## Installing

Install the package using your R console:

`install.packages('edr')` 

or

`devtools::install_github('citoplasme/edr')`

The data frames have to be in the following format:
Columns: antecedent_support, p_value, kurtosis, skewness, mean, median, mode, standard_deviation, distribution_values, feature_conditions
Column Type: Number, Number, Number, Number, Number, Number, Number, Number, List(Number), List(String)

## Illustrative examples

```
# Using the illustrative data attached to the package
data("test_data_v2")



```

### Contact:

For any bug report or suggestion please contact me at jony.pi10@gmail.com
