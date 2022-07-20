## edr

A package to analyze Error Distribution Rules

### Error Distribution Rules

Error Distribution Rules (EDRs) are a combination between Distribution Rules and Error Dependence Plots. These are model agnostic and a drill-down technique to evaluate regression models, which consider multivariate interactions between features. EDRs uncover regions of the input space with deviating performance providing an interpretable description of these regions. They can be regarded as a complementary tool to the standard reporting of the expected average predictive performance. Moreover, by providing interpretable descriptions of these specific regions, EDRs allow end users to understand the dangers of using regression tools for some specific cases that fall on these regions, e.g., they improve the accountability of models. This package contains multiple methods to graphically visualize data frames composed of such rules.

## Installing

Install the package using your R console:

`devtools::install_github('citoplasme/edr')`

## Guidelines

The input data frames have to follow the format:

| antecedent_support | p_value | kurtosis | skewness | mean | median | mode | standard_deviation | distribution_values | feature_conditions |
|---|---|---|---|---|---|---|---|---|---|
| Number | Number | Number | Number | Number | Number | Number | Number | List(Number) | List(String) |

## Illustrative examples

```
# Using the illustrative data attached to the package
data("example_rules")

# Calculate the quantiles of every subgroup
calculate_quantiles(example_rules)

# Calculate the counter-factual subgroups of every subgroup
calculate_quantiles(example_rules) %>% counter_factual_subgroups(c(0.00742,0.547, 0.926, 1.33, 2.42))

# Create and plot a graph representing the relationships between subgroups and individual feature conditions
x <- create_graph(example_rules, 0.926)
plot_graph(x$vertices, x$edges)

# List all subgroups' feature conditions as a vector
get_subgroups(example_rules)

# Filter subgroups based on containing a certain pattern ("rm=]")
filter_subgroups(example_rules, "rm=]")

# Plot performance boxplots for a set of subgroups, having the whole error distribution as reference
performance_boxplots(rules = example_rules %>% head(-1), reference_conditions = example_rules %>% tail(1) %>% pull(feature_conditions), reference_distribution = example_rules %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), label = "error")

# Plot a single subgroup, having the whole error distribution as reference
plot_single_subgroup(subgroup_conditions = example_rules[1,]$feature_conditions, subgroup_distribution = example_rules[1,]$distribution_values %>% unlist(use.names = FALSE), reference_conditions = example_rules[nrow(example_rules),]$feature_conditions, reference_distribution = example_rules[nrow(example_rules),]$distribution_values %>% unlist(use.names = FALSE), type = "Boxplot", label = "error")

# Plot multiple subgroups, having the whole error distribution as reference
plot_multiple_subgroups(rules = example_rules %>% head(5), reference_conditions = example_rules %>% tail(1) %>% pull(feature_conditions), reference_distribution = example_rules %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), title = "First 5 Subgroups", label = "error", type = "density")




plot_multiple_models(rules = rbind(rule_1, rule_2), title = "Example of a title", label = "errors", type = "boxPlot")


plot_grid_single_subgroup(example_rules, reference_conditions = example_rules %>% tail(1) %>% pull(feature_conditions), reference_distribution = example_rules %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), type = "boxplot", label = "error", items_per_page = 6)
```

### Contact:

For any bug report or suggestion please contact me via the [Issues](https://github.com/citoplasme/edr/issues) section.
