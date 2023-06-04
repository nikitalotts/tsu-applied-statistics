### Task 1: Simple Linear Regression Model
Let the regression model be described by the equation: $y_i = a + bx_i + ε_i,$ $ε_i \sim N(0, σ^2), i = \overline{1, n}.$

A) Generate datasets of $n$ observations according to the proposed model. Set all necessary parameters independently.

B) Plot a scatter plot for the original model.

C) Implementing all necessary formulas independently, without using the lm() function:
1. Find the least squares estimates of the model parameters.
2. Find the variances of the observations and parameter estimates.
3. Construct confidence intervals for the unknown parameters.
4. Test hypotheses regarding the significance of regression coefficients.
5. Find the coefficient of determination for the model.
6. Test the hypothesis of model adequacy.
7. Repeat the calculations using the `lm()` function.
8. Compare the obtained results.
9. Plot the regression function on the graph, confidence intervals for direct regression and for the forecast.


### Task 2: Nonlinear Models of Simple Regression

Let the regression model be described by one of the following equations:

1. $y_i = ax_i^bε_i,$ where $ln(ε_i) \sim N(0, σ^2), i = \overline{1, n}$.

2. $y_i = ae^{bx_i}ε_i,$ where $ln(ε_i) \sim N(0, σ^2), i = \overline{1, n}$.

3. $y_i = a + bln(x_i) + ε_i,$ where $ε_i \sim N(0, σ^2), i = \overline{1, n}.$

4. $y_i = a + bx_i + ε_i,$ where $ε_i \sim N(0, σ^2), i = \overline{1, n}.$

For each equation, do the following steps:

1. Generate datasets of $n$ observations for each of the proposed models, following the example of the linear model. Set all necessary parameters independently.
2. Plot scatter plots for the original models.
3. Perform linearization and plot scatter plots for the linearized models.
4. Find the least squares estimates of the model parameters.
5. Find the variances of the observations and parameter estimates.
6. Construct confidence intervals for the unknown parameters.
7. Test hypotheses regarding the significance of regression coefficients.
8. Find the coefficient of determination for the model.
9. Test the hypothesis of model adequacy.
10. Plot confidence intervals for the regression line and for predictions on the graph.