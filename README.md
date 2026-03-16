# Pandemic Management: Diagnostic Test Performance Simulator

This R Shiny application allows users to simulate and visualize the performance of diagnostic tests (specifically Positive Predictive Value (PPV) and Negative Predictive Value (NPV)) under different testing strategies and scenarios.

## Features

The application provides three main simulation modules:

1.  **Two Tests (Independent Case)**:
    *   Simulates the combined performance of two independent diagnostic tests.
    *   Visualizes PPV and NPV as a function of prevalence.
    *   Adjustable sensitivity and specificity for each test.

2.  **Two Tests (Dependent Case)**:
    *   Simulates the combined performance of two tests where their outcomes are dependent.
    *   Uses dependency variables `a` and `b` to model the correlation between test results.
    *   Visualizes the impact of dependency on predictive values across various prevalence ranges.

3.  **More Combined Tests (Multi-Week)**:
    *   Extends the dependent model to multiple rounds of testing (weeks).
    *   Allows input for the number of weeks to see how cumulative testing affects diagnostic accuracy.

## Testing Strategies

For each module, users can choose between two common combining strategies:

*   **Believe the Positive (BP)**: A patient is considered positive if *either* test 1 or test 2 returns a positive result.
*   **Believe the Negative (BN)**: A patient is considered positive only if *both* test 1 and test 2 return a positive result.

## How to Run

To run this application, ensure you have R and the `shiny` package installed.

1.  Open the project in RStudio using the `cs350 project.Rproj` file.
2.  Open `PandemicManagement/app.R`.
3.  Click the "Run App" button in RStudio, or run the following in your R console:
    ```r
    shiny::runApp('PandemicManagement')
    ```

## Requirements

*   R
*   `shiny` package
