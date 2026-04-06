# NBA-Player-Salary-Analysis

## Overview
This project analyzes how player performance impacts NBA salaries using multiple statistical modeling techniques. The goal was to understand which factors truly drive player earnings and how those relationships change across different salary levels.

## Dataset
The dataset includes 7,000+ NBA player-season records (2010–2025) with:
- Player demographics (age, position)
- Performance statistics (points, assists, rebounds, etc.)
- Advanced shooting metrics
- Salary (target variable)

## Methods
- Exploratory Data Analysis (distributions, transformations, correlation analysis)
- Correspondence Analysis to explore relationships between categorical variables
- Factor Analysis to reduce performance metrics into core dimensions
- Regularized Regression (Ridge, LASSO, Elastic Net, All Subsets)
- Quantile Regression to analyze salary impacts across different pay levels

## Key Results
- Salary is not driven by a single stat but a combination of factors
- Consistent “core” predictors across models:
  - Starter status
  - Points
  - Assists
  - Blocks
- Performance metrics group into key dimensions such as scoring volume, rebounding, and efficiency
- Relationships between stats and salary vary significantly across salary tiers (e.g., superstar vs role players)

## Tools
- R (glmnet, psych, corrplot, quantreg, ca)
- Statistical modeling techniques
