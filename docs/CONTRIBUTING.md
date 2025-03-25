# Contributing Guidelines

Thank you for your interest in contributing to the Liver Transplant Waitlist Survival Analysis project! This document provides guidelines and instructions for contributing to the project.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [How to Contribute](#how-to-contribute)
- [Development Workflow](#development-workflow)
- [Coding Standards](#coding-standards)
- [Testing Guidelines](#testing-guidelines)
- [Documentation Guidelines](#documentation-guidelines)
- [Submission Guidelines](#submission-guidelines)
- [Review Process](#review-process)
- [Community](#community)

## Code of Conduct

This project adheres to a Code of Conduct that all contributors are expected to follow. By participating, you are expected to uphold this code. Please report unacceptable behavior to the project maintainers.

Key principles:
- Be respectful and inclusive
- Be collaborative
- Focus on the best possible outcomes for patients
- Maintain scientific rigor and integrity

## Getting Started

### Prerequisites

- R (version 4.2.0 or higher)
- RStudio (recommended)
- Git
- Knowledge of survival analysis and/or machine learning

### Setup

1. Fork the repository
2. Clone your fork:
   ```
   git clone https://github.com/your-username/liver-transplant-analysis.git
   ```
3. Set up the development environment:
   ```
   cd liver-transplant-analysis
   source("src/setup.R")
   ```
4. Create a new branch for your contribution:
   ```
   git checkout -b feature/your-feature-name
   ```

## How to Contribute

There are many ways to contribute to this project:

### Code Contributions

- Implement new features or models
- Fix bugs
- Optimize existing code
- Add tests

### Non-Code Contributions

- Improve documentation
- Report bugs
- Suggest enhancements
- Review pull requests
- Share the project with others

### Reporting Bugs

When reporting bugs, please include:

1. A clear, descriptive title
2. Steps to reproduce the issue
3. Expected behavior
4. Actual behavior
5. R version and package versions
6. Any error messages or logs

### Suggesting Enhancements

Enhancement suggestions are welcome! Please provide:

1. A clear description of the enhancement
2. The motivation for the enhancement
3. Possible implementation approaches
4. Any relevant research papers or references

## Development Workflow

1. **Update your fork**: Before starting work, make sure your fork is up to date with the main repository.
   ```
   git remote add upstream https://github.com/original-owner/liver-transplant-analysis.git
   git fetch upstream
   git checkout main
   git merge upstream/main
   git push origin main
   ```

2. **Create a branch**: Create a new branch for your work.
   ```
   git checkout -b feature/your-feature-name
   ```

3. **Develop**: Make your changes, following the coding standards.

4. **Test**: Run tests to ensure your changes don't break existing functionality.
   ```
   source("tests/run_tests.R")
   ```

5. **Document**: Update documentation to reflect your changes.

6. **Commit**: Commit your changes with a clear message.
   ```
   git add .
   git commit -m "Add feature: your feature description"
   ```

7. **Push**: Push your changes to your fork.
   ```
   git push origin feature/your-feature-name
   ```

8. **Pull Request**: Create a pull request from your branch to the main repository.

## Coding Standards

### R Style Guide

This project follows the [tidyverse style guide](https://style.tidyverse.org/) for R code. Key points:

- Use snake_case for variable and function names
- Use spaces around operators
- Limit line length to 80 characters
- Use meaningful variable names
- Comment your code

### Function Documentation

All functions should be documented using roxygen2 style:

```r
#' Function Title
#'
#' Detailed description of what the function does.
#'
#' @param param1 Description of param1
#' @param param2 Description of param2
#'
#' @return Description of return value
#'
#' @examples
#' example_function(param1 = "value", param2 = 123)
#'
#' @export
example_function <- function(param1, param2) {
  # Function implementation
}
```

### Code Organization

- Place utility functions in `src/utils.R`
- Place data processing functions in `src/data_preprocessing.R`
- Place analysis functions in appropriate files based on functionality
- Keep functions focused on a single task
- Use consistent error handling

## Testing Guidelines

### Test Coverage

All new code should include tests. We aim for at least 80% test coverage.

### Test Organization

- Place tests in the `tests` directory
- Name test files with the pattern `test_*.R`
- Group related tests together

### Running Tests

Run all tests with:
```r
source("tests/run_tests.R")
```

## Documentation Guidelines

### Code Documentation

- Document all functions with roxygen2 comments
- Include examples in function documentation
- Explain complex algorithms or statistical methods
- Reference relevant papers or sources

### Project Documentation

- Update README.md with new features
- Update CHANGELOG.md for each release
- Keep API.md up to date with function changes
- Document model parameters and performance metrics

## Submission Guidelines

### Pull Request Process

1. Ensure your code follows the project's coding standards
2. Update documentation to reflect your changes
3. Add tests for new functionality
4. Update the CHANGELOG.md with your changes
5. Submit a pull request with a clear title and description
6. Address any feedback from reviewers

### Commit Messages

Follow these guidelines for commit messages:

- Use the present tense ("Add feature" not "Added feature")
- Use the imperative mood ("Move cursor to..." not "Moves cursor to...")
- Limit the first line to 72 characters
- Reference issues and pull requests after the first line

Example:
```
Add AORSF hyperparameter tuning function

- Implement grid search for AORSF parameters
- Add cross-validation framework
- Document new function with examples
- Add tests for parameter validation

Fixes #123
```

## Review Process

All submissions require review before being merged:

1. A project maintainer will review your pull request
2. Automated tests will run to check for issues
3. Feedback may be provided for necessary changes
4. Once approved, a maintainer will merge your pull request

## Community

### Communication Channels

- GitHub Issues: For bug reports and feature requests
- Project Wiki: For documentation and guides
- Email: For private communications with maintainers

### Recognition

All contributors will be acknowledged in the project's README.md file. Significant contributions may result in co-authorship on related publications.

### Versioning

This project follows [Semantic Versioning](https://semver.org/):

- MAJOR version for incompatible API changes
- MINOR version for new functionality in a backward-compatible manner
- PATCH version for backward-compatible bug fixes

## Thank You!

Your contributions help improve survival prediction for liver transplant waitlist patients. We appreciate your time and effort in making this project better!