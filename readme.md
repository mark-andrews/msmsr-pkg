# msmsr: Model Selection and Model Simplification Resources

This R package provides datasets, functions, and utilities for the two-day workshop on **Model Selection and Model Simplification** for empirical researchers.

## Installation

You can install the development version from a local directory:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install msmsr from local directory
devtools::install_local("path/to/msmsr-pkg")
```

Or from GitHub (once uploaded):

```r
devtools::install_github("username/msmsr")
```

## Quick Start

```r
# Load the package
library(msmsr)

# Load all course datasets at once
load_course_data()

# Or load specific datasets
ames <- ames_housing()

# Install additional suggested packages
install_extras()
```

## Available Datasets

- **Ames Housing** (`ames_housing()`): Residential property sales in Ames, Iowa (2006-2010)
  - 2930 observations, 81 variables
  - Includes physical characteristics, location, quality ratings, and sale prices

## Package Philosophy

This package is designed to make the course experience smooth:

1. **Single Installation**: Install `msmsr` and all dependencies are handled automatically
2. **No Multiple Library Calls**: Just `library(msmsr)` - no need to load multiple packages
3. **Consistent Interface**: All data accessed through simple functions
4. **Real Data**: All datasets are real-world data, not simulated

## Main Functions

- `ames_housing()`: Get the Ames housing dataset
- `load_course_data()`: Load all course datasets into global environment
- `install_extras()`: Install suggested packages for enhanced functionality

## Development

To generate documentation and NAMESPACE:

```r
# Install roxygen2 if needed
install.packages("roxygen2")

# Generate documentation
devtools::document()

# Check package
devtools::check()

# Load for testing
devtools::load_all()
```

## License

MIT License
