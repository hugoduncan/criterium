# Histogram Core Requirements:

## Input

Required: Vector of numeric values
Optional: Pre-computed IQR value
Only validation is non-empty vector


## Bin Determination

Use Freedman-Diaconis rule for bin width:

width = 2 * IQR * n^(-1/3)
Calculate IQR if not provided


Number of bins derived from:

Data range (max - min)
Computed bin width


Bin edges should exactly cover data range
All data points must be included in binning


## Output Format

Return a map containing:

Vector of bin counts
Vector of bin centers
Bin width (constant)
Vector of probability density values
Total number of samples (for verification)
Min and max values (for verification)


## Error Handling

Empty input vector throws
Degenerate cases (all same value) throws
