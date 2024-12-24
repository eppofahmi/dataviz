# Data Visualization Tool

An interactive R Shiny application for creating various types of plots using echarts4r.

## Features

- Multiple plot types support
- Interactive visualizations
- Data upload capability
- Aggregation options
- Export functionality

## Plot Types and Requirements

### Currently Implemented

1. **Column/Bar Chart**
   - Required inputs: 
     - Y-variable (numeric)
     - X-variable (categorical)
     - Optional: Group variable
   - Supports aggregation: sum, count

2. **Line Chart**
   - Required inputs:
     - Y-variable (numeric)
     - X-variable (categorical/date)
     - Optional: Group variable
   - Supports aggregation: sum, count

3. **Area Chart**
   - Required inputs:
     - Y-variable (numeric)
     - X-variable (categorical/date)
     - Optional: Group variable
   - Supports aggregation: sum, count

4. **Pie Chart**
   - Required inputs:
     - Value variable (numeric)
     - Category variable
   - Supports aggregation: sum, count

5. **Donut Chart**
   - Required inputs:
     - Value variable (numeric)
     - Category variable
   - Supports aggregation: sum, count

6. **Rose Chart**
   - Required inputs:
     - Value variable (numeric)
     - Category variable
   - Supports aggregation: sum, count

7. **Map**
   - Required inputs:
     - Longitude (numeric)
     - Latitude (numeric)
     - Value variable (numeric)
   - Supports aggregation: sum, count
   - Features: Points or Heatmap display

8. **Boxplot**
   - Required inputs:
     - Value variable (numeric)
     - Group variable (categorical)
   - No aggregation
   - Shows statistical summary

9. **Scatter Plot**
   - Required inputs:
     - X-variable (numeric)
     - Y-variable (numeric)
     - Optional: Z-variable for color (numeric)
   - No aggregation
   - Shows correlation

### To Be Implemented

1. **Histogram**
   - Single numeric variable distribution
   - Configurable bins
   - No aggregation needed

2. **Density Plot**
   - Single numeric variable
   - Optional grouping
   - No aggregation needed

3. **Violin Plot**
   - Numeric variable for distribution
   - Categorical variable for groups
   - No aggregation needed

4. **Correlation Heatmap**
   - Multiple numeric variables
   - Shows correlation matrix
   - No aggregation needed

5. **Bubble Chart**
   - X-variable (numeric)
   - Y-variable (numeric)
   - Size variable (numeric)
   - Optional color variable
   - Optional aggregation

## Development Guide

### Project Structure

```
.
├── app.R              # Main application file
├── R/
│   ├── data_processing/
│   │   ├── aggregate_data.R
│   │   ├── process_boxplot_data.R
│   │   ├── process_scatter_data.R
│   │   └── aggregate_map_data.R
│   ├── plot_generation/
│   │   ├── generate_plot.R
│   │   ├── generate_boxplot.R
│   │   ├── generate_scatter_plot.R
│   │   └── generate_map_plot.R
│   └── utils/
│       └── helpers.R
└── www/
    └── styles.css
```

### Key Components

1. **Data Processing Functions**
   - Handle data validation
   - Perform aggregations
   - Calculate statistics
   - Return consistent structure:
     ```r
     list(
       data = processed_data,
       message = status_message,
       plot_message = plot_status
     )
     ```

2. **Plot Generation Functions**
   - Create echarts4r visualizations
   - Handle interactive features
   - Apply consistent styling

3. **UI Components**
   - Conditional inputs based on plot type
   - Dynamic variable selection
   - Appropriate aggregation options

### Adding New Plot Types

1. Create data processing function:
   ```r
   process_new_plot_data <- function(df, ...) {
     # Validation
     # Processing
     # Return standard structure
   }
   ```

2. Create plot generation function:
   ```r
   generate_new_plot <- function(processed_data, ...) {
     # Create and return echarts4r plot
   }
   ```

3. Add UI elements:
   ```r
   conditionalPanel(
     condition = "input.plot_type == 'new_plot'",
     # Required inputs
   )
   ```

4. Update server logic in eventReactive

## Usage Guide

### Data Requirements

- CSV file format
- Clean column names
- Appropriate data types:
  - Numeric for values
  - Categorical for grouping
  - Date/time in standard format

### Plot Selection Guidelines

1. For Distribution Analysis:
   - Boxplot: Compare distributions across groups
   - Scatter: Examine relationships between variables

2. For Composition:
   - Pie/Donut: Show parts of a whole
   - Stacked Column: Show parts over categories

3. For Trends:
   - Line: Time series or ordered categories
   - Area: Cumulative or stacked trends

4. For Comparison:
   - Column: Compare categories
   - Bar: Compare categories (horizontal)

5. For Geographic Data:
   - Map: Show spatial patterns
   - Heatmap: Show density patterns

### Best Practices

1. Choose appropriate plot types for your data
2. Use aggregation when dealing with large datasets
3. Consider using grouping for more detailed analysis
4. Export results for further use
5. Check statistical summaries in the data table

## Future Enhancements

1. Additional plot types
2. More customization options
3. Advanced statistical features
4. Enhanced export capabilities
5. Custom themes support

## Dependencies

- shiny
- dplyr
- echarts4r
- DT
- tidyr