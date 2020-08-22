# Configuration

Datasets configuration can be provided in a `yaml` file or as a nested list. Below you can find a detailed description of possible options.

## Data frames in a dataset

A single yaml file can include multiple data frames. Entry for each will be used as name of the data frame when it comes to generating data.

```yaml
first_data_frame:
  ...
second_data_frame:
  ...
third_data_frame:
  ...
```

## Data frame configuration

Data frame configuration includes two sections:

1. `columns` - where you can describe columns of your data frame.
2. `default_size` - optional value that describes default size of the data frame.

### Columns

Each column of your data frame should be described in a separate entry in
columns section. Entry name will be used as column name.

Currently there are three major types of columns implemented:

1. Built-in basic columns (integer, numeric, string, boolean and set)
2. Columns that use custom function to be generated.
3. Columns calculated from other columns.

Type of column is set by choosing a proper `type` value in column
description. Check following sections for more details.

#### Built-in columns

##### integer

Random integers from a range

Parameters:

* `type: integer` - column type
* `unique` (optional, default: FALSE) - boolean, should values be unique
* `min` (optional, default: 0) - integer, minimum value to occur in the column.
* `max` (optional, default: 999999) - integer, maximum value to occur in the column.

Example:

```yaml
data_frame:
  columns:
    integer_column:
      type: integer
      min: 2
      max: 10
```

##### numeric

Random float numbers from a range

Parameters:

* `type: numeric` - column type
* `unique` (optional, default: FALSE) - boolean, should values be unique
* `min` (optional, default: 0) - numeric, minimum value to occur in the column.
* `max` (optional, default: 999999) - numeric, maximum value to occur in the column.

Example:

```yaml
data_frame:
  columns:
    numeric_column:
      type: numeric
      min: 2.12
      max: 10.3
```

##### string

Random string that follows given pattern

Parameters:

* `type: string` - column type
* `unique` (optional, default: FALSE) - boolean, should values be unique
* `length` (optional, default: NULL) - integer, string length. If NULL, string length will be random (see next parameters).
* `min_length` (optional, default: 1) - integer, minimum length if length is random.
* `max_length` (optional, default: 15) - integer, maximum length if length is random.
* `pattern` (optional, default: "[A-Za-z0-9]") - string pattern, for details check [this](https://rdrr.io/cran/stringi/man/stringi-search-charclass.html).

Example:

```yaml
data_frame:
  columns:
    string_column:
      type: string
      length: 3
      pattern: "[ACGT]"
```

##### boolean

Random boolean

Parameters:

* `type: boolean` - column type

Example:

```yaml
data_frame:
  columns:
    boolean_column:
      type: boolean
```

##### set

Column with elements from a set

Parameters:
* `type: set` - column type
* `set` (optional, default: NULL) - set of possible values, if NULL, will use a random set.
* `set_type` (optional, default: NULL) - type of random set, can be "integer", "numeric" or "string".
* `set_size` (optional, default: NULL) - integer, size of random set
* If set is random, you can add parameters required by type of set (eg, min, max, pattern, etc.)

Example:

```yaml
data_frame:
  columns:
    set_column_one:
      type: set
      set: ["aardvark", "elephant", "hedgehog"]
    set_column_two:
      type: set
      set_type: integer
      set_size: 3
      min: 2
      max: 10
```

#### Custom columns

There are two levels of custom generator that can be used.
You can provide a function that generates a single value or
a function that provides a whole column.

##### custom value generator

Generate column values using custom function available in your environment. Function should return a single value.

Parameters:

* `type: custom` - column type
* `custom_generator` - name of the function that will provide values.
* All parameters required by your custom function.

Example:

```r
return_sample_paste <- function(vector_of_values) {
  values <- sample(vector_of_values, 2)
  paste(values, collapse = "_")
}
```

```yaml
data_frame:
  columns:
    custom_column:
      type: custom
      custom_generator: return_sample_paste
      vector_of_values: ["a", "b", "c", "d"]
```

### Default size

__TODO__
