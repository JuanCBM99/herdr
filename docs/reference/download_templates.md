# Download Template Files from GitHub

Downloads example data files (templates) from the GitHub repository and
saves them to a local folder.

## Usage

``` r
download_templates(dest_folder = "user_data")
```

## Arguments

- dest_folder:

  The name of the folder where the templates will be saved. Defaults to
  "user_data".

## Value

The absolute path to the \`dest_folder\` where the files were saved.

## Examples

``` r
if (FALSE) { # \dontrun{
 # Call the function to download the files
 data_path <- download_templates()

 # View the downloaded files
 list.files(data_path)

 # Read one of the files
 read.csv(file.path(data_path, "livestock_definitions.csv"))
} # }
```
