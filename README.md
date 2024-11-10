# RhinoDCM1.0

*TODO: brief description*

## Table of Contents

- [Installation](#installation)
    - [System Dependencies](#system-dependencies)
    - [R Dependencies](#r-dependencies)
- [Useful Resources for Rhino R Package](#useful-resources-for-rhino-r-package)
    - [Links](#links)
    - [Other Commands](#other-commands)
- [Usage](#usage)
- [License](#license)

## Installation

### System Dependencies

To set up the necessary system dependencies, execute the `setup.sh` script provided in the root directory of the project. This script will update your package lists and install all required system packages.

1. **Clone the Repository:**

   ```bash
   git clone https://github.com/YaliniNadar/RhinoDCM1.0
   cd RhinoDCM1.0
   ```

2. **Make the Setup Script Executable:**

TODO: add other setup scripts
   Ensure the `setup.sh` script has execute permissions.

   ```bash
   chmod +x setup.sh
   ```

3. **Run the Setup Script:**

   Execute the script to install system dependencies.

   ```bash
   ./setup.sh
   ```

### Required Packages

- **`git-all`**

- **`r-base`**

- **`libssl-dev`**

- **`libcurl4-openssl-dev`**

- **`libfontconfig1-dev`**

- **`libfreetype6-dev`**

- **`libharfbuzz-dev`**

- **`libfribidi-dev`**

- **`libpng-dev`**

- **`libtiff5-dev`**

- **`libjpeg-dev`**

- **`libxml2-dev`**

- **`libgit2-dev`**

- **`pkg-config`**

- **`build-essential`**

- **`libyaml-dev`**

- **`libgit2-28`**


### R Dependencies

Ensure you have `renv` installed to manage R dependencies. Then, restore the project dependencies:

1. **Install `renv` (if not already installed):**

   ```r
   install.packages("renv")
   ```

2. **Restore Dependencies:**

   This command installs all required R libraries as specified in `renv.lock`.

   ```r
   renv::restore()
   ```

## Useful Resources for Rhino R Package

### Links

1. **[Website Tutorial](https://appsilon.com/rhino-r-package-tutorial/):** A comprehensive tutorial on using the Rhino R package.
2. **[YouTube Workshop Tutorial](https://www.youtube.com/watch?v=8H_ZHUy8Yj4&t=3330s):** A detailed workshop walkthrough.
3. **[Rhino Documentation](https://appsilon.github.io/rhino/):** Official documentation for the Rhino package.
4. **[Routing in Rhino](https://appsilon.github.io/shiny.router/articles/rhino.html#change-pages-from-ui):** Guide on routing within Rhino applications.

### Other Commands

Here are some additional commands to help manage your Rhino project:

1. **Initialize a New Rhino Project:**

   Create a new Rhino application named "RhinoApplication".

   ```r
   rhino::init("RhinoApplication")
   ```

2. **Run the Application:**

   Start the Shiny app.

   ```r
   shiny::runApp()
   ```

3. **Take a Snapshot of Dependencies:**

   Capture the current state of your R dependencies.

   ```r
   renv::snapshot()
   ```

4. **Build JavaScript Code:**

   Compile the necessary JavaScript for the Rhino application.

   ```r
   rhino::build_js()
   ```

## Usage

*TODO: Provide instructions and examples on how to use*

## License

*TODO: Specify the license*