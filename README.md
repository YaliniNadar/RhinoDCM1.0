## Useful Resources for Rhino R package

### Links
1. Website Tutorial: https://appsilon.com/rhino-r-package-tutorial/
2. YouTube Workshop Tutorial: https://www.youtube.com/watch?v=8H_ZHUy8Yj4&t=3330s
3. Rhino Documentation: https://appsilon.github.io/rhino/
4. Routing in Rhino: https://appsilon.github.io/shiny.router/articles/rhino.html#change-pages-from-ui

### To Run an existing Project 
1. Clone Repo (make sure you have `renv.lock` file)
3. Restore Dependencies (installs all required libraries): ```renv::restore()```
4. Run app: ```shiny::runApp()```

### Other Commands
1. Initialize a new Rhino project: ```rhino::init(“RhinoApplication”)```
2. Run app: ```shiny::runApp()```
3. Take a snapshot in R console: ```renv::snapshot()```
4. Build JS Code: 
```rhino::build_js()```