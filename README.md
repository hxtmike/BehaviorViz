# BehaviorViz

BehaviorViz is a dashboard tool that allows a user to upload any BehaviorSpace file in the standard format (Table output) and explore the relationships between simulation inputs and output measures. BehaviorSpace is a software tool integrated with NetLogo that allows you to perform experiments with agent-based models.

For more information about the output files of BehaviorSpace, please visit the [BehaviorSpace Guide](https://ccl.northwestern.edu/netlogo/docs/behaviorspace.html) or [NetLogo Manual](https://ccl.northwestern.edu/netlogo/docs/)

## Usage

### Online Showcase

[Here](https://hxtmike.shinyapps.io/behaviorviz/) is an online showcase. It may not work due to the time limitation of [shinyapps.io by Posit](https://www.shinyapps.io/). As a result, the online showcase is not suitable for everyday use.

### Local Deployment

After downloading the full repository and installing `R 4.3+` and all packages listed in `ui.r` and `server.r`, you could deploy BehaviorViz on the local machine.

+ set working directory as the directory of BehaviorViz
+ run below code in R Console/Rstudio

    ```R
    library(shiny)
    runApp()
    ```

+ Alternatively, run it in a command line environment using `Rscript`

    ```Bash
    Rscript ./shiny.r
    ```

## Donation

Welcome to support me if you like `BehaviorViz` or help me to cover the expense of the [online version](https://hxtmike.shinyapps.io/behaviorviz/).

The donation page is [here](https://github.com/sponsors/hxtmike/)

## Contributions

If you'd like to contribute to the development, feel free to fork the repository and submit a pull request. I'll be happy to review your changes and merge them into the main codebase.

## License

All code in this project is covered under the MIT License
