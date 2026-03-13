# SAS Methods Workshop 2026 (GBE)
Author: Joey Heffner
Last Updated: 03/13/2026

This Shiny application provides an interactive platform for visualizing and modeling happiness data from the **Great Brain Experiment**. I cleaned a subset of the data for visualization, but the full dataset can be found below under Research Background. 

## Research Background
The app implements computational models based on:
* **Happiness Model:** [Rutledge et al. (2014), PNAS](https://www.pnas.org/doi/10.1073/pnas.1407535111?url_ver=Z39.88-2003&rfr_id=ori%3Arid%3Acrossref.org&rfr_dat=cr_pub++0pubmed). A computational and neural model of momentary happiness.
* **Data Source:** [Rutledge (2021, Dryad)](https://datadryad.org/dataset/doi:10.5061/dryad.prr4xgxkk). Public dataset from the Great Brain Experiment.

## Current Features
* **Participant Selection:** Toggle between individual subjects to see data and model fits.
* **Happiness Modeling:** Compare raw happiness ratings with the model predictions.
* **Parameter Playground:** Manually tune model parameters via sliders to see real-time effects on the model likelihood. 
* **SSE Surface Mapping:** Visualize the optimization landscape (gradient space) for any two parameters of the happiness model.
* **Behavioral Analysis:** Summary statistics.

## Getting Started

### Prerequisites
You will need **R** , **RStudio**, and the following packages:
```r
install.packages(c("shiny", "tidyverse", "scales"))
```

### Starting the app
Open RStudio and make sure to select the project (`SAS_2026_GBE_App.Rproj`) to ensure relative paths work. Open the `app.R` file and click "Run App" to launch the Shiny app locally. 

## Future plans

In the future, I plan to implement more behavioral modeling using Prospect Theory. This was made as a demonstration for a methods talk for the Society for Affective Science (2026) and focused on the happiness model. I hope it's useful and serves as a foundation for affective scientists to develop their own paradigms and test their theories of how affective features (whether discrete emotions, appraisals, or other features) change within a task. 
