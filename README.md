# PACE-HRH-UI

PACE-HRH is a simulation model that enables human resources for health (HRH) managers to explore future workforce needs. It offers several additional capabilities over other models, for example, allowing for sub-national level forecasts, incorporating statistical uncertainty into forecasts that generate a confidence interval (a range of likely values) for workload estimates, and allowing for workload to be estimated for specific cadres of workers. PACE-HRH enables users to investigate how needs differ across multiple different contexts that exist in a health system. In Ethiopia, for example, the model can forecast capacity for different health post types and explore the impact of programmatic changes, like adjustments to catchment size, health worker (HW) utilization, or task shifting between cadres. The open-source PACE-HRH model has been utilized in Ethiopia in collaboration with the Ministry of Health, MERQ Consultancy, Vital Wave, and the Institute for Disease Modeling (IDM) (part of the Gates Foundation). <br><br> [PACE-HRH-UI](https://github.com/InstituteforDiseaseModeling/PACE-HRH-UI) is a shinyapp that encapulates the
[PACE-HRH](https://github.com/InstituteforDiseaseModeling/PACE-HRH/releases)
package which employs stochastic simulation for capacity projections
based on excel spreadsheets configurations, it offers users a friendly
interface to visualization the input data and simulation results. Users
can also download and coompare results from different run for further
analysis.

For detailed technical documentation and training videos, visit the [wiki](https://github.com/vitalwaveinc/PACE-HRH-UI/wiki). 

### Download the app

Install the most recent version of the PACE-HRH-UI binary file from our
[releases](https://github.com/vitalwaveinc/PACE-HRH-UI/releases)

### Install and Run on Windows without Git and Rstudio

To install the app in a separate environment: <br> Download
`start_pace_ui.bat` and run it by double clicking it (or open a
commandline window to your downloaded folder to run it). If this is your first time installing, please follow the [step by step guide](https://github.com/vitalwaveinc/PACE-HRH-UI/wiki/Appendices#appendix-5-zip-file-installation) for installation and
running the app. <br> Once installed on your desired folder, for future
use, you should go to the subfolder with name PACE-HRH-UI-{version} and
run the start_pace_ui.bat from there, this will start the app in offline mode and open
your default browser so that you can interact with it. To close the app,
simply press any key on the command prompt window. <br> You can zip this
pre-installed self-contained folder and send it to those who do not have
internet access, they should be able to run `start_pace_ui.bat` without
installation. <br><br>
If you need to update the app when there is a [new release](https://github.com/vitalwaveinc/PACE-HRH-UI/releases) available, 
please follow this [video].

### Making Targeted Changes to Model Parameters
The PACE-HRH-UI app comes with predefined configurations files for Ethiopia, if you wish to update the model, please refer to the Technical Documentation section on [Making Targeted Changes to Model Parameters](https://github.com/vitalwaveinc/PACE-HRH-UI/wiki/Model-Implementation:-Making-Targeted-Changes-to-Model-Parameters). 

### Run the App Locally

The first step is torun `install_packages.R`, once you have all the
dependent packages installed, in RStudio open `app.R` and click on "Run
App". Alternatively you can launch the app from the RStudio console with
the following:

```         
library(shiny)
runApp(port=8888)
```

<br> This will launch your default browser and open
<http://localhost:8888>, you can use any port as you wish but if you
switch port between two runs, your data may not be saved in the same
local storage.

### Implementing the Model 

For more information on implementing the model, please refer to the Technical Documentation section on [Running the Model](https://github.com/vitalwaveinc/PACE-HRH-UI/wiki/Model-Implementation:-Running-the-Model).

