## Covid-O-Meter

https://alhenry.shinyapps.io/covid-o-meter/

Covid-O-Meter (or covidometer) is an open access, daily-updated interactive web app designed to
track and visualise various statistics related to the Covid-19 pandemic.

Covid-O-Meter is built on [R Shiny](https://shiny.rstudio.com/) framework using the [John Hopkins CSSE Covid-19 Dataset](https://github.com/CSSEGISandData/COVID-1).


### Motivation & Aims

Since the beginning of the Covid-19 outbreak,
there are already many sophisticated stasticial models, descriptive statistics, and data visualisations
arising from various projects from across the globe
(for a list, see [The Coronavirus Tech Handbook Infographics collection](https://coronavirustechhandbook.com/infographics)).

Each of these projects has its own perks and tells a different story.
For a complex and worldwide problem such as Covid-19, however,
narratives on specific problems on certain population or areas
may not always fit well in other context.
A combination of domain expertise and local knowledge is often required
to make sense of a specific subset of the data.

To aid this time-consuming data sleuthing work,
Covid-o-meter provides a simple interface for users
to navigate through different parts of the dataset directly and to
make their own discoveries.

### Contents

#### Case statistics

* Cumulative Number of cases
* Number of new cases

#### Death statistics

* Cumulative number of deaths
* Number of new deaths
* Case fatality rate = Number of deaths (cumulative) / Number of cases (cumulative)

#### Animation

* Number of cases (per-day time-lapse)
* Number of deaths (per-day time-lapse)

### Plot elements

#### Y-Axis

The Y-axis represents the corresponding _statistics_ on a log base 2 scale (default  for number of cumulative / new cases /deaths) or a linear scale (default for case fatality rate).

The log base 2 scale is used to represent doubling rate due to the exponential nature of the disease spread (for discussion, see e.g. [explainer by John Burn-Murdoch](https://twitter.com/jburnmurdoch/status/1237748598051409921)).

#### X-Axis

The X-axis represents _day after first N cases (for case statistics) / deaths (for death statistics)_ on a one-unit increase linear scale.

The following example illustrates how the _day_ variable is calculated for case statistics if N is set to 100:

| Date            | Number of cases  | Day  |
|-----------------|-----------------:|-----:|
| 1 March 2020    | 88               | 0    |
| 2 March 2020    | 93               | 0    |
| 3 March 2020    | 105              | 1    |
| 4 March 2020    | 134              | 2    |
| 5 March 2020    | 199              | 3    |
| ...             | ...              | ...  |


### Interactive elements

#### Tooltip

On mouse hover, each data point (represented by dot) shows:

* Country name
* Date
* Day after first N cases / deaths (X-axis coordinate)
* Value of the statistics (Y-axis coordinate)

#### Parameters

The draggable sidebar contains functionality to adjust data and plot parameters as follows:

| Parameter                        | Description                                                                                                          |
|----------------------------------|----------------------------------------------------------------------------------------------------------------------|
| Countries                        | Add / remove countries to display (limited to a maximum of 20 to allow some physical distancing between data points) |
| Period                           | Adjust range of dates to use in calculation                                                                          |
| Minimum cases / deaths for day 1 | Minimum number of cases / deaths to be counted as Day 1                                                              |
| Range of days to display         | Lower and upper limit of day (i.e. the X-axis) to display                                                            |
| Scale                            | Select between linear / log base 2 scale for Y-Axis                                                                  |
| Show doubling rate               | Show / hide doubling rate for number of cases / deaths per 1, 3, and 7 days (hover on the lines for description)     |

After adjusting the desired parameters, __press the `Update plots / animation` button__ to see the change

_Note:_

* `Scale` and `Show doubling rate` will trigger automatic change on click
* Generating a new animation could take ~2 minutes to complete

#### Download
The download button will save a static version of the corresponding plot as `.png` file.
Use the width / height button to adjust the size (in inches).

_Note:_ Animation can be downloaded as a `.gif` image via web browser interface
(e.g. on Google Chrome press right-click then select `Save Image As...`)


### Contributing

Pull requests and suggestions are welcome.

Email: albert.henry.16@ucl.ac.uk


### Acknowledgements

This work uses the publicly available [Covid-19 dataset](https://github.com/CSSEGISandData/COVID-19) provided by John Hopkins CSSE.
Please read and follow the terms of use of the data.

This work is inspired by other similar works, e.g.
[Covid-19 charts](https://www.ft.com/coronavirus-latest) by [John Burn-Murdoch](https://twitter.com/jburnmurdoch) of Financial Times,
[91-Divoc project](https://91-divoc.com/pages/covid-visualization/) by [Wade Fagen-Ulmschneider](https://waf.cs.illinois.edu/), [Covid-19 Tracker](https://vac-lshtm.shinyapps.io/ncov_tracker/) by [Dr Edward Parker](https://www.lshtm.ac.uk/aboutus/people/parker.edward) and [Quentin Leclerc](https://qleclerc.netlify.com/) of The London School of Hygiene & Tropical Medicine
