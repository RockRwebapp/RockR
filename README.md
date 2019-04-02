<p align="center">
  <img src="www/Figs/RockR.png">
  <br>
  http://apps.earthsciences.iupui.edu:3838/RockR
</p>

# RockR

RockR! is a program designed to make plotting data onto geoscience discriminant and classification diagrams easy. It was created as a classroom and laboratory tool for students in petrology, mineralogy, geochemistry, sedimentology/stratigraphy, and introductory geology courses. The application streamlines the process of plotting data onto a variety of discrimination diagrams, as well as styling the plot with straightforward, yet extensive, aesthetic controls. The application output is publication quality and can be easily retrieved. The ease and efficiency of RockR! allows students and researchers using the app more time to devote to data interpretation, rather than lengthy plotting processes.

RockR! provides a graphical user interface to the R programming language. For ternary plots, RockR! wraps functionality provided by the R package ggtern. For TAS diagrams and metamorphic facies diagrams, RockR! wraps functionality provided in the R package ggplot2. Additional R packages used include Shiny, shinywidgets, shinyBS, shinycssloaders, colourpicker, readxl, readr, grid, gridExtra, knitr, ggalt, and png.

If you wish to download RockR! to contribute to or understand the behind-the-scenes workings of it, you can download the files here or fork this repository. If you do, make sure to read and adhere to the license agreement (link at the bottom of this README).

If you wish to see our reference list for RockR!, create a citation for RockR!, or contact the people behind RockR!, please click on the “Credits” tab.

If you’d like to learn how to input data into RockR!, including limitations on data input, click on the “Inputting Data” tab after clicking “Help”. You may also download sample data sets there.

For instructions to create and edit your plots, please click on the “Building Plots” tab after clicking “Help”.

There are a number of discrimination plots, diagrams, and curves pre-programmed into the software. Please click on the “Available Plots” tab to see the current list of pre-programmed options, their designations within the app, and the primary source literature references if available. You will see the “Available Plots” tab after clicking “Help”.

# Project Structure

RockR! is divided into three sections: ternary, bivariate, and metamorphic facies. The three sections allow you to plot data onto three different types of plots: ternary diagrams, bivariate plots, and metamorphic facies diagrams respectively. Depending on what plot you want to create, click on the “Bivariate”, “Ternary”, or “Mm Facies” tabs on the top of the page.

## Ternary Section
The ternary section includes an introductory walkthrough of ternary diagrams, how they work, and how to plot data onto them. You can plot data on the blank starting plot and assign your own labels to the three axes, or you can select a pre-programmed discrimination diagram using the drop-down menu on the right panel of the Plot tab.

Additional controls for adding or removing plot components are available below the main plot display. In addition, if you have chosen a discrimination diagram, further additional control for color and transparency for individual fields will show below the main plot.

## Bivariate Section
The plot axes can be manipulated within a reasonable range to customize plots for publication. Additional plot controls also allow for manipulation of points on the plot, alteration of axis limits, alteration of tick increments, and transformations (i.e. logarithmic).

The bivariate section includes an introductory walkthrough of total alkali-silica (TAS) plots, the classification reasoning for the different igneous rock types, and how to plot data onto them. For TAS plots, there are also options to add and remove a line dividing alkaline and subalkaline rock regions of the plot, and the rock type field labels may also be added or removed from the plot.

## Metamorphic Facies Section
The metamorphic facies section includes a brief introduction to metamorphic facies, including links to relevant literature. This section offers two different metamorphic facies plots, the ability to invert the plot to relate depth and pressure to each other visually, and numerous curves and additional features that can be toggled on and off.

# Contributors
- John T. Shukle
- Thomas E. zur Loye
- Jeremiah L. Mickey

- Twitter [@RRwebapp](https://twitter.com/RRwebapp)
- Facebook [@RockRwebapp](https://www.facebook.com/RockRwebapp/?eid=ARAR3piAw1ZiSVQWLZzVOEJa47Zn11XKpC8WY8s0Izli2wTlSi7mFzaOVdEIAg1y8UJYtcJ_XKD2jIs)
- Reddit [u/RockRwebapp](https://www.reddit.com/user/RockRwebapp/)

# Acknowledgments
- Indiana University - Purdue University of Indianapolis (IUPUI)
   - Department of Earth Sciences

# How to Cite RockR
- RockR (2019). Retrieved from http://apps.earthsciences.iupui.edu:3838/RockR/

# License
Licensed under the [GNU General Public License v3.0](https://github.com/RockRwebapp/RockR/blob/master/LICENSE)
