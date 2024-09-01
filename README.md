# Project Description

The first part of this shiny app is a strike zone visual that categorizes pitches based on outcome. The user may select a pitcher and a time frame bounded by a start date and end date. From there, the user may choose from filtered lists of pitch types and event outcomes that specifically occur when the selected player played. Upon clicking the Update button, the resulting visual will display the recorded location of where the pitch crossed home plate, with respect to the average strike zone. The points are furthter differentiated inside of the visual by color, with different colors representing strikes, balls, and hits, as per the legend.

The second tab includes a frequency table of event outcomes over time, dependent on pitch type. The inputs for this visual are the same as for the strike zone, though the only specific input able to be modified on this tab are the pitch type and outcome type. Upon clicking the Update button specifically on this tab, the app produces a graphic with each data point representing the count of the selected outcome, achieved with the selected pitch, during each specific game in the time interval. The smooth line plotted within the graphic fits a smooth trend line to model the average frequency of the selected statistic over time.

The data used for this application is statcast pitch-by-pitch data collected for the years 2017 through 2022. We further filtered this data to include only the 100 pitchers with the most pitches thrown between 2017 and 2022.


Authors: Cameron Kerkemeyer, Zach Larson

Data Source: “Statcast Search.” Baseballsavant.Com, baseballsavant.mlb.com/statcast_search. Accessed 1 May 2024.
