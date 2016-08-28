# ADLModelling
The repository contains an R script to generate human routine model.
The data for this R script can be obtained from https://archive.ics.uci.edu/ml/datasets/Activities+of+Daily+Living+%28ADLs%29+Recognition+Using+Binary+Sensors
The data is provided by Francisco Javier OrdÃ³Ã±ez, Carlos III University of Madrid, fordonez '@' inf.uc3m.es

Open the LoadAndProcessAdlData.R file for more instructions.

The LoadAndProcessAdlData.R script reads a file, for individual B in the above dataset, having Activity of Daily Living (ADL) with start and end timestamp in order to model that person's routine.

This script illustrates the Human Routine Model as described in the article "Leveraging Graph Databases & Analytics for IoT Applications" published in both the below links- 
* https://www.linkedin.com/pulse/leveraging-graph-databases-analytics-iot-applications-john-abraham?articleId=8207721914121564403
* https://midnytphilosopher.wordpress.com/2016/08/24/graph-databases-analytics-for-iot-applications-human-routine-model/

The script assumes that you have Neo4j graph server installed on your computer and these packages installed in R - dplyr, lubridate, RNeo4j, sqldf
