# LIFE-SDVS

`LIFE-SDVS`, LIFE Spatial Data Visualization System, is a software system that I wrote for my master thesis in computer sciecne at University of Leipzig in 2016. 

LIFE stands for the Leipzig Research Centre for Civilization Diseases. It is part of the Medical Faculty of the University of Leipzig, Germany, and conducts a large medical research project focusing on civilization diseases in the Leipzig population. The analyses in LIFE have been mostly limited to non-spatial aspects. To integrate geographical facet into the findings, a spatial visualization tool is necessary. Hence, `LIFE-SDVS`, an automatic map visualization tool wrapped in an interactive web interface, is constructed. 

The implementation of LIFE-SDVS was achieved by two software components: an independent, self-contained R package `lifemap` and the `LIFE Shiny Application`. 

## `lifemap`
The package `lifemap` enables the automatic spatial visualization of statistics on the map of Leipzig and to achieve boundary labeling for maps. The package `lifemap` also contains two self-developed algorithms. The `Label Positioning Algorithm` was constructed to find good positions within each region on a map for placing labels, statistical graphics and as starting points for boundary label leaders. The `Label Alignment Algorithm` solves the leader intersection problem of boundary labeling.


## `LIFE Shiny Application`
However, to use the plotting functions in lifemap, the users need to have basic knowledge of R and it is a tedious job to manually input the argument values whenever changes on the maps are necessary. An interactive Shiny web application, the `LIFE Shiny Application`, is therefore built to create a user friendly data exploration and map generation tool. `LIFE Shiny Application` is capable of obtaining experimental data directly from the LIFE database at runtime. Additionally, a data preprocessing unit can transform the raw data into the format needed for spatial visualization. On the `LIFE Shiny Application` user interface, users can specify the data to display, including what data to be fetched from database and which part of the data shall be visualized, by using the filter functions provided. Many map features are also available to improve the aesthetic presentation of the maps. The resulting maps can also be downloaded for further usage in scientific publications or reports.


Due to copyright, the map data are not included in this repository. If you want to learn more about this software, please read the following publication:

Lin, Y.-C., Groß, A., Kirsten, T. (2017) Integration and visualization of spatial data in LIFE. it - Information Technology 59(4): 161-170.


