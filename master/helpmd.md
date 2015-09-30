---
title: "Help File"
author: "AA"
date: "Wednesday, September 30th, 2015"
output: html_document
---

<table class="table table-hover table-responsive">

<tbody>

<tr>

<td style="vertical-align: top; width: 1054px;"><span style="font-weight: bold; text-decoration: underline;">Scenario:</span>  

1.  Census 2011 Reported Cycle Commuting
2.  Government Target: this scenario is based on the UK government's proposed target (as set out in its draft [Cycling Delivery Plan](https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/364791/141015_Cycling_Delivery_Plan.pdf)) to double cycling in England. Taking population increases and trip rate decreases into account, the target implies doubling the proportion of trips made by cycling nationwide. Trips that have a large distances or that already have a high rate of cycling will see cycling increase less than two-fold; trips with a below average current rate of cycling but high potential (based on the distance) will see cycling increase more than two-fold.
3.  Gender equality: Women have the same cycling rate as men
4.  Go Dutch: Cycling propensity at the levels seen in the Netherlands
5.  Electric bicycles: Willingness to cycle longer trips increases with widespread use of electric bicycles

</td>

<td style="vertical-align: top; width: 535px;">![Scenario Selection](http://geo8.webarch.net/norfolk/assets/help/scenario-selection.png)
</td>

<tr>

<td style="vertical-align: top; width: 1054px;"><span style="font-weight: bold; text-decoration: underline;">Zone Attribute</span>  

1.  Scenario Level of Cycling is the model predicted rate of cycling in the given area for the chosen scenario  

2.  Scenario Increase in Cycling is the corresponding number of additional cycle commuters over and above Census 2011  

3.  None: No zone attribute is displayed  

</td>

<td style="vertical-align: top; width: 535px;">![Zone Attribute](http://geo8.webarch.net/norfolk/assets/help/zone-attribute.png)  
</td>

</tr>

<tr>

<td style="vertical-align: top; width: 1054px;"><span style="font-weight: bold; text-decoration: underline;">Cycling Flows</span>  

1.  Straight Lines between the population weighted centroids of Census Medium Super Output Areas  

2.  'Fastest' Route using [Cycle Streets Routing](http://www.cyclestreets.net/journey/help/howitworks/)  

3.  'Fastest' and 'Quietest' Routes using Cycle Streets Routing

</td>

<td style="vertical-align: top; width: 535px;">![Cycling Flows](http://geo8.webarch.net/norfolk/assets/help/cycling-flows.png)  
</td>

</tr>

<tr>

<td style="vertical-align: top; width: 1054px;"><span style="font-weight: bold; text-decoration: underline;">Freeze Lines</span>  

1.  Ticked: The top cycling flows shown remain those calculated based on the visible map area when the box was ticked  

2.  Unticked: If the visible map area changes, flows are recalculated accordingly

</td>

<td style="vertical-align: top; width: 535px;">![Fixed Lines](http://geo8.webarch.net/norfolk/assets/help/freeze-lines.png)  
</td>

</tr>

<tr>

<td style="vertical-align: top; width: 1054px;"><span style="font-weight: bold; text-decoration: underline;">Line Attribute</span>

1.  Scenario Level of Cycling is the model predicted rate of cycling between an origin destination pair for the chosen scenario
2.  Scenario Increase in Cycling is the corresponding number of additional cycle commuters that can be expected between the origin destination pair  

</td>

<td style="vertical-align: top; width: 535px;">![Line Attribute](http://geo8.webarch.net/norfolk/assets/help/line-attribute.png)  
</td>

</tr>

<tr>

<td style="vertical-align: top; width: 1054px;"><span style="font-weight: bold; text-decoration: underline;">Flows to Show</span>  
When viewing flows, this slider selects how many of the of the top 50 are shown</td>

<td style="vertical-align: top; width: 535px;">![Number of Lines to Show](http://geo8.webarch.net/norfolk/assets/help/number-of-lines.png)  
</td>

</tr>

<tr>

<td style="vertical-align: top; width: 1054px;"><span style="font-weight: bold; text-decoration: underline;">Fastest Route</span> The purple line represents the 'Fastest' cycling route calculated by CycleStreets between the population weighted centroid of two zones. Once selected the following information is displayed:

1.  Total commutes 2011: The total number of commuting trips between these two zones
2.  Cycling 2011: Reported cycle commutes between these two zones
3.  Scenario Cycling: The model predicted rate of cycling between an origin destination pair for the chosen scenario
4.  Scenario Increase: The difference between the Census 2011 Cycling and the Scenario  

5.  Route Distance: The route distance in km between the centre of the zones

<span style="font-weight: bold; text-decoration: underline;"></span></td>

<td style="vertical-align: top; width: 535px;">![Direct Route](http://geo8.webarch.net/norfolk/assets/help/direct-route.png)  
</td>

</tr>

<tr>

<td style="vertical-align: top;"><span style="font-weight: bold; text-decoration: underline;">Quiet Route</span> The light blue line represents the 'Quitest' cycling route calculated by CycleStreets between the population weighted centroid of two zones. Once selected the following information is displayed:

1.  Total commutes 2011: The total number of commuting trips between these two zones
2.  Cycling 2011: Reported cycle commutes between these two zones
3.  Scenario Cycling: The model predicted rate of cycling between an origin destination pair for the chosen scenario
4.  Scenario Increase: The difference between the Census 2011 Cycling and the Scenario
5.  Route Distance: The route distance in km between the centre of the zones

</td>

<td style="vertical-align: top;">![Quiet Route](http://geo8.webarch.net/norfolk/assets/help/quiet-route.png)  
</td>

</tr>

<tr>

<td style="vertical-align: top;"><span style="font-weight: bold; text-decoration: underline;">Straight Lines</span> The red line represents a straight line between the population weighted centroid of two zones. Once selected the following information is displayed:

1.  Total commutes 2011: The total number of commuting trips between these two zones
2.  Cycling 2011: Reported cycle commutes between these two zones
3.  Scenario Cycling: The model predicted rate of cycling between an origin destination pair for the chosen scenario
4.  Scenario Increase: The difference between the Census 2011 Cycling and the Scenario
5.  Route Distance: The route distance in km between the centre of the zones

</td>

<td style="vertical-align: top;">![Straight Line](http://geo8.webarch.net/norfolk/assets/help/straight-line-popup.png)  
</td>

</tr>

<tr>

<td style="vertical-align: top;"><span style="font-weight: bold; text-decoration: underline;">Zone Information</span> Zone information is displayed on clicking on the black circle in the centre of each zone:

1.  Zone: Zone name according to the Census
2.  Scenario Level of Cycling: the model predicted rate of cycling in the given area for the chosen scenario
3.  Scenario Increase in Cycling: the corresponding number of additional cycle commuters over and above Census 2011
4.  Hilliness: Shows the average hilliness of the zone from [digital elevation model](http://srtm.csi.cgiar.org/) data supplied by NASA.

</td>

<td style="vertical-align: top;">![Zone](http://geo8.webarch.net/norfolk/assets/help/zone-popup.png)  
</td>

</tr>

<tr>

<td style="vertical-align: top;"><span style="font-weight: bold; text-decoration: underline;">Zone Attribute Legend</span> The legend shows quartiles for the displayed zone attribute according to the scenario selected.</td>

<td style="vertical-align: top;">![Legend](http://geo8.webarch.net/norfolk/assets/help/legend.png)</td>

</tr>

</tr>

</tbody>

</table>
