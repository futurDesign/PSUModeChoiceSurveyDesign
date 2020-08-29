#Mike McQueen
#Portland State Univeristy
#Design of Mode Choice Stated Choice Experiment

#Load Relevant Packages
library(choiceDes)
library(AlgDesign)
library(dplyr)
library(tidyr)
library(htmlTable)

##Generate the set of alternatives and alternative-specific attributes/levels
levs <- list(rep(3,9))

## Find the Optimal Design
des <- dcm.design(cand = levs, nb = 4, sets = 6, alts = 1, Rd = 20, fname = "design.txt")
save(des, file = "des_021020.RData")

## Code design for Qualtrics
des_text <- read.table("4_blocks_6_ques_021020.txt", header = TRUE)

#Fix the column names
names(des_text) <- c("card", "vers", "task", "car_walk_time", "car_drive_time", "car_parking_cost", "bike_ride_time",
                     "em_walk_time", "em_scoot_time", "em_ride_time", "em_scoot_cost", "em_ticket_cost")

#Assign level values
d <- des_text %>% 
  mutate(car_walk_time = recode(car_walk_time, `1` = 0, `2` = 2, `3` = 4),
         car_drive_time = recode(car_drive_time, `1` = 25, `2` = 35, `3` = 45),
         car_parking_cost = recode(car_parking_cost, `1` = 7, `2` = 11, `3` = 15),
         bike_ride_time = recode(bike_ride_time, `1` = 25, `2` = 35, `3` = 45),
         em_walk_time = recode(em_walk_time, `1` = 0, `2` = 2, `3` = 4),
         em_scoot_time =  recode(em_scoot_time, `1` = 5, `2` = 10, `3` = 15),
         em_ride_time =  recode(em_ride_time, `1` = 25, `2` = 30, `3` = 35),
         em_scoot_cost =  recode(em_scoot_cost, `1` = 0.50, `2` = 1.75, `3` = 3.00),
         em_ticket_cost =  recode(em_ticket_cost, `1` = 1.25, `2` = 2.50, `3` = 3.75))

#Export this key
key <- d
save(key, file = "key_021020.Rdata")

#Export html tables

for(i in 1:nrow(d)) {
write(sprintf("<style type=\"text/css\">.tg  {border-collapse:collapse;border-spacing:0;width:auto;}
.tg td{font-family:Arial, sans-serif;font-size:14px;padding:10px 20px;border-style:solid;border-width:1px;word-break:normal;border-color:black;display: table-cell !important;vertical-align: inherit !important;}
      .tg th{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 20px;border-style:solid;border-width:1px;word-break:normal;border-color:black;}
      .tg .tg-v0hj{font-weight:bold;background-color:#efefef;border-color:inherit;text-align:center;vertical-align:top}
      .tg .tg-0pky{border-color:inherit;text-align:left;vertical-align:top}
      </style>
      <div style=\"display: block; width: 100%%; overflow-x: auto;\">
      <table border=\"3\" class=\"tg\" summary=\"I choose personal car.\">
      <tbody>
      <tr>
      <th class=\"tg-v0hj\" colspan=\"2\">Personal Car</th>
      <th class=\"tg-v0hj\">Totals:</th>
      </tr>
      <tr>
      <td class=\"tg-0pky\" colspan=\"1\">Drive time:</td>
      <td class=\"tg-0pky\" colspan=\"1\">%2.0f min</td>
      <td class=\"tg-0pky\" colspan=\"1\" rowspan=\"2\"><strong>%2.0f min</strong></td>
      </tr>
      <tr>
      <td class=\"tg-0pky\" colspan=\"1\">Walking time:</td>
      <td class=\"tg-0pky\" colspan=\"1\">%2.0f min</td>
      </tr>
      <tr>
      <td class=\"tg-0pky\">Parking Cost:</td>
      <td class=\"tg-0pky\">$%.2f</td>
      <td class=\"tg-0pky\"><strong>$%.2f</strong></td>
      </tr>
      </tbody>
      </table>
      </div>
      =================
      <style type=\"text/css\">.tg  {border-collapse:collapse;border-spacing:0;width:auto;}
      .tg td{font-family:Arial, sans-serif;font-size:14px;padding:10px 20px;border-style:solid;border-width:1px;word-break:normal;border-color:black;display: table-cell !important;vertical-align: inherit !important;}
      .tg th{font-family:Arialv, sans-serif;font-size:14px;font-weight:normal;padding:10px 20px;border-style:solid;border-width:1px;word-break:normal;border-color:black;}
      .tg .tg-v0hj{font-weight:bold;background-color:#efefef;border-color:inherit;text-align:center;vertical-align:top}
      .tg .tg-0pky{border-color:inherit;text-align:left;vertical-align:top}
      </style>
      <div style=\"display: block; width: 100%%; overflow-x: auto;\">
      <table border=\"3\" class=\"tg\">
      	<tbody>
      		<tr>
      			<th class=\"tg-v0hj\" colspan=\"2\">Bike</th>
      			<th class=\"tg-v0hj\">Totals:</th>
      		</tr>
      		<tr>
      			<td class=\"tg-0pky\">Walking time:</td>
      			<td class=\"tg-0pky\">0 min</td>
      			<td class=\"tg-0pky\" colspan=\"1\" rowspan=\"2\"><strong>%2.0f mins</strong></td>
      		</tr>
      		<tr>
      			<td class=\"tg-0pky\">Bike time:</td>
      			<td class=\"tg-0pky\">%2.0f min</td>
      		</tr>
      		<tr>
      			<td class=\"tg-0pky\">Cost:</td>
      			<td class=\"tg-0pky\">Free</td>
      			<td class=\"tg-0pky\"><strong>Free</strong></td>
      		</tr>
      	</tbody>
      </table>
      </div>
      =================
      <style type=\"text/css\">.tg  {border-collapse:collapse;border-spacing:0;width:auto;}
      .tg td{font-family:Arial, sans-serif;font-size:14px;padding:10px 20px;border-style:solid;border-width:1px;word-break:normal;border-color:black;display: table-cell !important;vertical-align: inherit !important;}
                    .tg th{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 20px;border-style:solid;border-width:1px;word-break:normal;border-color:black;}
                    .tg .tg-v0hj{font-weight:bold;background-color:#efefef;border-color:inherit;text-align:center;vertical-align:top}
                    .tg .tg-0pky{border-color:inherit;text-align:left;vertical-align:top}
                    </style>
                    <div style=\"display: block; width: 100%%; overflow-x: auto;\">
                    <table border=\"3\" class=\"tg\">
                    <tbody>
                    <tr>
                    <th class=\"tg-v0hj\" colspan=\"2\">E-Scooter + MAX</th>
                    <th class=\"tg-v0hj\" colspan=\"\">Totals:</th>
                    </tr>
                    <tr>
                    <td class=\"tg-0pky\">Walking time:</td>
                    <td class=\"tg-0pky\">%2.0f min</td>
                    <td class=\"tg-0pky\" colspan=\"1\" rowspan=\"3\"><strong>%2.0f min</strong></td>
                    </tr>
                    <tr>
                    <td class=\"tg-0pky\">E-Scooter time:</td>
                    <td class=\"tg-0pky\">%2.0f min</td>
                    </tr>
                    <tr>
                    <td class=\"tg-0pky\">MAX time:</td>
                    <td class=\"tg-0pky\">%2.0f min</td>
                    </tr>
                    <tr>
                    <td class=\"tg-0pky\">E-scooter cost:</td>
                    <td class=\"tg-0pky\">$%.2f</td>
                    <td class=\"tg-0pky\" colspan=\"1\" rowspan=\"2\"><strong>$%.2f</strong></td>
                    </tr>
                    <tr>
                    <td class=\"tg-0pky\">MAX cost:</td>
                    <td class=\"tg-0pky\">$%.2f</td>
                    </tr>
                    </tbody>
                    </table>
                    </div>",
      d$car_drive_time[i], (d$car_walk_time[i] + d$car_drive_time[i]), d$car_walk_time[i] , d$car_parking_cost[i], d$car_parking_cost[i],
      d$bike_ride_time[i], d$bike_ride_time[i],
      d$em_walk_time[i], (d$em_walk_time[i] + d$em_scoot_time[i] + d$em_ride_time[i]), d$em_scoot_time[i], d$em_ride_time[i], d$em_scoot_cost[i], (d$em_scoot_cost[i] + d$em_ticket_cost[i]), d$em_ticket_cost[i]),
      paste("sets/set",i , ".txt", sep = ""))
}