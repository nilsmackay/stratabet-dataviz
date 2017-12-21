library(dplyr)
mydata <- read.csv("China Super League 2017 Chances.csv", stringsAsFactors = FALSE)

mydata      = mydata[mydata$icon != "owngoal",]
mydata$goal = ifelse(mydata$icon == "goal", 1, 0)

mydata$primaryLocation_x = (as.numeric(mydata$primaryLocation_x)/272)*68
mydata$location_x        = (as.numeric(mydata$location_x)/272)*68
mydata$primaryLocation_y = (as.numeric(mydata$primaryLocation_y)/420)*105
mydata$location_y        = (as.numeric(mydata$location_y)/420)*105

foot <- mydata %>% group_by(player) %>% summarize(right = sum(bodyPart == "Right"),
                                                  left = sum(bodyPart == "Left"),
                                                  n = right + left,
                                                  dif = abs(right-left))

foot$preference <- ifelse(foot$n < 5 | foot$dif < 4, 0, ifelse(foot$right > foot$left, 1, -1))

corners = mydata[mydata$type == "Direct Corner" | mydata$primaryType == "Corner",]
corners$primaryLocation_x = ifelse(corners$primaryLocation_x < 0, -34, 34)
corners$primaryLocation_y = 0.25
corners = corners[abs(corners$location_x) < 20,]

corners$side = ifelse(corners$primaryLocation_x <0, 1, 0)
corners$curve = 0

for(i in 1:nrow(corners)) {
  temp = foot[foot$player == corners[i,]$primaryPlayer,]$preference
  if(length(temp) > 0) {
    corners[i,]$curve = temp
  }
}

fieldlines = data.frame(x = c(-34.1, -34, 34, -20.25, 20.25, -20.35, -9.25, 9.25, -9.35, 0),
                        y = c(0, 0, 0, 0, 0, 16.5, 0, 0, 5.5, 11),
                        xend = c(34.1, -34, 34, -20.25, 20.25, 20.35, -9.25, 9.25, 9.35, 0),
                        yend = c(0, 105, 105, 16.5, 16.5, 16.5, 5.5, 5.5, 5.5, 11.5))


team_name = "Shandong Luneng"

library(ggplot2);library(RColorBrewer);library(dplyr)
windowsFonts(Lato=windowsFont("Lato"));
palette <- brewer.pal("Greys", n=9)

#side = 0 for corners from the right, side = 1 for corners from the left
side = 0
ggplot(corners[corners$team == team_name & corners$side == side,]) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend), data = fieldlines) +
  geom_curve(aes(x=-7.3124, y = 16.5, xend = 7.3124, yend=16.5), curvature = -0.5) +
  geom_curve(data=corners[corners$side == side & corners$curve == 1,], aes(x=primaryLocation_x, y=primaryLocation_y, xend = location_x, yend=location_y), 
             alpha = 0.03, curvature = 0.1, colour = palette[6]) +
  geom_curve(data=corners[corners$side == side & corners$curve == 0,], aes(x=primaryLocation_x, y=primaryLocation_y, xend = location_x, yend=location_y), 
             alpha = 0.03, curvature = -0.1, colour = palette[6]) +
  geom_point(data=corners[corners$side == side,], aes(x=location_x, y=location_y), 
             alpha = 0.025, colour= palette[6], stroke = 0, size = 15) +
  geom_curve(data=corners[corners$side == side & corners$curve == 1 & corners$team == team_name,], 
             aes(x=primaryLocation_x, y=primaryLocation_y, xend = location_x, yend=location_y), 
             curvature = 0.1, alpha = 0.5, colour = "steelblue", size = 0.5) +
  geom_curve(data=corners[corners$side == side & corners$curve == 0 & corners$team == team_name,], 
             aes(x=primaryLocation_x, y=primaryLocation_y, xend = location_x, yend=location_y), 
             curvature = -0.1, alpha = 0.5, colour = "steelblue", size = 0.5) +
  geom_point(data=corners[corners$team == team_name & corners$side == side & corners$goal == 1,],
             aes(x=location_x, y= location_y), shape = 21, size = 2.7 ,stroke = 0.8,  fill = "white", colour="steelblue") + 
  geom_point(data=corners[corners$team == team_name & corners$side == side & corners$goal == 0,],
             aes(x=location_x, y= location_y), shape = 21, size = 2.7, stroke = 0.2, fill = "steelblue", colour="white") +
  scale_fill_manual(values = c("steelblue", "white"), guide = FALSE) + 
  scale_colour_manual(values = c("NA", "steelblue"), guide = FALSE) +
  coord_cartesian(xlim=c(-35,35), ylim= c(0,30)) + 
  theme(
    text = element_text(family = "Lato", size = 12),
    plot.background = element_rect(fill = "#f8f8f7", colour = "#f8f8f7"),
    panel.background = element_rect(fill = "#f8f8f7"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = "Lato", colour = "white"),
    strip.background = element_rect(fill = "#5ca4a9") 
  )
        
        
