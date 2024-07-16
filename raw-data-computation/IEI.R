library(stringr)
library(textgRid)

# Read the TextGrid file

species <- "Red_Breasted_Nuthatch"

Potoo <- data.frame(Inter_Event_Interval = numeric(), Rhythm = numeric())

for (n in 1:5) {

textgrid_file <- paste("C:/Users/piette/Desktop/Inter_Event_Intervals/",species,"/i",n,"_1.TextGrid",sep="")

textgrid <- TextGrid(textgrid_file)

Call_Duration <- findIntervals(textgrid$Call)

duration <- Call_Duration$EndTime - Call_Duration$StartTime

Call_Duration <- cbind(Call_Duration,duration)

Mean <- mean(Call_Duration$duration)

Potoo <- rbind(Potoo,Mean)

} 


rhythm <- 1/Potoo

Potoo <- cbind (Potoo,rhythm)

colnames(Potoo) <- c("Inter_Event_Interval","Rhythm")



write.table (Potoo,  paste("C:/Users/piette/Desktop/Inter_Event_Intervals/",species,"/Rate.csv", sep = ""), sep=";", dec=",", col.names=TRUE, row.names=FALSE)


