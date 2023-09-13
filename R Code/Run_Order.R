Liquid.Contamination <- c("Yes", "No")
Liquid.Viscocity <- c("Water", "Honey", "Oil")
Press.Area <- c("28.3 mm", "113.1 mm", "201.1 mm")
Tube.Length <- c("150 mm", "900 mm")

Design <- expand.grid(Liquid.Contamination, Liquid.Viscocity, Press.Area, Tube.Length)
names(Design) <- c("Liquid Contamination","Liquid Viscocity","Press Area", "Tube Length")

Design <- rbind(Design,Design)

Design$RunOrder <- sample(1:nrow(Design),replace=FALSE)

head(Design)