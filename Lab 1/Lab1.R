#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###### Regression Modelling Lab 1
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Example 1
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Reading in the data

phys <- read.csv("phys1.csv")

### Exploring the data

names(phys)

View(phys)

### Creating a scatterplot

library(ggplot2)

ggplot(phys, aes(phys$Weight, phys$Power1)) + geom_point()

### Formatting the plot

## Adding appropriate axis labels

ggplot(phys, aes(phys$Weight, phys$Power1)) + geom_point() + 
  labs(x = "Weight (kgs)") +
  labs(y = "Power Output (Watts)") +
  labs(title = "Person's weight (kgs) vs power output (Watts) in the stair test")

## Changing the symbol type, size and colour

ggplot(phys, aes(phys$Weight, phys$Power1)) + 
  geom_point(colour = 'Blue', size = 2, shape = 2) + 
  labs(x = "Weight (kgs)") +
  labs(y = "Power Output (Watts)") +
  labs(title = "Person's weight (kgs) vs power output (Watts) in the stair test")

## Adding a line to the plot

ggplot(phys, aes(phys$Weight, phys$Power1)) + 
  geom_point(colour = 'Blue', size = 2, shape = 2) + 
  labs(x = "Weight (kgs)") +
  labs(y = "Power Output (Watts)") +
  labs(title = "Person's weight (kgs) vs power output (Watts) in the stair test") +
  geom_segment(aes(x = 45, y = 700, xend = 90, yend=1650))
  

## Labelling the points for gender

# Create a character variable for gender

Gender <- as.character(phys$Gender)

ggplot(data=phys, aes(Weight, Power1, color = Gender, shape = Gender, label = Gender)) + 
  geom_point() + 
  geom_text(aes(label = substr(Gender, 1, 1)), hjust = 0, nudge_x = 0.6,
            show.legend = FALSE) + 
  labs(x = "Weight (kgs)") +
  labs(y = "Power Output (Watts)") +
  labs(title = "Person's weight (kgs) vs power output (Watts) in the stair test")

# Creating a numerical variable for gender

Gender1 <- as.numeric(phys$Gender)

plot(phys$Power1~phys$Weight, xlab="Weight (kgs)", ylab="Power Output (Watts)",pch=Gender1, col=Gender1)
legend(75, 1000, legend=c("Female","Male"), pch=c(1,2), col=c(1,2))


##### Inference based on correlation

physM <- subset(phys, phys$Gender=="Male")
physF <- subset(phys, phys$Gender=="Female")

phys.male <- phys[Gender == "Male", ]
phys.female <- phys[Gender == "Female", ] 

cor.test(physM$Power1, physM$Weight)
cor.test(physF$Power1, physF$Weight)

cor.test(phys.male$Weight, phys.male$Power1)
cor.test(phys.female$Weight, phys.female$Power1)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
########### Example 2 the hubble data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hubble <- read.csv("hubble.csv")
names(hubble)

## Producing a scatterplot

plot(Velocity~Distance, data=hubble)

## Regression analysis

lm(Velocity~Distance, data=hubble)

model1 <- lm(Velocity~Distance, data=hubble)
plot(Velocity~Distance, data=hubble)
lines(fitted(model1)~Distance, data=hubble)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############## Example 3. Books data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

books <- read.csv("books.csv")
names(books)

plot(Number.of.Books~Year, data=books)

lm(Number.of.Books~Year+I(Year^2), data=books)

model2 <- lm(Number.of.Books~Year+I(Year^2), data=books)

plot(Number.of.Books~Year, data=books)
lines(fitted(model2))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
########### Example 4 cheese data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cheese <- read.csv("cheese.csv")
names(cheese)

plot(Taste~H2S, data=cheese)
plot(Taste~log(H2S), data=cheese)

model3 <- lm(Taste~log(H2S), data=cheese)

