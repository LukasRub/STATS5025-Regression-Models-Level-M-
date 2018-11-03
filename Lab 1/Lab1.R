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
  labs(y = "Power Output (Watts)")

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

# Creating a numerical variable for gender

Gender1 <- as.numeric(phys$Gender)

# Plotting

plot(phys$Power1~phys$Weight, xlab="Weight (kgs)", ylab="Power Output (Watts)",
     pch=Gender1, col=Gender1)
legend(75, 1000, legend=c("Female","Male"), pch=c(1,2), col=c(1,2))

ggplot(data=phys, aes(Weight, Power1, color = Gender, shape = Gender, label = Gender)) + 
  geom_point() + 
  geom_text(aes(label = substr(Gender, 1, 1)), hjust = 0, nudge_x = 0.6,
            show.legend = FALSE) + 
  labs(x = "Weight (kgs)") +
  labs(y = "Power Output (Watts)")

##### Inference based on correlation

phys.male <- phys[Gender == "Male", ]
phys.female <- phys[Gender == "Female", ] 

cor.test(phys.male$Weight, phys.male$Power1)
cor.test(phys.female$Weight, phys.female$Power1)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
########### Example 2 the hubble data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hubble <- read.csv("hubble.csv")
names(hubble)

## Producing a scatterplot

plot(Velocity~Distance, data=hubble)

p <- ggplot(hubble, aes(Distance, Velocity))
p <- p + geom_point()

p

## Regression analysis
model1 <- lm(Velocity~Distance, data=hubble)

plot(Velocity~Distance, data=hubble)
#lines(fitted(model1)~Distance, data=hubble)
#abline(coef = model1$coefficients)
abline(reg = model1, col = "Red")
abline(reg = model1.noint, col = "Blue", lty=2)


p <- ggplot(hubble, aes(Distance, Velocity))
p <- p + geom_point()
#p <- p + geom_smooth(method = 'lm', se = FALSE)

coeffs <- model1$coefficients

p <- p + geom_abline(intercept = coeffs["(Intercept)"], 
                         slope = coeffs["Distance"])

p

model1.noint <- lm(Velocity ~ Distance - 1, data = hubble)
model1.noint
slope <- model1.noint$coefficients

p <- ggplot(hubble, aes(Distance, Velocity)) + 
  geom_point() +
  geom_abline(slope = slope, 
              colour = "blue", lty = 2) +
  geom_abline(intercept = coeffs["(Intercept)"], 
              slope = coeffs["Distance"],
              colour = "red")
p
      


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############## Example 3. Books data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

books <- read.csv("books.csv")
names(books)
View(books)

plot(Number.of.Books~Year, data=books)

# Lukas code

books.lnxy <- books
books.lnxy$Year <- books.lnxy$Year + 1
View(books.lnxy)

p <- ggplot(data = books, aes(Year, Number.of.Books)) +
     geom_point()

p <- ggplot(data = books, aes(Year, log(Number.of.Books))) +
     geom_point()

p <- ggplot(data = books.lnxy, aes(log(Year), log(Number.of.Books))) +
     geom_point()

p

lm(Number.of.Books~Year+I(Year^2), data=books)
lm(log(Number.of.Books) ~ Year, data = books)
lm(log(Number.of.Books) ~ log(Year), data = books.lnxy)
nls(Number.of.Books ~ a * Year ^ b, data = books.lnxy, start = list(a=1, b=1))

model2 <- lm(Number.of.Books ~ Year + I(Year^2), data = books)
model2.expo.y <- lm(log(Number.of.Books) ~ Year, data = books)
model2.expo.xy <- lm(log(Number.of.Books) ~ log(Year + 1), data = books)
model2.expo.xy.nl <- nls(Number.of.Books ~ a * Year ^ b, data = books.lnxy, 
                         start = list(a=1, b=1))


summary(model2)
summary(model2.expo.y)
summary(model2.expo.xy)
summary(model2.expo.xy.nl)

predictions <- cbind(books, 
                     quadratic = predict(model2), 
                     exponential = predict(model2.expo.y),
                     exponential2 = predict(model2.expo.xy),
                     exponential3 = predict(model2.expo.xy.nl))

#model2.coeffs <- model2.expo.xy$coefficients

#predictions$exponential2 <- (model2.coeffs[1] * (predictions$Year+1) ^ 
#  model2.coeffs[2])

View(predictions)

p <- ggplot(data = predictions, aes(Year, Number.of.Books))
p <- p + geom_point()
p <- p + geom_line(aes(y = quadratic), colour="red")
p <- p + geom_line(aes(y = exp(exponential)), colour="blue")
p <- p + geom_line(aes(y = exp(exponential2)), colour = "green")

p

plot(Number.of.Books~Year, data=books)
lines(fitted(model2))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
########### Example 4 cheese data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cheese <- read.csv("cheese.csv")
names(cheese)

CheeseScatter <- function(aes) {
  p <- ggplot(data = cheese, aes)
  p <- p + geom_point()
  return(p)
}

p <- CheeseScatter(aes(Lactic.Acid, Taste))

p <- CheeseScatter(aes(H2S, Taste))

p <- CheeseScatter(aes(log(Lactic.Acid), Taste))

p <- CheeseScatter(aes(log(H2S), Taste))

p

model3.lactic.acid <- lm(Taste ~ Lactic.Acid, data = cheese)
model3.log.lactic.acid <- lm(Taste ~ log(Lactic.Acid), data = cheese)
model3.log.h2s <- lm(Taste ~ log(H2S), data = cheese)

summary(model3.lactic.acid)
summary(model3.log.lactic.acid)
summary(model3.log.h2s)

cheese.pred <- cbind.data.frame(
  Case               = cheese$Case,
  Taste              = cheese$Taste,
  Lactic.Acid        = cheese$Lactic.Acid,
  H2S                = cheese$H2S,
  pred.bo.lactic     = predict(model3.lactic.acid),
  pred.bo.log.lactic = predict(model3.log.lactic.acid),
  pred.bo.log.h2s    = predict(model3.log.h2s))

typeof(cheese.pred)
View(cheese.pred)

p <- ggplot(data = cheese.pred, aes(Lactic.Acid, Taste))
p <- p + geom_point()
p <- p + geom_line(aes(y = pred.bo.log.lactic), colour='red', 
                   lty=2, alpha=0.7, show.legend = TRUE, label = "log(Lactic Acid")
p <- p + geom_line(aes(y = pred.bo.lactic), colour='blue', 
                   lty=2, alpha=0.7, show.legend = TRUE, label = "Lactic Acid")
p 


p <- ggplot(data = cheese.pred, aes(log(H2S), Taste))
p <- p + geom_point()
p <- p + geom_line(aes(y = pred.bo.log.h2s), colour='green',
                   lty=2, alpha=0.7, show.legend = TRUE)
p

p <- ggplot(data = cheese.pred, aes(H2S, Taste))
p <- p + geom_point()
p <- p + geom_line(aes(y = pred.bo.log.h2s), colour='green',
                   lty=2, alpha=0.7, show.legend = TRUE)
p

cor.test(cheese$Taste, cheese$Lactic.Acid)
cor.test(cheese$Taste, log(cheese$Lactic.Acid))
cor.test(cheese$Taste, log(cheese$H2S))
