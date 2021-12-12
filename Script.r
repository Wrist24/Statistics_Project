nba <- read.csv("nba.csv")
nba
attach(nba)
install.packages("foreign")
library(foreign)
install.packages("MASS")
library(MASS)
install.packages("e1071")
library(e1071)
Sredna_Tocka_Intervali <- function(x,from,to,by){
  x=cut(x,seq(from,to,by),include.lowest=T)
  vec=seq(from+by/2,to-by/2,by)
  names(vec)=levels(x)
  unname(vec[x])
}
Cestoti_Godini <- table(nba$Age)
Cestoti_Godini
Relativna_Frekvencija_Godini <- prop.table(Cestoti_Godini)
Relativna_Frekvencija_Godini
Kumulativna_Frekvencija_Godini <- cumsum(Cestoti_Godini)
Kumulativna_Frekvencija_Godini
Tabela_Godini <- cbind(Cestoti_Godini, Relativna_Frekvencija_Godini, Kumulativna_Frekvencija_Godini)
Tabela_Godini
Rang_Godini = nba$Age
range(Rang_Godini)
breaks_godini = seq(18, 43, by=5)
breaks_godini
Rang_Godini.cut = cut(Rang_Godini, breaks_godini, right = FALSE)
Rang_Godini_Cestoti = table(Rang_Godini.cut)
Rang_Godini_Cestoti
Tabela_Intervali_Godini <- cbind(Rang_Godini_Cestoti)
Tabela_Intervali_Godini
Sredina_Godini <- Sredna_Tocka_Intervali(breaks_godini, 18, 43, 5)
Sredina_Godini
Cestoti_Natprevari <- table(nba$G)
Cestoti_Natprevari
Relativna_Frekvencija_Natprevari <- prop.table(Cestoti_Natprevari)
Relativna_Frekvencija_Natprevari
Kumulativna_Frekvencija_Natprevari <- cumsum(Cestoti_Natprevari)
Kumulativna_Frekvencija_Natprevari
Tabela_Natprevari <- cbind(Cestoti_Natprevari, Relativna_Frekvencija_Natprevari, Kumulativna_Frekvencija_Natprevari)
Tabela_Natprevari
Rang_Natprevari = nba$G
range(Rang_Natprevari)
breaks_natprevari = seq(0, 90, by=9)
breaks_natprevari
Rang_Natprevari.cut = cut(Rang_Natprevari, breaks_natprevari, right = FALSE)
Rang_Natprevari_Cestoti = table(Rang_Natprevari.cut)
Rang_Natprevari_Cestoti
Tabela_Intervali_Natprevari <- cbind(Rang_Natprevari_Cestoti)
Tabela_Intervali_Natprevari
Sredina_Natprevari <- Sredna_Tocka_Intervali(breaks_natprevari, 0, 90, 9)
Sredina_Natprevari
barplot(Cestoti_Godini)
barplot(Relativna_Frekvencija_Godini)
barplot(Kumulativna_Frekvencija_Godini)
barplot(Rang_Godini_Cestoti)
barplot(Cestoti_Natprevari)
barplot(Relativna_Frekvencija_Natprevari)
barplot(Kumulativna_Frekvencija_Natprevari)
barplot(Rang_Natprevari_Cestoti)
Godini <- nba
Godini
Primerok <- Godini[sample(1:nrow(Godini), 60, replace = FALSE),]
stem(Primerok$G)
Vozrast <- Primerok[order(Primerok$Age),]
dotchart(Vozrast$G, labels = Vozrast$Age, cex = .7,
         main = "Dotchart for games played", 
         ylab = "Player's age")
mean(Primerok$Age, trim = 0, na.rm = FALSE)
mean(Primerok$G, trim = 0, na.rm = FALSE)
median(Primerok$Age, na.rm = FALSE)
median(Primerok$G, na.rm = FALSE)
Moda <- function(v) {
  unikat <- unique(v)
  unikat[which.max(tabulate(match(v, unikat)))]
}
Moda(Primerok$Age)
Moda(Primerok$G)
quantile(Primerok$Age)
quantile(Primerok$G)
max(Primerok$Age)-min(Primerok$Age)
max(Primerok$G)-min(Primerok$G)
IQR(Primerok$Age)
IQR(Primerok$G)
var(Primerok$Age)
var(Primerok$G)
Standard_Devijacija <- sd(Primerok$Age)
Standard_Devijacija
sd(Primerok$G)
cor(Primerok$Age, Primerok$G)
Interval_Doverba <- function(vektor,sigma,nivo_doverba = 0.95) {
  Golemina_Primerok = length(vektor);
  Prosek = mean(vektor);
  alfa = 1-nivo_doverba;
  z = qnorm(1-alfa/2)
  SE = sigma/sqrt(Golemina_Primerok)
  Prosek+c(-z*SE,z*SE)
}
Interval_Doverba(Primerok$Age, Standard_Devijacija)
# Testing a hypothesis
# е0 = Average age of the given sample of players in 24 years old with a
#      confidence interval of 95%
# ер = The given sample of players is on average older than 24 years
mu = 24;
xbar = mean(Primerok$Age);
alternative = "greater";
alfa = 0.05;
n = length(Primerok$Age);
Z = (xbar-mu)/(Standard_Devijacija/sqrt(n))
if(alternative=="greater"){
  Za = -qnorm(1-alfa);
  print(Za);
  print(Z);
  if(Z<=Za || Z>=-Za){
    print("е0 is declined, ер is accepted: The average of the sample is bigger than 24.")
  }
  else {
    print("е0 is accepted: The average of the sample is 24.");
  }
}
shapiro.test(Primerok$Age)
Tabela_Kontingencija = table(Primerok$Age, Primerok$G)
Tabela_Kontingencija
chisq.test(Tabela_Kontingencija)
chisq.test(rbind(Primerok$Age, Primerok$G))
par(mfrow = c(1, 1))
model <- lm(Primerok$Age~Primerok$G)
round(coefficients(model), 3)
plot(Primerok$Age, Primerok$G)
abline(reg = model)
z <- coefficients(model)
summary(model)
lm(formula = Primerok$Age~Primerok$G)
z
