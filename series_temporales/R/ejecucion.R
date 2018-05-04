# Tidy data ----

data <- read_excel("Data/data.xlsx", sheet = "Hoja1")


year <- rep(2006:2017, each = 4)
data <- mutate(data, Year = year)
dat  <- unite(dat,"Fecha",c("Year", "Quarter"),sep=".")
View(a)




a=t(dat)

data

colnames(a)=select(dat,Fecha)
