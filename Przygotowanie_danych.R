
install.packages("ggplot2")
install.packages("dplyr")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("corrplot")  # Instalacja pakietu corrplot

library(dplyr)
library(ggplot2)
library(car)
library("VIM")
library(readxl)
library(FactoMineR)
library(factoextra)
library(outliers)
library(EnvStats)
library(corrplot)

dane<-read.csv("C:/Users/48690/Desktop/Przygotowanie/dane_p.csv",sep=";") #wczytanie danych
styl<-read.csv("C:/Users/48690/Desktop/Przygotowanie/styl.csv",sep=";") #wczytanie pliku ze stylami

#1. Określenie celu badania: Celem badania jest przygotowanie danych do stworzenia modeli regresji badającego tego jak
#dane zmienne dotyczące piwowarstwa zależą od innych zmiennych dotyczących piwowarstwa np. czy czas warzenia piwa wpływa na gęstość trunku.

#2. 
#a. Skala pomiarowa zmiennych: 
#, Size.L(ilorazowa), OG(ilorazowa), FG(ilorazowa), ABV(ilorazowa), IBU(ilościowa,ilorazowe),
#color(ilorazowa), boilsize(ilorazowa), Boiltime(interwałowa), BoilGravity(ilorazowa), efficiency(ilościowa, porządkowa),
#Mashthickness(ilorazowa), SugarScale(jakościowa, porządkowa), Brewmethod(jakościowa, nominalna wielodzielcza),
#Pitchrate(ilorazowa), PrimaryTemp(przedziałowa), PrimingMethod(jakościowa, nominalna wielodzielcza),
#PrimingAmount(ilorazowa). Zastrzec należy, że dla niektórych zmiennych ocena wymaga
#wiedzy odnośnie fizyki i piwowarstwa jak np. odnośnie gęstości, gdzie przyjęto, że istnieje 0 absolutne.
#Dla jednostki IBU uznano, że istnieje zero absolutne, gdyż to zawartości 1 miligrama izo-alfa kwasów pochodzących z chmielu w 1 litrze piwa

#b. Jednostki miary: size.L - litry, OG - gęstość, FG - gęstość, ABV - ilość alkoholu w %, IBU - jednostka goryczy,
#Color - ciemność koloru, ciężko uznać co jest miarą, boilsize - litry, BoilGravity - niuton na metr sześcienny prawdopdoobnie,
#Efficiency - nieidentyfikowalna, prawdopodobnie żadna, MashThickness - kwarty, PitchRate - liczba milionow komórek drożdży na mililitr/ na stopień Plato
#PrimaryTemp - celsjusze lub fahrenheity.

table(dane$SugarScale) #Zdecydowanie dominuje skala pomiarowa "Specific gravity"
ggplot(dane , aes(x=factor(BrewMethod), fill=factor(BrewMethod))) + 
  geom_bar() +
  theme(legend.position="none")
#Wykres wskazuje na to, że najczęściej stosowaną metodą jest ze wszystkimi zbożami, sporą popularnością charakteryzuje 
#się także warzenie w worku (BIAB)
ggplot(dane, aes(x = BoilSize, y = BoilTime)) +
  geom_point(color = "blue") +
  labs(title = "Wykres rozrzutu BoilSize vs BoilTime",
       x = "Boilsize",
       y = "BoilTime") +
  theme_minimal() #Wykres ten może nie być najlepszy z uwagi na to, że zmienna BoilTime jest bardzo skokowa i nie przyjmuje wielu wariantów cechy
#Jednak pokazuje to specyfikę zmiennej

plot(dane$PrimaryTemp, dane$BoilGravity) #Wykres rozrzutu dla zmiennej BoilGravity i PrimaryTemp
plot(dane$Size.L.,dane$BoilSize) #Zauważalna jest silna dodatnia, liniowa korelacja między zmiennymi
cor(dane$Size.L.,dane$BoilSize) #Zależność jest prawie funkcyjna
cor(dane$BoilTime,dane$BoilSize) #Pomiędzy tymi zmiennymi korelacja jest niska


ggplot(dane, aes(x=ABV)) + 
  geom_histogram(color="white", fill="darkgrey") #Wykres objętości alkoholu wskazuje, że najczęściej wynosi ona 5%

summary(dane$BoilTime) #Średni czas gotowania wyniósł 65,07 minut, a mediana wyniosła 60, co wskazuje na umiarkowaną asymetrię prawostronną.
#Taka sama wartość 1. kwartyla i 3. kwartyla mówi, że w zbiorze dominuj czas gotowania równy 60 minut.

summary(dane$BoilSize) #Średnia arytmetyczna dla płynu na początku wrzenia wynosi 49,73 litrów, a wartość środkowa 27,44 litry
#Rozstęp kwartylowy wynosi około 10 litrów.


dane$BoilSize_1<-recode(dane$BoilSize,"1:20='<20';21:30='21-30';31:100='31-100';else='>100'")
table(dane$BoilSize_1) #Stworzono przedziały dla zmiennej BoilSize. Sugerowano się statystykami opisowymi przy podziale szerokości przedziałów
#Dominuje przedział od 21 do 30 litrów.

summary(dane) #podstawowe statystyki opisowe dla wszystkich zmiennych
dane1<-as.data.frame(dane)
head(dane,10)
summary(dane$OG)
summary(dane$FG)
summary(dane$ABV)
summary(dane$IBU)


#Wizualizacja brakó danych
plot_missing<-aggr(dane1, col=c('darkgrey','tomato'),
                   numbers=TRUE, sortVars=TRUE,
                   labels=names(dane), cex.axis=0.6,
                   cex.lab=1.5,
                   gap=1, ylab=c('Braki',"Wzór braków"))


summary(aggr(dane1, plot=FALSE))
#Braki danych dominują przy zmiennej UserID, jednak nie ma sensu imputować danych dla tej zmiennej.
#Ponadto niektóre braki danych zostały zapisane jako NA, a niektóre jako N/A
#Dla większości wierszy lub kolumn występują jakiekolwiek braki danych

dane2 <- dane1 %>%
  mutate_all(~ ifelse(. == "N/A", NA, .)) #Zamiana danych N/A na NA
result <- dane2 %>%
  summarise_all(~ sum(is.na(.)))
result #Liczba braków danych dla każdej kolumny. Dla niektórych zmiennych imputacja nie ma sensu, ponieważ braki stanowią zdecydowaną większość.

# Liczba braków danych w każdej kolumnie
col_na <- colSums(is.na(dane2))


col_elements <- nrow(dane2)

# Udział braków danych w każdej kolumnie
col_missing_ratio <- col_na / col_elements

# Wyświetlenie wyniku
print(100*round(col_missing_ratio,4)) #Udziały braków dla zmiennych
dane2 <- dane2 %>% select(-PrimingMethod) #Usunięcie zmiennej PrimingMethod, udział braków to aż 91 %
dane2 <- dane2 %>% select(-PrimingAmount) #Usunięcie zmiennej PrimingAmount, udział braków to aż 93 %
dane2 <- dane2 %>% select(-PitchRate) #Usunięcie zmiennej PitchRate, udział braków to aż 53 %
dane2 <- dane2 %>% select(-MashThickness) #Usunięcie zmiennej PitchRate, udział braków to aż 40 %, wciąż za dużo, aby próbować imputować
dane2 <- subset(dane2, select = -PrimaryTemp) #usunięcie kolumny OG


dane2$BoilGravity<-as.numeric(dane2$BoilGravity)
summary(dane2$BoilGravity)
hist(dane2$BoilGravity)
boxplot(dane2$BoilGravity, main = "Boxplot") 
qqnorm(dane2$BoilGravity, main = "Normal Q-Q plot")  # wykres kwartyl-kwartyl


dane3<-dane2
dane3$BoilGravity<-as.numeric(dane3$BoilGravity)
dane3<-dane3%>%
  mutate(BoilGravity=if_else(is.na(BoilGravity), round(median(BoilGravity,na.rm = T),0), BoilGravity))
summary(dane3) #imputacja medianą dla zmiennej BoilGravity, ponieważ rozkład jest prawostronny i występują wartości skrajne.


boxplot(dane3$BoilGravity, main = "Boxplot") 
qqnorm(dane3$BoilGravity, main = "Normal Q-Q plot")  # wykres kwartyl-kwartyl
mean_boil=mean(dane3$BoilGravity) #Średnia dla zmiennej boilgravity
sd_boil=sd(dane3$BoilGravity) #odchylenie standardiwe
Tmin = mean_boil-(3*sd_boil)
Tmax = mean_boil+(3*sd_boil)
dane3$BoilGravity[which(dane3$BoilGravity < Tmin | dane3$BoilGravity > Tmax)] #wartości skrajne, jednak zastosowanie średniej może nie być najlepsze
med_Boil = median(dane3$BoilGravity)
abs_dev_Boil = abs(dane3$BoilGravity-med_Boil) 
mad = 1.4826 * median(abs_dev_Boil) #Odchylenie medianowe
Tmin = med_Boil-(3*mad) 
Tmax = med_Boil+(3*mad) 
dane3$BoilGravity[which(dane3$BoilGravity < Tmin | dane3$BoilGravity > Tmax)] #wartości skrajne metodą mediany
ks.test(dane3$BoilGravity, "pnorm", mean = mean(dane3$BoilGravity), sd = sd(dane3$BoilGravity))
#Istnieją silne dowody, aby stwierdzić, że rozkład zmiennej BoilGravity nie jest r. normalnego, więc nie jest zasadne
#czynienie testów statystycznych na wykrywanie wartości nietypowych. Zastosowano metodę odchylenia medianowego.
dane3 <- dane3[!(dane3$BoilGravity < Tmin | dane3$BoilGravity > Tmax), ]
summary(dane3$BoilGravity) #dane po wyrzuceniu wartości nietypowych
boxplot(dane3$BoilGravity)
View(dane3)



summary(dane3$OG)
plot(dane3$OG)
boxplot(dane3$OG) #Istnieją outliery dla tej zmiennej
ks.test(dane3$OG, "pnorm", mean = mean(dane3$OG), sd = sd(dane3$OG))
#Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy, że r. jest normalny, a więc
#wnioskować należy, że rozkład jest normalny
mean_OG=mean(dane3$OG) #Średnia dla zmiennej boilgravity
sd_OG=sd(dane3$OG) #odchylenie standardiwe
Tmin = mean_OG-(3*sd_OG)
Tmax = mean_OG+(3*sd_OG)
dane3$OG[which(dane3$OG < Tmin | dane3$OG > Tmax)]
dane3 <- dane3[!(dane3$OG < Tmin | dane3$OG > Tmax), ]

summary(dane3$FG)
boxplot(dane3$FG)
Q1_FG <- quantile(dane3$FG, 0.25)  # Pierwszy kwartyl dla zmiennej FG
Q3_FG <- quantile(dane3$FG, 0.75)  # Trzeci kwartyl dla zmiennej FG
IQR_FG <- Q3_FG - Q1_FG  # Interkwartylowy zakres dla zmiennej FG
lower_limit_FG <- Q1_FG - 1.5 * IQR_FG  # Dolna granica dla zmiennej FG
upper_limit_FG <- Q3_FG + 1.5 * IQR_FG  # Górna granica dla zmiennej FG
dane3 <- dane3[!(dane3$FG < lower_limit_FG | dane3$FG > upper_limit_FG), ]
summary(dane3$FG) #Imputacja metodą odchylenia kwartylowego


summary(dane3$ABV)
plot(dane3$ABV)
boxplot(dane3$ABV)
ks.test(dane3$ABV, "pnorm", mean = mean(dane3$ABV), sd = sd(dane3$ABV))
Q1_ABV <- quantile(dane3$ABV, 0.25)  # Pierwszy kwartyl dla zmiennej ABV
Q3_ABV <- quantile(dane3$ABV, 0.75)  # Trzeci kwartyl dla zmiennej ABV
IQR_ABV <- Q3_ABV - Q1_ABV  # Interkwartylowy zakres dla zmiennej ABV
lower_limit_ABV <- Q1_ABV - 1.5 * IQR_ABV  # Dolna granica dla zmiennej ABV
upper_limit_ABV <- Q3_ABV + 1.5 * IQR_ABV  # Górna granica dla zmiennej ABV
dane3 <- dane3[!(dane3$ABV < lower_limit_ABV | dane3$ABV > upper_limit_ABV), ]
#Usunięcie wartości skrajnych przy pomocy odchylenia międzykwartylowego

boxplot(dane3$Efficiency)

# Obliczenie interkwartylowego zakresu (IQR) dla zmiennej Efficiency
Q1_Efficiency <- quantile(dane3$Efficiency, 0.25)  # Pierwszy kwartyl dla zmiennej Efficiency
Q3_Efficiency <- quantile(dane3$Efficiency, 0.75)  # Trzeci kwartyl dla zmiennej Efficiency
IQR_Efficiency <- Q3_Efficiency - Q1_Efficiency  # Interkwartylowy zakres dla zmiennej Efficiency

# Określenie granic dla outlierów na podstawie IQR dla zmiennej Efficiency
lower_limit_Efficiency <- Q1_Efficiency - 1.5 * IQR_Efficiency  # Dolna granica dla zmiennej Efficiency
upper_limit_Efficiency <- Q3_Efficiency + 1.5 * IQR_Efficiency  # Górna granica dla zmiennej Efficiency

# Usunięcie outlierów na podstawie wyznaczonych granic dla zmiennej Efficiency
dane3 <- dane3[!(dane3$Efficiency < lower_limit_Efficiency | dane3$Efficiency > upper_limit_Efficiency), ]

# Sprawdzenie liczby usuniętych outlierów
removed_outliers_Efficiency <- sum(dane3$Efficiency < lower_limit_Efficiency | dane3$Efficiency > upper_limit_Efficiency)
cat("Liczba usuniętych outlierów dla zmiennej Efficiency:", removed_outliers_Efficiency, "\n")

# Sprawdzenie nowej liczby wierszy w ramce danych po usunięciu outlierów
cat("Liczba wierszy po usunięciu outlierów dla zmiennej Efficiency:", nrow(dane3), "\n")
boxplot(dane3$Efficiency)



# Obliczenie interkwartylowego zakresu (IQR) dla zmiennej BoilSize
Q1_BoilSize <- quantile(dane3$BoilSize, 0.25)  # Pierwszy kwartyl dla zmiennej BoilSize
Q3_BoilSize <- quantile(dane3$BoilSize, 0.75)  # Trzeci kwartyl dla zmiennej BoilSize
IQR_BoilSize <- Q3_BoilSize - Q1_BoilSize  # Interkwartylowy zakres dla zmiennej BoilSize

# Określenie granic dla outlierów na podstawie IQR dla zmiennej BoilSize
lower_limit_BoilSize <- Q1_BoilSize - 1.5 * IQR_BoilSize  # Dolna granica dla zmiennej BoilSize
upper_limit_BoilSize <- Q3_BoilSize + 1.5 * IQR_BoilSize  # Górna granica dla zmiennej BoilSize

# Usunięcie outlierów na podstawie wyznaczonych granic dla zmiennej BoilSize
dane3 <- dane3[!(dane3$BoilSize < lower_limit_BoilSize | dane3$BoilSize > upper_limit_BoilSize), ]

# Sprawdzenie liczby usuniętych outlierów
removed_outliers_BoilSize <- sum(dane3$BoilSize < lower_limit_BoilSize | dane3$BoilSize > upper_limit_BoilSize)
cat("Liczba usuniętych outlierów dla zmiennej BoilSize:", removed_outliers_BoilSize, "\n")

# Sprawdzenie nowej liczby wierszy w ramce danych po usunięciu outlierów
cat("Liczba wierszy po usunięciu outlierów dla zmiennej BoilSize:", nrow(dane3), "\n")


# Przeprowadzenie PCA
dane_PCA <- dane3[, c("BoilSize", "BoilTime", "ABV","Efficiency","OG","FG","BoilGravity")] #Zmienne wzięte pod uwagę do dalszej analizy
View(dane_PCA)
dane_PCA_scaled <- scale(dane_PCA)
pca_result <- prcomp(dane_PCA_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)

# Współczynniki głównych składowych
pca_result$rotation

# Wyniki transformacji
pca_transformed <- pca_result$x
head(pca_transformed)
install.packages("ggplot2")
library(ggplot2)

# Konwersja wyników PCA do ramki danych
pca_data <- data.frame(pca_transformed)

# Dodanie identyfikatorów i klasyfikacji outlierów (opcjonalnie)
pca_data$ID <- rownames(dane3)
pca_data$outlier <- ifelse(abs(pca_data$PC1) > 10 | abs(pca_data$PC2) > 8, "Outlier", "Not Outlier")

# Wizualizacja wyników PCA z ggplot2
ggplot(pca_data, aes(x = PC1, y = PC2, color = outlier, label = ID)) +
  geom_point(size = 2) +
  geom_text(aes(label = ifelse(outlier == "Outlier", as.character(ID), '')), hjust = 1, vjust = 1) +
  labs(title = "PCA - Wykrywanie outlierów", x = "Główna Składowa 1", y = "Główna Składowa 2") +
  scale_color_manual(values = c("Outlier" = "red", "Not Outlier" = "blue")) +
  theme_minimal()
#Przeprowadzono analizę PCA do wykrycia potencjalnych wartości skrajnych, widać, że niektóre obserwację są nietypowe, jednak nie jest ich wiele i nie są to skrajne odchylenia
dane_PCA[8518,]




dane_zbior <- dane3[, c( "BoilSize", "BoilTime", "ABV","Efficiency","OG","FG","BoilGravity")]
korelacje<-cor(dane_zbior)
korelacje
corrplot(korelacje, method = "color", type = "upper", 
         tl.col = "blue", tl.srt = 45, 
         addCoef.col = "blue", number.cex = 0.7) 
#Wykres korelacji wskazuje, że najwyższa korelacja jest pomiędzy zmienną OG, a ABV, wynosi ona ponad 0,96
#W związku z tym postanowiono usunąć zmienną OG, ponieważ merytorycznie przypomina zmienną FG (jest też korelacja na poziomie 0,75)

dane_zbior <- subset(dane_zbior, select = -OG) #usunięcie kolumny OG
View(dane_zbior)

dane_zbior <- dane3[, c("BeerID","Name","Style", "BoilSize", "BoilTime", "ABV","Efficiency","FG","BoilGravity")]

nrow(dane_zbior)
# Załadowanie pakietu dplyr
library(dplyr)

# Definicja funkcji do obliczenia wielkości próby
calculate_sample_size <- function(N, p = 0.5, E = 0.05, Z = 1.96) {
  n <- (N * Z^2 * p * (1 - p)) / (E^2 * (N - 1) + Z^2 * p * (1 - p))
  return(ceiling(n))  # Zaokrąglenie do najbliższej większej liczby całkowitej
}

# Załadowanie danych (przykładowa struktura danych)
# dane_zbior <- read.csv("sciezka_do_pliku.csv")

# Liczba obserwacji w oryginalnym zbiorze danych
N <- nrow(dane_zbior)

# Obliczenie wielkości próby
liczebnosc_probki <- calculate_sample_size(N, p = 0.3, E = 0.05, Z = 1.96)
liczebnosc_probki #p=0,5 przyjęty jako najbardziej "restrykcyjna
# Dobór losowej próby bez zwracania
probka <- sample_n(dane_zbior, size = liczebnosc_probki, replace = FALSE)

#Zastosowano losowanie proste bez zwracania, ponieważ jest to najbardziej uniwersalna opcja, która zapewnia rzeczywistą losowość próby
#będąc odporna na na przykład wzorce w danych (wyższość nad losowaniem systematycznym). Dobór nielosowy wymagałby wiedzy eksperckiej
#z zakresu piwowarstwa.

print(probka)
View(probka)

dane_gotowe<-probka
View(dane_gotowe) #Oto są gotowe dane, które można wykorzystać do budowy modeli regresji i poszukiwania zależności między zmiennymi
#Zawarte zostały zmienne zarówno, które można wykorzystać w ilościowej analizie, jak i te indetyfikujące daną obserwację.

















