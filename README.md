Opis projektu
Celem tego projektu jest przeprowadzenie analizy danych dotyczących procesu piwowarskiego. Analizowane dane pochodzą z różnych zmiennych, takich jak czas gotowania, gęstość, zawartość alkoholu, gorycz, kolor piwa, efektywność procesu warzenia i wiele innych. Celem jest przygotowanie danych do budowy modeli regresyjnych oraz zrozumienie, jak różne zmienne wpływają na proces piwowarski, w tym analiza zależności między zmiennymi, identyfikacja braków danych, a także wykrywanie wartości odstających.

Wymagania
Aby uruchomić projekt, należy zainstalować następujące pakiety R:

ggplot2
dplyr
FactoMineR
factoextra
corrplot
VIM
car
EnvStats
outliers
readxl
Można to zrobić za pomocą następującego polecenia:

r
Kopiuj
Edytuj
install.packages(c("ggplot2", "dplyr", "FactoMineR", "factoextra", "corrplot", "VIM", "car", "EnvStats", "outliers", "readxl"))
Przygotowanie danych
Wczytywanie danych
Projekt zaczyna się od wczytania dwóch plików CSV: dane_p.csv (zawierającego dane o piwowarstwie) oraz styl.csv (zawierającego informacje o stylach piwa).
r
Kopiuj
Edytuj
dane <- read.csv("ścieżka_do_pliku/dane_p.csv", sep = ";")
styl <- read.csv("ścieżka_do_pliku/styl.csv", sep = ";")
Skala pomiarowa zmiennych
Dla każdej zmiennej określono odpowiednią skalę pomiarową, np. zmienne takie jak Size.L (ilość w litrach), OG (gęstość piwa), ABV (zawartość alkoholu), IBU (goryczka) są zmiennymi ilorazowymi.

Podstawowe statystyki opisowe
Dla każdej z analizowanych zmiennych obliczono podstawowe statystyki opisowe (średnia, mediana, kwartyle). Na podstawie tych wyników dokonano analizy skali zmiennych oraz wykresów.

r
Kopiuj
Edytuj
summary(dane$BoilSize)
Wizualizacja danych
Wykorzystano wykresy takich jak histogramy, wykresy rozrzutu oraz wykresy boxplot, które pomagają w zrozumieniu rozkładów zmiennych oraz zależności między nimi. Na przykład:
r
Kopiuj
Edytuj
ggplot(dane, aes(x = ABV)) + 
  geom_histogram(color = "white", fill = "darkgrey")
Zidentyfikowanie braków danych
Przeprowadzono identyfikację braków danych oraz usunięcie zmiennych, które miały wysoki procent brakujących wartości. Zastosowano techniki imputacji dla niektórych zmiennych, takich jak zamiana wartości "N/A" na NA oraz imputacja medianą.
r
Kopiuj
Edytuj
dane2 <- dane1 %>% mutate_all(~ ifelse(. == "N/A", NA, .))
Wykrywanie wartości odstających
Zidentyfikowano wartości odstające przy użyciu testu normalności (np. test Kolmogorova-Smirnova) oraz różnych metod wykrywania wartości nietypowych, takich jak odchylenie standardowe oraz odchylenie medianowe.
r
Kopiuj
Edytuj
ks.test(dane3$BoilGravity, "pnorm", mean = mean(dane3$BoilGravity), sd = sd(dane3$BoilGravity))
Podsumowanie analizy
Opis zmiennych
Każda zmienna została opisana pod względem skali pomiarowej, jednostki miary oraz ewentualnych założeń, które zostały przyjęte (np. zero absolutne dla IBU).

Wizualizacje
Stworzono wykresy, które pomagają zobrazować rozkład danych i relacje między zmiennymi, np. wykresy rozrzutu dla BoilSize vs BoilTime i korelacje między zmiennymi.

Imputacja danych
Zostały przeprowadzone techniki imputacji (np. imputacja medianą) oraz usunięcie zmiennych o wysokim udziale brakujących danych. Zmienne o dużej liczbie braków zostały usunięte, aby nie wpływały na dalsze analizy.

Przygotowanie danych do modeli regresyjnych
Po usunięciu wartości odstających, imputacji danych i dokonaniu odpowiednich przekształceń danych, dane zostały przygotowane do dalszej analizy i budowy modeli regresyjnych.

Wykorzystanie wyników
Po przygotowaniu danych, celem jest zbudowanie modeli regresyjnych, które będą analizować zależności pomiędzy zmiennymi procesu piwowarskiego. Dalsza analiza może obejmować np. regresję liniową, analizę głównych składowych (PCA) czy też inne metody statystyczne.

Struktura katalogów
dane_p.csv — dane główne, zawierające informacje o procesie piwowarskim
styl.csv — dane o stylach piwa
Projekt_Analiza_Piwowarska.R — skrypt R z pełną analizą
Licencja
Projekt jest objęty licencją MIT. Można go dowolnie używać, modyfikować i rozpowszechniać, pod warunkiem zachowania informacji o autorze.

