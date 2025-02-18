# Analiza Danych Piwowarskich

## Opis projektu
Projekt ma na celu przeanalizowanie danych związanych z procesem piwowarskim. Skupiamy się na zmiennych takich jak czas gotowania, gęstość, zawartość alkoholu, gorycz, kolor piwa i efektywność procesu. Celem jest przygotowanie danych pod modele regresyjne oraz zrozumienie, jak poszczególne zmienne wpływają na proces warzenia piwa.

## Wymagania
Do uruchomienia projektu potrzebujesz kilku pakietów R: ggplot2 (Tworzenie wykresów), dplyr (Manipulacja danymi), FactoMineR (Analiza wielowymiarowa), factoextra (Wizualizacja wyników analizy PCA), corrplot (Wizualizacja macierzy korelacji), VIM (Analiza braków danych), car (Analiza statystyczna), EnvStats (Statystyki opisowe i testy), outliers (Detekcja wartości nietypowych), readxl (Wczytywanie danych z plików Excel). Aby zainstalować wszystkie wymagane pakiety, uruchom:

install.packages(c("ggplot2", "dplyr", "FactoMineR", "factoextra", "corrplot", "VIM", "car", "EnvStats", "outliers", "readxl"))

## Przygotowanie danych
1. **Wczytywanie danych**: Na początku wczytujemy dwa pliki CSV: dane_p.csv (z danymi o piwowarstwie) oraz styl.csv (z informacjami o stylach piwa). 

dane <- read.csv("ścieżka_do_pliku/dane_p.csv", sep = ";")
styl <- read.csv("ścieżka_do_pliku/styl.csv", sep = ";")

2. **Skala pomiarowa zmiennych**: Każda zmienna została przypisana do odpowiedniej skali pomiarowej. Przykładowo zmienne jak Size.L (ilość w litrach), OG (gęstość piwa), ABV (zawartość alkoholu) oraz IBU (goryczka) są zmiennymi ilorazowymi.

3. **Statystyki opisowe**: Dla każdej zmiennej obliczamy podstawowe statystyki (średnia, mediana, kwartyle). Dzięki temu lepiej rozumiemy, jak rozkładają się nasze dane.


4. **Wizualizacja danych**: Wykorzystujemy wykresy takie jak histogramy, wykresy rozrzutu oraz boxploty, by lepiej zobrazować rozkład danych i zależności między zmiennymi. Na przykład:



5. **Braki danych**: Identyfikujemy brakujące wartości i usuwamy zmienne z dużą ilością braków. Dla niektórych zmiennych stosujemy imputację, np. zastępujemy wartości "N/A" na NA.


6. **Wartości nietypowe**: Sprawdzamy, czy w danych występują wartości nietypowe. Używamy testów normalności, takich jak test Kolmogorova-Smirnova, i różnych metod detekcji nietypowych wartości.


## Podsumowanie analizy
1. **Opis zmiennych**: Każda zmienna została opisana pod kątem skali pomiarowej, jednostki miary oraz ewentualnych założeń.

2. **Wizualizacje**: Stworzyliśmy wykresy, które pokazują rozkład danych i korelacje między zmiennymi, np. wykresy rozrzutu BoilSize vs BoilTime.

3. **Imputacja danych**: Dzięki imputacji oraz usunięciu zmiennych z brakującymi danymi udało się poprawić jakość danych.

4. **Przygotowanie danych do modeli**: Po oczyszczeniu danych i wykryciu wartości nietypowych, dane zostały przygotowane do analizy i budowy modeli regresyjnych.

## Struktura katalogów
- dane_p.csv – Dane dotyczące procesu piwowarskiego
- styl.csv – Dane o stylach piwa
- Przygotowanie_danych.R – Skrypt R do analizy danych

## Licencja
Projekt jest objęty licencją MIT. Możesz dowolnie używać, modyfikować i rozpowszechniać ten kod, pod warunkiem zachowania informacji o autorze.
