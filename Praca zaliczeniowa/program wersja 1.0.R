#' ---
#' title: "Analiza częstości słów, oraz modelowanie tematów LDA stenogramów sejmowych"
#' author: "Szymon Dąborwski, Michał Zakrzewski, Mateusz Kurzeja "
#' date:   "04.06.2026 "
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable      # Wygląd (bootstrap, cerulean, darkly, journal, lumen, paper, readable, sandstone, simplex, spacelab, united, yeti)
#'     highlight: kate      # Kolorowanie składni (haddock, kate, espresso, breezedark)
#'     toc: true            # Spis treści
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show    
#'     number_sections: false # Numeruje nagłówki (lepsza nawigacja)
#' ---


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)


#----------PROGRAM WERSJA 1.0------------------------

#' # 0. Pakiety i dane wejściowe
# 0. Pakiety i dane wejściowe ----
##Wymagane pakiety ----

#install.packages(c("pdftools", "tm", "stringr", "tidyverse", "dplyr", "stopwords", "tidytext", "topicmodels", "ggplot2"))

library(pdftools)
library(tm)
library(stringr)
library(tidyverse)
library(dplyr)
library(wordcloud)
library(stopwords)
library(tidytext)
library(topicmodels) 
library(ggplot2)


## Adres do pliku ze stenogramem ----
sciezka_do_pdf = "25_a_ksiazka_bis.pdf"

#Numer strony z początkiem tekstu
strona_stenogram = 5 #Jako backup, program powinien wykryć początek stenogramu

#' 1. Odczytanie tekstu z PDF-u, przetworzenie tekstu 
# 1. Odczytanie tekstu z PDF-u, przetworzenie tekstu -----

#Wczytanie danych z PDF (zwraca listę z dokładnymi pozycjami słów dla każdej strony)
dane_z_pdf = pdf_data(sciezka_do_pdf)

#' ## 1.1. Funkcja, która prawidłowo scala słowa dla pojedynczej strony
## 1.1. Funkcja, która prawidłowo scala słowa dla pojedynczej strony----

#Stenogramy sejmowe mają formę dwukolumnowną. Dlatego by otrzymać ciągły tekst wykorzystamy tą funcje
przetworz_strone_dwukolumnowa = function(strona_df) {
  # Jeśli strona jest pusta, zwróć pusty ciąg
  if(nrow(strona_df) == 0) return("")
  
  # Ustalenie połowy strony na osi X
  srodek_x = max(strona_df$x) / 2
  
  # Zaokrąglenie osi Y pomaga wyrównać słowa w tej samej linijce
  strona_df = strona_df %>%
    mutate(y_round = round(y / 5) * 5)
  
  # Odseparowanie lewej kolumny i posortowanie słów od góry do dołu, od lewej do prawej
  lewa_kolumna = strona_df %>% 
    filter(x < srodek_x) %>% 
    arrange(y_round, x)
  
  # Odseparowanie prawej kolumny i analogiczne posortowanie
  prawa_kolumna = strona_df %>% 
    filter(x >= srodek_x) %>% 
    arrange(y_round, x)
  
  # Połączenie słów w ciągły tekst
  tekst_lewa = paste(lewa_kolumna$text, collapse = " ")
  tekst_prawa = paste(prawa_kolumna$text, collapse = " ")
  
  # Połączenie obu kolumn ze spacją w środku
  paste(tekst_lewa, tekst_prawa, sep = " ")
}

# Zastosowanie funkcji do wszystkich stron w dokumencie
tekst_strony_poprawiony = sapply(dane_z_pdf, przetworz_strone_dwukolumnowa)


# Utworzenie ciągłego tekstu
caly_tekst = paste(tekst_strony_poprawiony, collapse = "\n")

#' ## 1.2. Usunięcie spisu treści 
## 1.2. Usunięcie spisu treści ----
#Treść właściwa stenogramu rozpoczyna się od : (Początek.....), albo (Wzowienie.....)
#Dzielimy teskt po pierwszym wystąpieniu tych słów
podzial_tekstu = strsplit(caly_tekst, "(?=\\((Początek |Wznowienie ))", perl = TRUE)[[1]]

#Sprawdzamy, czy udało się znaleźć punkt startowy
if (length(podzial_tekstu) > 1) {
  # Bierzemy wszystko od odnalezionego znacznika do końca tekstu
  czysty_tekst_bez_spisu <- paste(podzial_tekstu[2:length(podzial_tekstu)], collapse = "")
} else {
  #Jak szukanie początku stenogramu zawiedzie, wykorzystujemy backup
  czysty_tekst_bez_spisu = paste(caly_tekst[strona_stenogram:length(podzial_tekstu), collapse = ""])
}

#' ## 1.3. Ponowne tekstu przekształcenie na oddzielne linie
## 1.3 Ponowne tekstu przekształcenie na oddzielne linie----
linie_tekstu = unlist(strsplit(czysty_tekst_bez_spisu, "\n"))

print(linie_tekstu[1])

#' # 2. Uworzenie korpusu, tokenizacja i oczyszczanie 
# 2. Uworzenie korpusu, tokenizacja i oczyszczanie ----

#' ## 2.1. Usunięcie niepotrzebnych słów
## 2.1. Usunięcie niepotrzebnych słów----
#Przygotowanie słów do usunięcia
stopwords_pl = stopwords(language = "pl", source = "stopwords-iso") #Polskie stop words
#Wektror ze słowami do usunięcia, które mają niską wartość analityczną
custom_stopwords = c("panie", "pan", "pani", "marszałku", 
                    "wysoka", "izbo", "posłowie", "poseł", 
                    "bardzo", "proszę", "dziękuję",
                     "oklaski", "wicemarszałek", "r", "ustawy")
#Uworzenie ramki danych z niepotrzebnymi słowami
df_stopwords_pl = data.frame(slowo = stopwords_pl, stringsAsFactors = FALSE)
df_stopwords_custom = data.frame(slowo = custom_stopwords, stringsAsFactors = FALSE)
df_stopwords = rbind(df_stopwords_pl, df_stopwords_custom) #Polączona

#Ile stop words mamy?
nrow(df_stopwords)


#' ## 2.2. Utworzenie korpusu 
## 2.2. Utworzenie korpusu ----
corpus = VCorpus(VectorSource(linie_tekstu))

#Fragment korpusu
corpus[[1]][[1]]

#Oczyszczenie korpusu

corpus = tm_map(corpus, content_transformer(tolower))        # Małe litery
corpus = tm_map(corpus, removePunctuation)                   # Usuwanie interpunkcji
corpus = tm_map(corpus, removeNumbers)                       # Usuwanie cyfr
corpus = tm_map(corpus, removeWords, df_stopwords$slowo)     # Usuwanie  stop-words
corpus = tm_map(corpus, stripWhitespace)                     # Usuwanie podwójnych spacji

#' ## 2.3 Tokenizacja 
## 2.3. Tokenizacja ----

tdm = TermDocumentMatrix(corpus)
tdm_m = as.matrix(tdm) 

#Zliczenie częstości w macierzach

v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)

#' # 3. Eksploracyjna analiza danych 
# 3. Eksploracyjna analiza danych ----

#Chmura słów

wordcloud(words = tdm_df$word,
          freq = tdm_df$freq, 
          min.freq = 7, 
          max.words = 80,
          scale = c(1.5, 0.5),
          colors = brewer.pal(8, "Dark2"))

#' # 4. Topic modeling 
# 4. Topic modeling ----

#' ## 4.1. Funkcja top_terms_by_topic_LDA 
## 4.1. Funkcja top_terms_by_topic_LDA ----
# która wczytuje tekst 
# (wektor lub kolumna tekstowa z ramki danych)
# i wizualizuje słowa o największej informatywności
# przy metody użyciu LDA
# dla wyznaczonej liczby tematów

top_terms_by_topic_LDA <- function(input_text, # wektor lub kolumna tekstowa z ramki danych
                                   plot = TRUE, # domyślnie rysuje wykres
                                   k = number_of_topics) # wyznaczona liczba k tematów
{    
  corpus <- VCorpus(VectorSource(input_text))
  DTM <- DocumentTermMatrix(corpus)
  
  # usuń wszystkie puste wiersze w macierzy częstości
  # ponieważ spowodują błąd dla LDA
  unique_indexes <- unique(DTM$i) # pobierz indeks każdej unikalnej wartości
  DTM <- DTM[unique_indexes,]    # pobierz z DTM podzbiór tylko tych unikalnych indeksów
  
  # wykonaj LDA
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta") # pobierz słowa/tematy w uporządkowanym formacie tidy
  
  # pobierz dziesięć najczęstszych słów dla każdego tematu
  top_terms <- topics  %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) # uporządkuj słowa w malejącej kolejności informatywności
  
  
  
  # rysuj wykres (domyślnie plot = TRUE)
  if(plot == T){
    # dziesięć najczęstszych słów dla każdego tematu
    top_terms %>%
      mutate(term = reorder(term, beta)) %>% # posortuj słowa według wartości beta 
      ggplot(aes(term, beta, fill = factor(topic))) + # rysuj beta według tematu
      geom_col(show.legend = FALSE) + # wykres kolumnowy
      facet_wrap(~ topic, scales = "free") + # każdy temat na osobnym wykresie
      labs(x = "Terminy", y = "β (ważność słowa w temacie)") +
      coord_flip() +
      theme_minimal() +
      theme(
        strip.text = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 8)  #  Zmniejszenie czcionki słów, aby się zmieściły (możesz zmienić np. na 7 lub 6)
      ) +
      scale_fill_brewer(palette = "Set1") 
  }else{ 
    # jeśli użytkownik nie chce wykresu
    # wtedy zwróć listę posortowanych słów
    return(top_terms)
  }
  
  
} 

#' ## 4.2. Modelowanie tematów: ukryta alokacja Dirichleta 
## 4.2. Modelowanie tematów: ukryta alokacja Dirichleta ----

#Dwa tematy

number_of_topics = 2

top_terms_by_topic_LDA(tdm_df$word)

#Czery tematy

number_of_topics = 4

top_terms_by_topic_LDA(tdm_df$word)

#Sześć tematów

number_of_topics = 6

top_terms_by_topic_LDA(tdm_df$word)