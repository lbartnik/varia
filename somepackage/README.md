Kolekcja
========

collection - zbiór obiektów w postaci plików rds w dwupoziomowej strukturze
  w systemie plików; obiektom towarzyszą pliki rds z etykietami; jako klasa
  w R umożliwia wyszukiwanie w ramach kolekcji z możliwością przekazania do
  równoległego przetwarzania oraz nadawanie etykiet


```
c <- collection('path/to/col', 'name') # nazwa jest opcjonalna?

select(c, .id = '4c4712a4141d261ec0ca8f9037950685') %>%
  tag(tag_name = 'tag-value', another_tag_name = 'another-value')

tag(c, '4c4712a4141d261ec0ca8f9037950685', some_tag = 'some-value')

select(c, tag_name = 'tag-value')
```

`tag` zapisuje nadane etykiety do systemy plików i nie zwraca żadnej wartości
`select` wybiera obiekty na podstawie etykiet: `.id` która jest zawsze obecna
  i nadawana automatycznie w momencie dodania obiektu do kolekcji lub innej,
  utworzonej przez uzytkownika; (wyszukiwanie powinno zakładać, że ciągi znaków
  to wyrażenia regularne)


dopisanie obiektu do kolekcji (podobnie nie zwraca wartości):

```
insert(c, iris, tag_name = 'my version of iris')
```

automatyczne nadawanie etykiet wielu obiektom:
```
select(c, tag = '*.xy.*') %>%
  tag(function(x) c(class = class(x)))
```


Przetwarzanie
=============

```
select(c, some_thing = 'something') %>%
  do({
    lm(y ~ x, data = .)
  }) %>%
  store_in(c, 'other-name')
```

dopóki nie pojawi się wywołanie store\_in cały potok jest obiektem
zawierającym poszczególne wywołania, funkcje i środowiska wywołań;

dodanie store\_in powoduje uruchomienie procesu, domyślnie za pomocą
`parallel::mclapply`, ale można przed store\_in dodać np. `cluster()`
lub inne by zmienić miejsce uruchomienia

```
select(c, some_thing = 'something') %>%
  do({
    lm(y ~ x, data = .)
  }) %>%
  cluster(aws_cluster) %>%
  store_in(c, 'other-name')
```


Operator %>%
============

Operator musi zapisywać wszystkie informacje potrzebne do uruchomienia
potoku w innym procesie R: wywołanie i kompletne środowisko; może się
zdarzyć tak, że funkcja w potoku nie będzie pochodziła ze znanego
pakietu, tzn. będzie funkcją użytkownika - wtedy trzeba przesłać jej
ciało, ale także ew. zmienne globalne do których się odwołuje i
środowiska-rodziców

(w pierwszej wersji można wprowadzić ograniczenie że funkcja musi być
samowystarczalna, tzn. nie może odwoływać się do zmiennych spoza niej
samej i jej argumentów; można też napisać prostą funkcję, która to
przetestuje - np. uruchamiając kod użytkownika w "czystym" środowisku
albo badając ciało funkcji w inny sposób - czy jest taka metoda w R?)

pytanie: czy operator %>% z magrittr pozwala zapisać ciało funkcji?


Zapisywanie wyników
===================

Obecny projekt zakłada możliwość wykorzystania Dropbox do współdzielenia
danych, a zatem:

* procesy wykonawcze (worker) nie mogą modyfikować etykiet istniejących
  obiektów (może dodatkowa, ukryta flaga, która pozwoli na automatyczne
  badanie tego warunku?)
* procesy mogą zwracać więcej niż jeden obiekt

```
# ........ %>%
  do({
    list(
	  tagged(lm(y ~ x, data = .), name = 'model-1', trial = 10),
	  tagged(lm(y ~ z, data = .), name = 'model-2', trial = 10)
	)
  }) %>%
  store_in(.....)
```


