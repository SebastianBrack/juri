# Liste aller Keywords

* if
* repeat
* fun
* operator
* as
* iterate

### Operator-Zeichen:
```
+-*/><.=!%
```

### Trennzeichen:
```
()[]
```


# Listen
**Listennamen** beginnen mit einem ```:```.
Listen werden mit folgender Sytax deklariert:
```
:myList = [1 2 3 4]          // Erstellt die Liste mit den gegebenen Elementen

:anotherList = [2 to 345]      // Erstellt eine Liste mit den Zahlen von 2 bis 345

:longList = init 1000 0      // Erstellt eine Liste mit 1000 nullen

:evenNums = init 50 as i
    i * 2                    // Erstellt eine Liste mit den Zahlen 0,2,4,8... 
```

Einzelne Listenelemente können per **Index** referenziert und geändert werden.  Das Erste Element hat den Index 0.
```
print(0:myList)     // gibt 1 aus
print(-1:myList)    // gibt 4 aus
2:myList = 99       // weist dem Element an Index 2 den wert 99 zu: [1 2 99 4]
```

Um die **Länge** einer Liste herrauszufinden fragen Sie einfach.
```
?:myList            // evaluiert zu 4
```

Um über eine Liste zu **iterieren** stellt juri die ```iterate``` Anweisung zur Verfügung.
```
iterate :myList as x
    print(x)
```

Hier noch ein Beispiel wie man mit einer Klassischen If-Schleife über eine Liste iteriert.
```
i = 0
if i < ?:myList repeat
    print(i:myList)
    i = i+1
```

**Funktionsparameter** können als Liste festgelegt werden (noch nicht Implementiert)
```
fun snipList :list start end
    i = start
    if i <= end repeat
        print(i:list)
        i = i+1
```
