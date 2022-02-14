# Liste aller Keywords

* if
* repeat
* fun
* operator

### Operator-Zeichen:
```
+-*/><.=!%
```

### Trennzeichen:
```
()[]
```


# Listen (noch nicht implementiert)
**Listennamen** beginnen mit einem ```:```.

```
:myList = [1 2 3 4]
```

Listenelemente können per **Index** referenziert werden.
```
2:myList
```

Um die **Länge** einer Liste herrauszufinden fragen Sie einfach.
```
?:myList
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

**Funktionsparameter** können als Liste festgelegt werden
```
fun snipList :list start end
    i = start
    if i <= end repeat
        print(i:list)
        i = i+1
```
