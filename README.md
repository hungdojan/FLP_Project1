# Funkcionální projekt z předmětu FLP

V prvním podúkolu byl naimplementován jednoduchý parser, který dokáže ověřit správnost
formátu stromu ve vstupním souboru. Parser očekává následující 2 formáty stromových
elementů:

```
<LEAF>   ::= <INDENT> "Leaf" ":" <STRING> <EOL>
<NODE>   ::= <INDENT> "Node" ":" <INT> "," <FLOAT> <IGNORED_DATA> <EOL>

<INDENT> ::= series of double spaces
<EOL>    ::= end of line
```

V případě, že strom se nepodaří vytvořit, program vyhodí chybovou hlášku. Mezi chyby
patří:
- špatné zanořené stromu (velikost "indentation" před stromovým elementem),
- špatný formát dat v elementu (není *int* nebo *float*),
- prázdný řádek,
- strom není dokončen (chybí mu potomek).
