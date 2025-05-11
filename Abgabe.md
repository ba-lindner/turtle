# Turtle Graphics

## Abgabepaket
Alle relevanten Dateien sind unter
[https://github.com/ba-lindner/turtle](https://github.com/ba-lindner/turtle)
zu finden. Eine Kopie des Repositories ist auch als .zip-Datei beigefügt.
Es beinhaltet:
* Den gesamten Quellcode im Ordner `src`.
* Einige Beispiele im Ordner `examples`.
  Jedes Beispiel, was Erweiterungen zur Sprache (siehe unten) verwendet,
  kennzeichnet dies in den ersten Zeilen mit dem Kommentar
  `" +feature <feature>`.
* Diese Datei (`Abgabe.md`).

Weitere Komponenten, die nicht direkt für die Abgabe relevant sind:
* Der Ordner `ccomp` beinhaltet einige Dateien, die für den
  Turtle-zu-C Transpiler notwendig sind.
* Im Ordner `turtle-web` wurde angefangen, einen Webserver für
  Turtle zu schreiben. Dazugehörig ist ebenfalls die Datei `openapi.yaml`.
* Die Dateien `README.md`, `Extensions.md` und `Todo.md` enthalten
  einiges Gelaber. Der relevante Teil davon ist jedoch auch hier enthalten.

Zusätzlich zum Repository ist eine Dokumentation ebenfalls als .zip beigefügt.
Die Startseite liegt bei `turtle/index.html`. Es ist jedoch zu beachten, dass die
Dokumentation derzeit (Stand 11.05.) nur teilweise fertiggestellt ist.
Da derzeit noch daran gearbeitet wird, kann vermutlich gegen Ende der Woche
eine vollständige Dokumentation auf GitHub zu finden sein. Weiter unten ist
beschrieben, wie die Dokumentation aus dem Quellcode erzeugt werden kann.

## Build-Anleitung

### Vorraussetzungen
* `cargo` Version >= 1.78 (sollte auf linuprak bereits
  in passender Version vorhanden sein)
* `CMake` Version <= 3.31 (CMake >= 4.0 kann nicht verwendet werden,
  da die sdl2-Library für Rust ein veraltetes Format von CMakeLists.txt
  verwendet, welches von aktuellen CMake-Verisonen nicht mehr erkannt wird)

### Build
Zum Kompilieren kann der Befehl `cargo build` bzw. `cargo build --release`
verwendet werden. Das Resultat ist dann unter `target/debug/turtle` bzw.
`target/release/turtle` zu finden.

Um `turtle` direkt auszuführen, kann auch der Befehl `cargo run [--release] -- <args>` verwendet werden.

Ebenfalls möglich ist die Installation. Dies geschieht über
`cargo install --path .` oder
`cargo install --git https://github.com/ba-lindner/turtle`.
Dadurch wird `turtle` in das Verzeichnis `~/.cargo/bin` installiert
und sollte sich somit im `PATH` befinden.

Die Dokumentation kann über `cargo doc [--no-deps] --document-private-items`
erzeugt werden und ist danach unter `target/doc` zu finden. Die Startseite
liegt bei `turtle/index.html`. Die bei der Generierung auftretenden Warnungen
sind bekannt und sind zu ignorieren.

### Aufruf
Da `turtle` zum Parsen der CLI-Argumente `clap` verwendet, bietet
`turtle help` bereits eine umfangreiche Hilfe für die Bedienung.

Die Ausgabe von `turtle help` und `turtle help debug`:
```
C:\Users\bernh\Documents\Rust\turtle>turtle help
Usage: turtle <COMMAND>

Commands:
  run      Start interpreter
  shell    Start turtle shell
  debug    Start debugger
  compile  Compile to C
  check    Check syntax
  help     Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help
  -V, --version  Print version

C:\Users\bernh\Documents\Rust\turtle>turtle help debug
Start debugger

Usage: turtle debug [OPTIONS] <FILE> [-- <ARGS>...]

Arguments:
  <FILE>
          turtle source file

  [ARGS]...
          args passed to turtle

Options:
  -f, --feature <FEATURES>
          enabled features

          Possible values:
          - types:          introduces types: strings, booleans and numbers
          - multithreading: parallel execution with multiple turtles drawing concurrently
          - events:         react to users clicking or typing
          - parameters:     easier command-line arguments

  -F, --disabled <DISABLED>
          disabled features

          Possible values:
          - types:          introduces types: strings, booleans and numbers
          - multithreading: parallel execution with multiple turtles drawing concurrently
          - events:         react to users clicking or typing
          - parameters:     easier command-line arguments

  -b, --breakpoint <BREAKPOINT>
          set breakpoints in format line,column

  -i, --interface <INTERFACE>
          select debugging interface

          [default: terminal]

          Possible values:
          - terminal: run as standalone debugger
          - vs-code:  run as vscode extension

  -w, --window <WINDOW>
          display mechanism

          [default: sdl]

          Possible values:
          - sdl:      use SDL2 to draw turtles
          - void:     no display
          - buffered: buffered window

  -h, --help
          Print help (see a summary with '-h')
```

## Bericht
### Zeitaufwände
Eine akkurate Auflistung der Aufwände ist nicht mehr sinnvoll möglich, da
mehr als ein Jahr an dem Projekt gearbeitet wurde. Aus den Anfängen existieren
jedoch noch **ungefähre** Angaben je Modul:
* 7h für den Lexer
* 7h für den Parser
* 4h für den Interpreter
* 7h für den Compiler
* 2h für sonstiges

Diese Zeitaufwände beschreiben den damaligen Stand, d.h. beinhalten
keine der Erweiterungen zur Turtle Graphics Sprache.

### Besondere Aspekte
Nach der Fertigstellung des Compilers war ein Debugger das nächste Ziel.
Dies gestaltete sich jedoch als überaus schwierig. Eine wichtige Anforderung
an den Debugger war neben der direkten Funktionalität, dass ein Nutzer mit
Eingaben auf der Befehlszeile den Programmablauf steuern kann, auch die
Bereitstellung einer API, mit der auch andere Anwendungsfälle abgedeckt
werden könnten. Dies bedeutet, dass jeder Debug-Befehl einem Funktionsaufruf
entspricht, der nach Ende des Befehls ebenfalls abgeschlossen ist. Ein
trivialer Debugger kann dies nicht bieten, da er die Nutzerinteraktion
tief in seinen rekursiven Aufrufen macht. Es musste also irgendwie die
Möglichkeit geschaffen werden, den Weg entlang des AST, den ein Interpreter
durch seine rekursiven Aufrufe abbildet, zu speichern und wieder zu laden.

Nach einer längeren Zeit erfolglosen herumprobierens wurde jedoch eine
wahrhaft geniale Lösung gefunden: Rusts `async` Funktionen. Diese werden
intern in State-Machines umgewandelt und bieten somit genau die gesuchte
Funktionalität (partielle Ausführung irgendwo tief im Stack), ohne dass
viel Code geändert werden muss. Lediglich an den beiden "Enden" der Kette
rekursiver Aufrufe musste ein wenig Code spezifisch für `async`
hinzugefügt werden, und schon hatte sich der Interpreter in einen Debugger
verwandelt.

Mit einem fertigen Debugger war nun der Weg frei für eine Erweiterung
der Turtle Graphics Sprache um weitere Elemente. Dabei stechen insbesondere
die Erweiterungen um Events und Multithreading heraus, da beide eine
Umstellung von einer einzelnen Turtle auf mehrere Turtles erforderten.
Dies erforderte nicht nur eine größere Umstrukturierung des Debuggers,
sondern auch die Definition der exakten Semantik - wie unabhängig sind
die einzelnen Turtles voneinander (z.B. Farbe), wie verhalten sich
`stop` und `finish` etc. Nach mehreren Tagen Arbeit wurde diese
Umstellung jedoch abgeschlossen. Insbesondere der Moment, als direkt
der erste Test (das Beispiel `multithreading.tg`) erfolgreich durchlief,
ist als ein Höhepunkt des Projektes hervorzuheben.

Ein dritter Aspekt, der bemerkenswert ist, wird ebenfalls durch die neue
Struktur des Debuggers ermöglicht. Mit der Umstellung auf mehrere Turtles,
die jedoch alle auf demselben Fenster zeichnen, war ein allgemeines
Interface für Windows bereits vorhanden. Gleichfalls wurde die
Kontrollschleife des Debuggers ausgelagert, sodass auch hier eine
definierte Schnittstelle existiert. Mit diesen können nun beide
Komponenten frei ausgetauscht werden.

Mit diesen Schnittstellen konnte nun der eigentlich bemerkenswerte
Aspekt erreicht werden: Der Kern des Debuggers, also der nicht austauschbare
Teil, der die gesamte Logik umfasst, beinhaltet fast gar keinen
Systemaufruf (ausgenommen malloc). Nahezu alle Aspekte, die einen Systemaufruf
benötigen - Zeichnen, Delays, Ausgaben von Debuginformationen - sind in
die austauschbaren Komponenten ausgelagert worden. Derzeit verbleiben
etwa drei Systemaufrufe, die einfach nur noch nicht ausgelagert wurden.

## Erweiterungen
Für `turtle` wurden drei Erweiterungen der Sprache implementiert, die
optional sind und automatisch von Lexer & Parser erkannt werden.
Eine grundsätzliche Änderung, die immer aktiv ist, ist die Auflösung
von Bedingungen: diese sind nun Ausdrücke, die einen Wert vom Typ
Boolean ergeben. Daraus ergibt sich eine Änderung im Vorrang der
Negation: `not a < b` wird nun als `(not a) < b` geparsed, obwohl
`not (a < b)` nach Vorgabe korrekt wäre.

### Typen
Die erste Erweiterung umfasst die Einführung von Typen. Es stehen
`num`, `string` und `bool` zur verfügung. Obwohl eine starke und
statische Typisierung implementiert wurde, existieren nur zwei
Stellen, an denen Typen angegben werden müssen:
* Bei Funktionsdeklarationen, z.B. `calc check(x: num, a: string): bool`
* Als Ausdruck zur Umwandlung zwischen den verschiedenen Typen,
  z.B. `store num(@1) in len`

Alle Variablen, sowohl lokale als auch globale, haben einen zur
Compilezeit bekannten Typen. Dabei haben alle vordefinierten Variablen
den Typen `num`, mit Ausnahme der Befehlszeilenargumente `@1` bis `@9`.
Diese sind vom Typ `string`.

Mit dieser Erweiterung kommen zudem zwei weitere vordefinierte Funktionen,
`substr(s: string, start: num, end: num): string` und
`strlen(s: string): num`. Ebenfalls wird der Befehl `print <expr>`
eingeführt, der einen String-Ausdruck nimmt und ausgibt. Strings
können über `add <expr> to <var>` und `<expr> + <expr>` verändert
werden, wobei jede Veränderung einen neuen String erzeugt.

### Events
Events bieten Nutzern die Möglichkeit, auch während der Ausführung eines
Programmes damit zu interagieren (und nicht nur beim Aufruf Parameter
anzugeben). Events basieren auf der Möglichkeit, mehrere Turtles
gleichzeitig auszuführen: wird ein Event ausgelöst, so wird eine neue
Turtle mit initialen Werten der Liste aktiver Turtles hinzugefügt,
die den entsprechenden Eventhandler ausführt.

Es existieren zwei verschiedene Events, für die folgendermaßen ein
Handler deklariert werden kann:
```
event key(char: string)
  " handler code
endevent

event mouse(x: num, y: num, left: bool)
  " handler code
endevent
```
Die Parameter der Handler müssen dabei mit den oben angegebenen
Typen übereinstimmen, es können jedoch auch nur alle Parameter
bis zum letzten benötigten genommen werden (d.h. sie können von
hinten beginnend weggelassen werden). Daraus ergibt sich ebenfalls
die Einschränkung, dass die Verwendung der Parameter `char` oder
`left` zwangsweise die Verwendung von Typen erfordert.

### Multithreading
Multithreading bietet insbesondere für Fraktale wie dem Pythagorasbaum
eine Möglichkeit, diese deutlich schneller zu zeichnen. Die Sprache
wird dabei lediglich um den Befehl `split <path>(<args>)` erweitert.
Dieser ist syntaktisch identisch mit dem Befehl `path`. Der
Unterschied besteht darin, dass bei `split` die ausführende Turtle
geklont wird und der Klon diese Funktion ausführt, während das
Original direkt mit dem nächsten Befehl weitermacht.

Eine Anwendung kann beim Beispiel `pyth_frak_split.tg` gesehen werden.
Dieses ist nahezu identisch mit dem vorgegebenen Beispiel `pyth_frak.tg`,
es wurde lediglich einmal zu Beginn `@delay` auf 200 gesetzt und der
erste rekursive Aufruf (in Zeile 17) durch ein äquivalentes `split`
ersetzt. Trotz des erheblich höheren Delays ist das parallele Programm
erheblich früher fertig.

Implementiert ist das Multithreading, indem alle Turtles hintereinander
bis zum nächsten Zeichenbefehl ausgeführt wird. Diese umfassen `walk`
in allen Varianten, `clear` und `wait`. Letzterer hat keinen Effekt,
außer dass er als Zeichenbefehl zählt und somit zur Synchronisation
mehrerer Turtles verwendet werden kann. Wurden nun einmal alle Turtles
soweit ausgeführt, so werden alle in dieser Zeit erstellten Klone
an die Liste aktive Turtles angehangen und ebenfalls bis zum ersten
Zeichenbefehl ausgeführt. Dies geschieht solange bis keine neuen
Turtles hinzukommen. Haben nun alle Turtles gezeichnet (oder das
Ende ihrer Funktion erreicht), pausiert der Debugger für `@delay`
Millisekunden. Danach werden für alle Events, die währenddessen
aufgetreten sind, die passenden Turtles der Liste hinzugefügt.
