```
                                    error code
                                 ┏━━━━━━━━━━━┫                               error message
                                 ┃           ┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫         source code with markers
                                 ┃           ┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┿━━━━━━━━━┫
                               ┏━┻━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━┻━━
                main header ── ┃ [error E0000]: Oops, there was an error with your design!
   file position of primary ── ┃        ╭──▶ test:0:9
                 empty line ── ┃        │
                source code ── ┃      0 │    print("Hello, world!")
        marker with message ── ┃        •    ┬─────────────────────
                               ┃        •    ╰╸ We are fed up with hello worlds!
                 empty line ── ┃        •
                               ┃      1 │ ╭┤ print("Hello!
                               ┃      2 │ ├┤ World?")
                               ┃        • │
  other marker with message ── ┃        • ╰╸ We also don't care about printing stuff
                 empty line ── ┃        •
                               ┃      5 │    def f = 0
                               ┃        •        ┬
                               ┃        •        ╰╸ Reserved name used.
                 empty line ── ┃        •
                               ┃     15 │    def fib = NULL
                               ┃        •        ┬──
                               ┃        •        ╰╸ You already have a fibonacci function here.
                 empty line ── ┃        •
           help with marker ── ┃        │ Help: Maybe you want to include a factorial computation?
                               ┃        ├──▶ test:0:0
                 empty line ── ┃        │
                               ┃      0 │    def fact = _!
                               ┃        •    ⁺⁺⁺⁺⁺⁺⁺⁺⁺⁺⁺⁺⁺
                 empty line ── ┃        •
        note without marker ── ┃        │ Note: I don't have anything to say to you.
                 empty line ── ┃        │
               closing rule ── ┃ ───────╯
                               ┗━━━━┳━━┳━┳━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                    ┃  ┃ ┗━━┫
                                    ┗━━┫    Gap for multiline markers
                      Line number gutter
```

---

Equivalent of Unicode characters:

| Unicode | ASCII |
| :-----: | :---: |
|  `╭`  |  `+`  |
|  `─`  |  `-`  |
|  `▶`  |  `>`  |
|  `│`  |  `\|`  |
|  `•`  |  `:`  |
|  `┬`  |  `^`  |
|  `─`  |  `~`  |
|  `╰`  |  `\``  |
|  `╸`  |  `-`  |
|  `├`  |  `+`  |
|  `⁺`  |  `+`  |
|  `⁻`  |  `-`  |
|  `╯`  |  `+`  |
|  `┤`  |  `>`  |
|  `├`  |  `+`  |
|  `┼`  |  `+`  |
