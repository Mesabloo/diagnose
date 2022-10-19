```
                   line number gutter
                                  ┏━┫      inner gap
                                  ┃ ┣━━━━━━┫                                  source code with markers
                                  ┃ ┃      ┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫
                               ┏━━┻━┻━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━
                main header ── ┃ ERROR in test:0:9 - error E0000: Oops, there was an error with your design!
 first error marker message ── ┃   0        print("Hello, world!")
             primary marker ── ┃            ─────────────────────
                source code ── ┃     We are fed up with hello worlds!
                 empty line ── ┃ 
          additional header ── ┃   test:1:3 - error:
                source code ── ┃     1         print("Hello!  
       other primary marker ── ┃               ───────────────
                               ┃     2         World?")
                               ┃               ────────
                               ┃       We also don't care about printing stuff.
                 empty line ── ┃ 
          additional header ── ┃   test:5:1 - error:
                               ┃     5         def f = 0
                               ┃                   ─
                               ┃       Reserved name used.
                 empty line ── ┃ 
additional secondary marker ── ┃   test:15:4 - note:
                               ┃     15        def fib = NULL
                               ┃                   ───
                               ┃       You already have a fibonacci function here.
                 empty line ── ┃ 
         a hint with a file ── ┃   help: Maybe you meant to include a factorial computation?
                               ┃               def fact = _!
                               ┃               ⁺⁺⁺⁺⁺⁺⁺⁺⁺⁺⁺⁺⁺
                 empty line ── ┃ 
     a note without markers ── ┃   note: I don't have anything to say to you.
                 empty line ── ┃ 
                               ┗┳━┳━━┳━┳━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━
                                ┃ ┃  ┃ ┃      ┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫
                                ┃ ┃  ┃ ┣━━━━━━┫                               source code with markers
                                ┃ ┃  ┗━┫      inner gap
                                ┗━┫    line number gutter
                          outer gap               
```

---

Equivalent of Unicode characters:

| Unicode | ASCII |
| :-----: | :---: |
|  `─`  |  `~`  |
|  `…`  | `...` |
|  `⁺`  |  `+`  |
|  `⁻`  |  `-`  |
