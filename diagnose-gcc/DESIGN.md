```
                               ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                main header ── ┃ test:0:9: error: Oops, there was an error with your design! [E0000]
 first error marker message ── ┃     We are fed up with hello worlds!
                source code ── ┃   print("Hello, world!")
             primary marker ── ┃   └────────────────────┘
          additional header ── ┃ test:1:3: error: We also don't care about printing stuff.
                source code ── ┃   print("Hello!  
       other primary marker ── ┃   └───────────────…
          additional header ── ┃ test:5:1: error: Reserved name used.
                               ┃   def f = 0
                               ┃       ┴
additional secondary marker ── ┃ test:15:4: note: You already have a fibonacci function here.
                               ┃   def fib = NULL
                               ┃       └─┘      
         a hint with a file ── ┃ help: Maybe you meant to include a factorial computation?
                               ┃   def fact = _!
                               ┃   ⁺⁺⁺⁺⁺⁺⁺⁺⁺⁺⁺⁺⁺
     a note without markers ── ┃ note: I don't have anything to say to you.
                               ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

---

Equivalent of Unicode characters:

|   Unicode   | ASCII |
| :----------: | :---: |
| `└` or `┴` |  `^`  |
|     `─`     |  `~`  |
|     `┘`     |  `~`  |
|     `…`     | `...` |
|     `⁺`     |  `+`  |
|     `⁻`     |  `-`  |
