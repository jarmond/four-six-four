(ns four-six-four.example-basic-programs)

(def curve
  {:listing
   ["10 INK 1,26"
    "20 MODE 1"
    "30 INPUT a"
    "40 MOVE 1,1"
    "50 DRAW 400,1"
    "60 DRAW 400,400"
    "70 FOR x=1 TO 400 STEP a"
    "80 DRAW 400,x"
    "90 MOVE x,1"
    "100 NEXT"
    "110 WHILE INKEY$=\"\""
    "120 WEND"
    "130 GOTO 10"]
   :tokens
   [0xb 0 10 0 0xa2 0x20 0x0f 0x2c 0x19 0x1a 0
    0x8 0 20 0 0xad 0x20 0x0f 0
    0x8 0 30 0 0xa3 0x20 0x61 0
    0xa 0 40 0 0xae 0x20 0x0f 0x2c 0x0f 0
    0xc 0 50 0 0x94 0x20 0x1a 0x90 0x1 0x2c 0x0f 0
    0xe 0 60 0 0x94 0x20 0x1a 0x90 0x1 0x2c 0x1a 0x90 0x1 0
    0x14 0 70 0 0x9e 0x20 0x78 0xef 0x0f 0x20 0xec 0x20 0x1a 0x90 0x1 0x20 0xe6 0x20 0x61 0
    0xc 0 80 0 0x94 0x20 0x1a 0x90 0x1 0x2c 0x78 0
    0xa 0 90 0 0xae 0x20 0x78 0x2c 0x0f 0
    0x6 0 100 0 0xb0 0
    0xc 0 110 0 0xd6 0x20 0xff 0x43 0xef 0x22 0x22 0
    0x6 0 120 0 0xd5 0
    0xa 0 130 0 0xa0 0x20 0x1e 0xa 0 0
    ]})

