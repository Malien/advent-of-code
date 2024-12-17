start:
  bst A ; B = A & 0b111
  bxl 3 ; B = B ^ 0b011
  cdv B ; C = A >> B
  bxc   ; B = B ^ C
  bxl 3 ; B = B ^ 0b011
  adv 3 ; A = A >> 3
  out B ; print(B & 0b111)
  jnz 0 ; if a != 0 then goto start
