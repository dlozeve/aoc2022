P←(10⊸×⊸+˜´∘⌽0∾-⟜'0') # string->number, 0 for empty string
i←P¨¨(+`0=≠¨)⊸⊔•FLines"input"  # parse input as list of lists
•Show ⌈´+´¨i # part 1
•Show +´3↑∨+´¨i # part 2
