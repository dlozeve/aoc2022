P←(10⊸×⊸+˜´∘⌽0∾-⟜'0') # string->number, 0 for empty string
i←P¨¨(+`0=≠¨)⊸⊔•FLines"input"  # parse input as list of lists
# Alternative version, with bqn-libs
#str ← •Import"../bqn-libs/strings.bqn"
#i ← str.ToNats¨ (2⥊@+10) str.Split •FChars"input"
•Show ⌈´+´¨i # part 1
•Show +´3↑∨+´¨i # part 2
