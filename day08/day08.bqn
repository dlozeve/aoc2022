inp←>'0'-˜•FLines⊑•args
Vis←{>∊¨⌈`¨<˘𝕩}
•Show +´⥊∨´⟨⊢,⌽˘,⍉,(⌽˘⍉)⁼⟩{𝕎𝕩}¨Vis¨⟨⊢,⌽˘,⍉,⌽˘⍉⟩{𝕎𝕩}¨4⥊<inp
Score←{m𝕊⟨x,y⟩:
    v←x‿y⊑m
    up←⌽x↑y⊏˘m
    left←⌽y↑x⊏m
    right←(y+1)↓x⊏m
    down←(x+1)↓y⊏˘m
    ×´{(≠𝕩)⌊1++´∧`v>𝕩}¨up‿left‿right‿down
}
•Show ⌈´⥊inp⊸Score¨↕≢inp
