⟨ToNats,Split⟩←•Import"../bqn-libs/strings.bqn"
Dirs←{𝕩≡'R' ? 1‿0; 𝕩≡'U' ? 0‿1; 𝕩≡'L' ? ¯1‿0; 𝕩≡'D' ? 0‿¯1}
d‿n←<˘⍉>' '⊸Split¨•FLines⊑•args
inp←(⊑¨ToNats¨n)/Dirs¨∾´d

Dist←⌈´∘|-
Follow←{𝕩+(2≤𝕩 Dist 𝕨)×(××⌈∘|)2÷˜𝕨-𝕩}
Move←{Follow`𝕩⊸+⌾⊑𝕨}

F←{
    pos←(<𝕨⥊<⟨0,0⟩)Move`𝕩
    inds←⍷>¯1⊸⊏¨pos
    # Disp←{⊑⟜".#"¨(⍉⌽˘(1⊸+⌾(𝕩⊸⊑))𝕨⥊0)}
    # •Show (1+(⌈´-⌊´)inds) Disp inds
    ≠inds
}
•Show 2‿10 F¨ <inp
