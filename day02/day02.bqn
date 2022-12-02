in←|>"AX"⊸-¨0‿2⊸⊏¨•FLines⊑•args
ShapeScore←{+´1+1⊏˘𝕩}
WinScore←{+´3×3⊸|1+-˜´˘𝕩}
•Show (ShapeScore+WinScore)in
in2←(3⊸|∘⊏˘in+¯1⊸+)⌾(1⊸⊏˘)in
•Show (ShapeScore+WinScore)in2
