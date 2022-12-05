str←•Import"../bqn-libs/strings.bqn"
in←∘‿2‿2⥊>str.ToNats¨•FLines⊑•args
•Show +´2≠|+´˘(×∘-)˝˘in
•Show +´(∧´(≤´0‿0⊸⍉)¨∘(⊢⋈⌽))˘in
