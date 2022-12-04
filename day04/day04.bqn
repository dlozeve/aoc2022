str←•Import"../bqn-libs/strings.bqn"
in←>⊑∘str.ToNats¨¨>'-'str.Split¨¨','str.Split¨•FLines⊑•args
•Show +´2≠|+´˘(×∘-)˝˘in
•Show +´(∧´(≤´0‿0⊸⍉)¨∘(⊢⋈⌽))˘in
