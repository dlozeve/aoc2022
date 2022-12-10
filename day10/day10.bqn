⟨ToNums,Split⟩←•Import"../bqn-libs/strings.bqn"
lengths‿vals←<˘⍉>{𝕊⟨i,n⟩: ⟨2, ⊑ToNums n⟩; ⟨1, 0⟩}¨" "⊸Split¨•FLines⊑•args
pos←(1∾lengths∾1)/1∾1∾1+`vals
•Show (20+40×↕6)(+´⊣×⊏)pos
•Out 1↓∾˝(@+10)⊸∾˘⊑⟜" █"¨1≥|∘‿40⥊(1↓¯1↓pos)-40|↕240
