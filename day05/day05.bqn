str←•Import"../bqn-libs/strings.bqn"

stacks‿proc←(@+2⥊10)str.Split•FChars⊑•args
stacks↩⍉¯1↓>(@+10)str.Split stacks
stacks↩str.Trim¨<˘((≠stacks)⥊0‿1‿0‿0)/stacks
orig_stacks←stacks
proc↩>(⊑str.ToNats)¨¨0‿1‿0‿1‿0‿1⊸/¨¯1↓" "⊸str.Split¨ (@+10)str.Split proc
# •Show stacks

Move←{
n𝕊⟨x,y⟩:
    crate←n↑x⊑stacks
    stacks↩(n⊸↓)⌾(x⊸⊑)stacks
    stacks↩(crate⊸∾)⌾(y⊸⊑)stacks;
𝕊𝕩: 1𝕊𝕩
}

Move˘¯1+(⊑˘/1⊸↓˘)proc
# •Show stacks
•Out ∾´⊏¨stacks

stacks↩orig_stacks
(⊑˘proc) Move˘ ¯1+1↓˘proc
# •Show stacks
•Out ∾´⊏¨stacks
