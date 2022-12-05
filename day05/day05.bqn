str←•Import"../bqn-libs/strings.bqn"

stacks‿proc←(@+2⥊10)str.Split•FChars⊑•args
stacks↩⍉¯1↓>(@+10)str.Split stacks
stacks↩' '⊸≠⊸/¨<˘((≠stacks)⥊0‿1‿0‿0)/stacks
proc↩>str.ToNats¨¯1↓(@+10)str.Split proc
# •Show stacks

Move←{
stacks𝕊⟨n,x,y⟩:
    x‿y ¯1⊸+↩
    crates←n↑x⊑stacks
    stacks↩(n⊸↓)⌾(x⊸⊑)stacks
    stacks↩(crates⊸∾)⌾(y⊸⊑)stacks;
𝕨𝕊⟨x,y⟩: 𝕨𝕊⟨1,x,y⟩
}

stacks1 ← (<stacks) Move` <˘(⊑˘/1⊸↓˘)proc
•Out ∾´⊏¨¯1⊑stacks1

stacks2 ← (<stacks) Move` <˘proc
•Out ∾´⊏¨¯1⊑stacks2
