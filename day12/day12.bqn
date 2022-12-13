map←>•FLines⊑•args
IndexOf←{<˘⍉>⌊(≠⊏𝕨)(÷˜⋈|)/⥊𝕨=𝕩}
s‿e←⊑¨map⊸IndexOf¨"SE"
h←-⟜'a'¨'z'⌾(e⊸⊑)'a'⌾(s⊸⊑)map

_fixedpoint←{𝔽∘⊢⍟≢⟜𝔽_𝕣∘⊢⍟≢⟜𝔽𝕩}

Dijkstra←{h𝕊s:
    vis←(≢h)⥊0
    dist←0⌾(s⊸⊑)(≢h)⥊∞
    Neighbors←{𝕊n:
        ns←n⊸+¨∾(-⋈⊢)¨(⌽⋈⊢)0‿1
        ns ((∧´¨(≢vis)⊸>¨)/⊢)↩ # inside the grid
        ns ((∧´¨0⊸≤¨)/⊢)↩ # inside the grid
        ns↩({¬𝕩⊑vis}¨ns)/ns # unvisited
        (1≥({𝕩⊑h}¨ns)-n⊑h)/ns # reachable height
    }
    Visit←{
        ∧´⥊𝕩 ? 𝕩;
	nexts←((≢dist)⥊(¬⥊𝕩)) IndexOf 1
	nexts↩(⍋{𝕩⊑dist}¨nexts)⊏nexts
        n←⊑nexts
        {dist (1+n⊑dist)⊸⌊⌾(𝕩⊸⊑)↩}¨Neighbors n # update distances
        ¬⌾(n⊸⊑)𝕩 # mark node as visited
    }
    Visit _fixedpoint vis
    dist
}

•Show e⊑h Dijkstra s

ss←map IndexOf'a'
•Show ⌊´ss⊑(-h)Dijkstra e
