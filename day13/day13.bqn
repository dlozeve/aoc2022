⟨Split,lf⟩←•Import"../bqn-libs/strings.bqn"
Parse←•BQN∘{'⟩'¨⌾((']'=𝕩)⊸/)'⟨'¨⌾(('['=𝕩)⊸/)𝕩}
in←Parse¨>lf⊸Split¨(2⥊lf)Split ¯1↓•FChars⊑•args

Cmp←{
    𝕨(∧○(0==))𝕩 ? 𝕨(>-<)𝕩;
    𝕨(∧○(0=≠))𝕩 ? 0;
    0=≠𝕨 ? ¯1;
    0=≠𝕩 ? 1;
    0=𝕨(Cmp○⊑)𝕩 ? 𝕨(Cmp○(1⊸↓))𝕩;
    0=≠≢𝕨 ? (≍𝕨)𝕊𝕩;
    0=≠≢𝕩 ? 𝕨𝕊≍𝕩;
    𝕨(Cmp○⊑)𝕩
}

•Show +´(⊢×(1+↕∘≠))¯1=Cmp´˘in

_BubbleSort←{Cmp _𝕣 in:
    Bubble←{
        1≥≠𝕩 ? 𝕩;
        1=((⊑𝕩)⊑in) Cmp (1⊑𝕩)⊑in ? (1⊑𝕩)∾𝕊(⊑𝕩)∾2↓𝕩;
        (⊑𝕩)∾𝕊1↓𝕩
    }
    Bubble⍟(≠𝕩)↕≠𝕩
}

•Show ×´1+(Cmp _BubbleSort ⟨⟨2⟩⟩‿⟨⟨6⟩⟩∾⥊in)⊐0‿1
