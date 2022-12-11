⟨lf,Split,ToNats⟩←•Import"../bqn-libs/strings.bqn"
ms←>lf⊸Split¨(2⥊lf)Split ¯1⊸↓•FChars⊑•args
items←ToNats¨1⊑¨<˘ms
tests←{𝕊⟨d,t,f⟩: {(0=d|𝕩)⊑f‿t}}˘⊑¨ToNats¨3↓˘ms
op←{𝕩≡"* old" ? ×˜; 𝕩≡"+ old" ? +˜; '*'≡⊑𝕩 ? (⊑ToNats𝕩)⊸×; '+'≡⊑𝕩 ? (⊑ToNats𝕩)⊸+}¨23↓¨2⊏˘ms
mod←×´⊑¨ToNats¨3⊑¨<˘ms

MonkeyBusiness←{part𝕊items:
    inspected←(≠items)⥊0
    Inspect←{part𝕊i:
        op←i⊑op
        test←i⊑tests
        Throw←{
            it←(part-1)⊑⟨⌊÷⟜3 Op 𝕩,mod|Op 𝕩⟩
            items↩∾⟜it⌾((Test it)⊸⊑)items
        }
        Throw¨ i⊑items
        inspected↩(≠i⊑items)⊸+⌾(i⊸⊑)inspected
        items↩⟨⟩˙⌾(i⊸⊑)items
    }
    Round←{part⊸Inspect¨↕≠𝕩}
    Round⍟((part-1)⊑20‿10000) items
    # •Show inspected
    ×´2↑∨inspected
}

•Show 1 MonkeyBusiness items
•Show 2 MonkeyBusiness items
