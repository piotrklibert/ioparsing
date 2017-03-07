doFile("src/syntax.io")

if(isLaunchScript,
    if(getSlot("__reloaded__") not,
        setSlot("__reloaded__", true)
        doFile(System launchScript)
        # System exit()
    )
)

doFile("src/parsing.io")
doFile("src/dsl.io")


# l1 := Literal with("a")
# l2 := Literal with("b")

# And with([l1, l2]) parseString("ab") println
# And with([l1, l2])

# MatchFirst with([]) println

# writeln("===================")
# p := DSL do(
#      W(alphas) setParseAction(block(t, $"parsed a word: '#{t at(0)}'"))
# )
# # p parseString("one") println
# # p parseString("two") println
# # p parseString("many") println


# writeln("===================")
# p := DSL do(
#      W(alphas) + OnePlus(W(alphas)) setParseAction(block(t, $"Parsed: #{t}"))
# )

# # p parseString("many many many more") foreach(println)

# writeln("===================")
# p := DSL do(
#     three_words := W(alphas) + W(alphas) + W(alphas)
#     res := And with(
#         list(C(three_words), W(alphas) named("za"))
#     ) setParseAction(
#         toks := tok __toklist join("\n")
#         $"Parsed: #{toks .. p}"
#     ) named("puci")
#     res
# )

# p parseString("many many many more") at(0) println
# p parseString("many many many more") at("za") getName println

p := DSL do(
    word := W(alphanums) named("word")
    OnePlus(word, EOL) + EOL
)

result := p parseString(" kill        orc    with hatchet
    kick orc in the face
")
result dump  println

# writeln("===================")
# p := DSL do(
#     part := (W(alphas .. "_") named("chunk", true)) + S(ZeroPlus("/"))
#     ZeroPlus(part) named("parts", true)
# )

# result := p parseString("home////cji/poligon/klibert_pl")
