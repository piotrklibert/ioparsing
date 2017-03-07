doFile("src/syntax.io")

if(isLaunchScript,
    if(getSlot("__reloaded__") not,
        setSlot("__reloaded__", true)
        doFile(System launchScript)
        System exit()
    )
)

doFile("src/parsing.io")
doFile("src/dsl.io")



writeln("=================== 10")
p := makeParser(
     W(alphas) setParseAction(block(t, "success!"))
)
[p parseToList("one"), p parseToList("two"), p parseToList("many")] do(
    assert(unique size == 1)
    assert(at(0) == list("success!"))
)

writeln("=================== 20")
p := DSL do(
     W(alphas) named("k") + OnePlus(W(alphas))
)

assert(
    p parseString("many many many more") asList at(0) == \
    ["many", ["many", "many", "more"]]
)


writeln("=================== 30")
p := makeParser( W(alphas .. " ") )
assert(p canParseNext("many many many more", 0) == true)
assert(p parseToList("many many many more") == ["many many many more"])


writeln("=================== 40")
p := makeParser(W(alphas))
res := p parseString("many many many more")
assert(res _asStringList == ["many"])


writeln("=================== 50")
p := makeParser(
    w := W(alphas)
    w + w + w
)
res := p parseString("many many many more")
assert(res _asStringList == ["many","many", "many"])

writeln("=================== 60")
p := makeParser(
    w := W(alphas)
    OnePlus(w)
)
res := p parseString("many many many more")
assert(res _asStringList == ["many","many", "many", "more"])

writeln("=================== 70")
p := DSL do(
    word := W(alphanums) named("word")
    OnePlus(word, EOL) + EOL suppress
)

result := p parseString(" kill        orc    with hatchet
    kick orc in the face
")
result dump println

# writeln("===================")
# p := DSL do(
#     part := (W(alphas .. "_") named("chunk", true)) + S(ZeroPlus("/"))
#     ZeroPlus(part) named("parts", true)
# )

# result := p parseString("home////cji/poligon/klibert_pl")
