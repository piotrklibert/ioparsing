doFile("src/syntax.io")

if(isLaunchScript,
    if(getSlot("__reloaded__") not,
        setSlot("__reloaded__", true)
        doFile(System launchScript)
        System exit()
    )
)

doFile("src/parsing.io")

l1 := Literal with("a")
l2 := Literal with("b")

And with([l1, l2]) parseString("ab")
And with([l1, l2]) println

MatchFirst with([]) println

DSL := Object clone do(
    do := method(
        msg := call argAt(0)
        cln := self clone
        cln self := cln
        cln appendProto(call sender)
        cln doMessage(msg)
    )
    L := method(x, Literal with(x))
    W := method(Word performWithArgList("with", call evalArgs))
    OnePlus := method(e, OneOrMore with(e))
)

p := DSL do(
     W(alphas) setParseAction(block(t, $"parsed a word: '#{t}'"))
)

writeln("===================")
p parseString("one") println
p parseString("two") println
p parseString("many") println

p := DSL do(
     OnePlus(W(alphas))
) setParseAction(block(t, $"Parsed: #{t}"))

p parseString("many many more") println
