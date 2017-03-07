DSL := Object clone do(
    do := method(
        msg := call argAt(0)
        cln := self clone
        cln self := cln
        cln appendProto(call sender)
        cln doMessage(msg)
    )

    Lit := method(x, Literal with(x))
    Opt := method(x, Optional with(x))
    S := method(x, Suppress with(x))
    Combine := method(x, Lobby Combine with(x))
    W := method(Word performWithArgList("with", call evalArgs))
    EOL := method(LineEnd with())
    OnePlus := method(e1, e2, OneOrMore with(e1, e2))
    ZeroPlus := method(e1, e2, ZeroOrMore with(e1, e2))
)
