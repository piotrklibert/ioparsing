_ParseResultsWithOffset := Object clone do(
    with := method(token, offset,
        cln := self clone
        cln tok := token
        cln offset := offset
        cln
    )
    at := method(i, i switch(0, self tok,
                             1, self offset))
    setOffset := method(i, self offset := i; self)
    asString := method(self tok repr)
    forward := method(call delegateTo(self tok))
)


ParseResults := Object clone do(
    with := method(toklist, name, asList, modal,
        # writeln("===\n", toklist, "  ", name, "  ", asList, "  ", modal, "  ", call sender slotSummary, "\n-----")
        cln := self clone
        cln __doinit := false
        cln __name := nil
        cln __parent := nil
        cln __accumNames := Map clone
        cln __asList := asList ifNilEval(true)
        cln __modal := modal ifNilEval(true)
        cln __tokdict := Map clone

        toklist ifNil(toklist := [])
        toklist isKindOf(List) whenFalse(toklist := [toklist])
        cln __toklist := toklist

        if(name isNil not and name size != 0,
            name := name asString
            cln __name := name
            if(modal not,
                cln __accumNames atPut(name, 0)
            )

            if((toklist isInstance([Sequence, List]) and toklist size != 0) or \
                    toklist isKindOf(ParseResults),
                if(toklist isKindOf(Sequence), toklist := [toklist])
                if(asList,
                    if(toklist isKindOf(ParseResults),
                        cln atPut(name, _ParseResultsWithOffset with(toklist copy(), 0))
                    ,
                        cln atPut(name, _ParseResultsWithOffset with(ParseResults with(toklist at(0)), 0))
                    )
                    cln at(name) __name := name
                ,
                    tl := toklist at(0) whenNil(toklist)
                    cln atPut(name, tl)

                )
            )
        )
        cln
    )

    forward := method(
        # Scheduler currentCoroutine showStack
        resend
    )


    appendSeq := method(other,
        if(other __tokdict isEmpty not,
            offset := self __toklist size
            addoffset := block(a,  if(a<0, offset, a + offset))
            otheritems := []
            other __tokdict foreach(k, vlist,
                vlist map(v,
                    res := _ParseResultsWithOffset with(v at(0), addoffset call(v at(1)))
                    otheritems append([k, res])
                )
            )
            otheritems foreach(el,
                self atPut(el at(0), el at(1))
                if(el at(1) tok isKindOf(ParseResults),
                    res := el at(1) at(0)
                    res __parent := WeakLink to(self)
                )
            )
        )
        self __toklist appendSeq(other __toklist)
        self __accumNames merge(other __accumNames)
        self
    )

    append := method(res,
        if(res isKindOf(ParseResults),
            self __toklist appendSeq(other __toklist)
            self __accumNames merge(other __accumNames)
        ,
            self __toklist append(res)
        )
        self
    )

    at := method(i,
        if(i isKindOf(Number),
            return  self __toklist[i]
        ,
            if(i in(self __accumNames) not,
                return self __tokdict at(i) at(-1) at(0)
            ,
                toks := self __tokdict at(i) map(at(0))
                return ParseResults with(toks)
            )
        )
    )

    atPut := method(k, v,
        if(v isKindOf(_ParseResultsWithOffset),
            self __tokdict atPut(k, self __tokdict at(k, []) append(v))
            sub := v at(0)
        ,
            if(k isKindOf(Number),
                self __toklist atPut(k, v)
                sub := v
            ,
                offpr := _ParseResultsWithOffset with(v, 0)
                self __tokdict atPut(k, self __tokdict at(k, []) append(offpr))
                sub := v
            )
        )
        if(sub isKindOf(ParseResults),
            ref := WeakLink clone
            ref setLink(self)
            sub __parent := ref
        )
    )

    copy := method(
        ret := ParseResults with(self __toklist)
        ret __tokdict := self __tokdict clone
        ret __parent := self __parent
        ret __accumNames merge(self __accumNames)
        ret __name := self __name
        return ret
    )

    _asStringList := method(sep,
        sep := sep whenNil("")
        out := []
        self __toklist foreach(item,
            if(out isEmpty not and sep != "",
                out append(sep)
            )
            if(item isKindOf(ParseResults),
                out appendSeq(item _asStringList())
            ,
                out append(item asString)
            )
        )
        out
    )
    asList := method(
        self __toklist map(res,
            if(res isKindOf(ParseResults), res asList(), res)
        )
    )
    repr := method(
        $"#{self __toklist fmt}; #{self __tokdict}"
    )
    remove := method(i,
        if(i isKindOf(Number), self __toklist, self __tokdict) removeAt(i)
    )

    removeAll := method(
        self __toklist := list()
        self __tokdict := Map clone
    )
    isEmpty := method(call delegateTo(self __toklist))
    size := method(call delegateTo(self __toklist))
    foreach := method(call delegateTo(self __toklist))
    reverse := method(call delegateTo(self __toklist))
    asBoolean := method(self __toklist ?isEmpty not)

    haskeys  := method(self __tokdict ?isEmpty not)
    contains := method(call delegateToMethod(self __tokdict, "hasKey"))
    keys     := method(call delegateTo(self __tokdict))
    values   := method(call delegateTo(self __tokdict))
    items    := method(self __tokdict map(k,v, [k, v]))
    content  := method(

    )
    asString := method(self dump())
    #     repr := if(self __asList,
    #         self __toklist map(asString) fmt
    #     ,
    #         self __toklist at(-1)
    #     )
    #     $"<ParseResult: [#{repr}]>"
    # )

    __lookup := method(sub,
         self __tokdict foreach(k, vlist,
            vlist foreach(res,
                v := res at(0)
                loc := res at(1)
                writeln("......"); writeln(res)
                if(sub == v, return k)
            )
        )
        return  nil
    )

    getName := method(
        if(self __name, return self __name)
        if(self __parent,
            par := self __parent link
            return if(par, par __lookup(self) , nil)
        )
        if(self size == 1 and self __tokdict size == 1 and self __tokdict values first at(1) in([0,-1]),
            return  self __tokdict keys first
        )
        return  nil
    )

    dump := method(indent, depth, full,
        indent := indent whenNil("")
        depth := depth whenNil(0)
        full := full whenNil(false)
        out := []
        NL := "\n"
        pad := [0, depth] asRange map("   ") join("")
        out append(indent .. self asList() fmt)
        full whenTrue(
            if(self haskeys(),
                self keys foreach(k,
                    v := self at(k)
                    if(out isEmpty not, out append(NL))
                    out append($"#{indent}#{pad}- #{k}")
                    if(v isKindOf(ParseResults),
                        if(v __toklist isEmpty not,
                            out append(v dump(indent, depth + 1))
                        ,
                            out append(v asString)
                        )
                    ,
                        out append(v asString)
                    )
                )
            ,
                if(self __toklist detect(isKindOf(ParseResults)),
                    self __toklist map(i, v,
                        if(v isKindOf(ParseResults),
                            out append("\n%s%s[%s]:\n%s%s%s" % (indent, pad, i, indent, pad .. "   ", v.dump(indent, depth + 1)))
                        ,
                            out append("\n%s%s[%s]:\n%s%s%s" % (indent, pad, i, indent, pad .. "   ", v asString))
                        )
                    )
                )
            )
        )
        return out join("")
    )



)
