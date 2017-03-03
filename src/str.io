Block oldAsString := Block getSlot("asString")
Block asFullString := Block getSlot("asString")
Block asString := Block getSlot("asSimpleString")
File asString := method("<File: '" .. self name .. "'>")
Directory asString := method("<Dir: '" .. self name .. "'>")


Sequence require := method(call sender doFile(self))

PRE := Sequence clone do(
    with := method(aString,
        cln := self clone
        cln copy(aString)
        cln
    )

    dedent := method(
        self
    )
)

PRE : := PRE getSlot("with")


Sequence beginsWithAnyOf := method(
    str := self
    if(call message arguments size == 1,
        args := call evalArgAt(0),
        args := call evalArgs
    )
    args map(prefix, str beginsWithSeq(prefix)) reduce(or)
)


// TODO: deprecated
Sequence dedentIfNeeded := method(
    debugWriteln("Deprecated method dedentIfNeeded called.")
    self dedent
)
Sequence dedent := method(
    lines := self split("\n")
    if(lines size == 0, return "")
    if(lines size == 1, return self)

    firstLine := lines first
    lines := lines slice(1)
    indent := "^ +" asRegex matchesIn(lines at(0)) at(0) sizeInChars
    lines := lines map(exSlice(indent))
    if(firstLine size > 0,
        lines prepend(firstLine stripped)
    )
    lines1 := []
    for(i, 0, lines size - 1,
        if(lines at(i) ?size == 0,
            lines1 append("\n")
        ,
            lines1 append(lines at(i))
        )

    )

    lines1 join(" ") stripped asMutable replaceSeq("\n ", "\n") asString
)

Sequence wrap := method(lineLength,
    lineLength ifNil(return self)
    out := []
    self split("\n") foreach(line,
        words := line split
        lines := list()
        curLine := "" asMutable
        curLen := 0
        words foreach(w,
            if(curLen + w size > lineLength,
                lines append(curLine asString)
                curLen := 0
                curLine zero
            )
            curLen := curLen + w size
            curLine appendSeq(" " .. w)
        )
        if(curLine size > 0,
            lines append(curLine stripped))
        out append(lines join("\n"))
    )
    out join("\n")
)

Sequence stripped := method(
    self asMutable performWithArgList("strip", call evalArgs)
)

Sequence stripCr := method(
    self asMutable removeSuffix("\r\n")
)

Sequence quoted := method(
    q := 34 asCharacter
    (q .. self .. q)
)


Regex matches := method(str,
    self matchesIn(str) all size > 0
)


List fmt := method(
    if(self size == 1,
        return self first)
    if(self size == 0,
        return "")

    els := self slice(1)
    s := self at(0) asString asMutable
    els foreach(el,
        s appendSeq(call evalArgAt(0) whenNil(", ") .. el)
    )
    s asString
)

Sequence rfind := method(str, idx,
    idx ifNil(idx := self size - 1)
    if(str isKindOf(Number), str := str asCharacter)
    if(idx >= self size, Exception raise("Argument out of bounds."))
    loop(
        if(self exSlice(idx) beginsWithSeq(str),
            return idx)
        if(idx <= 0,
            return nil)
        idx := idx - 1
    )
)


_col := method(loc, s,
    # Returns current column within a string, counting newlines as line separators.
    # The first column is number 1.
    len := s size
    nl := "\n" at(0)
    if((loc < 0) or (loc >= len), Exception raise("Index out of bounds."))
    if(s at(loc) == nl, return 0)

    if(loc >= 1 and s at(loc - 1) == nl,
        1
    ,
        pos := s rfind(nl, loc)
        if(pos not,
            loc + 1
        ,
            loc - pos
        )
    )
)

_lineno := method(loc, s,
    # Returns current line number within a string, counting newlines as line separators.
    # The first line is number 1.
    line := 1
    s foreach(i, c,
        if(i == loc, return line)
        if(c asCharacter == "\n", line := line + 1)
    )
    nil
)

_line := method(loc, strg,
    # Returns the line of text containing loc within a string, counting newlines
    # as line separators.
    lastCR := strg.rfind("\n", loc) ifNilEval(-1)
    nextCR := strg.find("\n", loc)
    if(nextCR >= 0,
        return strg exSlice(lastCR+1, nextCR)
    ,
        strg exSlice(lastCR+1)
    )
)
