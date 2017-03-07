TypeError      := Exception clone
ValueError     := Exception clone
IndexError     := Exception clone
AttributeError := Exception clone

ParseBaseException := Exception clone do(
    with := method(pstr, loc, msg, elem,
        if(call message arguments size != 4,
            ValueError raise($"
                Expected 4 arguments to ParseBaseException `with`, got
                #{call evalArgs}.
            " dedent)
        )
        cln := self clone
        cln loc := loc
        if(msg not,
            cln msg := pstr
            cln pstr := ""
        ,
            cln msg := msg
            cln pstr := pstr
        )
        cln parserElement := elem
        cln args := [pstr, loc, msg]
        cln
    )

    lineno := method(_lineno(self loc, self pstr))
    col := method(_col(self loc, self pstr))
    column := method(_col(self loc, self pstr))
    line := method(_line(self loc, self pstr))

    _from_exception := method(pe,
        # internal factory method to simplify creating one type of
        # ParseException from another - avoids having constructor signature
        # conflicts among subclasses
        self with(pe pstr, pe loc, pe msg, pe parserElement)
    )

    forward := method(AttributeError raise("No attribute: #{call message name}"))

    asString := method(
        out := nil
        out = $"
                #{msg} (at char #{self loc}),
                (line:#{self lineno})
        "
        # )
        # writeln(ex, out)
        # # ex whenNotNil(ex showStack)
        out
    )

    markInputline := method(markerString,
        # Extracts the exception line from the input string, and marks
        # the location of the exception with a special symbol.
        markerString ifNil(markerString := ">!<")
        line_str := self line
        line_column := self column - 1

        if(markerString,
            before := line_str exSlice(0, line_column)
            after := line_str exSlice(line_column)
            line_str := before .. markerString .. after
        )

        line_str stripped
    )

    # raise := method(
    #     self performWithArgList("with", call evalArgs) raise
    # )
)


ParseException := ParseBaseException clone
ParseFatalException := ParseBaseException clone
ParseSyntaxException := ParseFatalException clone

RecursiveGrammarException := Exception clone do(
    # exception thrown by L{ParserElement.validate} if the grammar could be
    # improperly recursive
    with := method(parseElementList,
        cln := self clone
        cln parseElementTrace := parseElementList
        cln
    )
    asString := method($"RecursiveGrammarException: #{parseElementTrace}")
)
