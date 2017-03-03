TypeError      := Exception clone
ValueError     := Exception clone
IndexError     := Exception clone
AttributeError := Exception clone

doRelativeFile("str.io")

_MAX_LEN   := 9999999999999999


alphas     := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
nums       := "0123456789"
hexnums    := nums .. "ABCDEFabcdef"
alphanums  := alphas .. nums
_bslash    := 92 asCharacter
whites     := "\t\n\r "
printables := "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!'#$%&\"()*+,-./:;<=>?@[\\]^_`{|}~"


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
        ex := try(
            out := $"
                #{msg} (at char #{self loc}),
                (line:#{self lineno}, col:#{self column})
            " dedent
        )
        ex whenNotNil(ex showStack)
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

_ParseResultsWithOffset := Object clone do(
    with := method(p1, p2,
        cln := self clone
        cln tup := [p1, p2]
        cln
    )

    at := method(i,
        self tup at(i)
    )
    setOffset := method(i,
        self tup := [self tup first, i]
        self
    )
)


ParseResults := Object clone do(
    with := method(toklist, name, asList, modal,
        if(toklist isKindOf(ParseResults), return toklist)
        toklist ifNil(toklist := [])
        cln := self clone
        cln __doinit := false
        cln __name := nil
        cln __parent := nil
        cln __accumNames := Map clone
        cln __asList := asList ifNilEval(true)
        cln __modal := modal ifNilEval(true)
        cln __toklist := []

        if(toklist isKindOf(List),
            toklist isEmpty ifFalse(
                cln __toklist copy(toklist)
            )
        ,
            cln __toklist := [toklist]
        )
        cln __tokdict := Map clone
        if(name,
            name isKindOf(Sequence) ifFalse(name := name asString)
            if(modal not, cln __accumNames atPut(name, 0))
            cln __name := name

            if(toklist isInstance([Sequence, List]) and toklist size != 0,
                toklist isKindOf(Sequence) whenTrue(toklist := [toklist])
                if(toklist isKindOf(ParseResults),
                    cln atPut(name, _ParseResultsWithOffset with(toklist copy(), 0))
                ,
                    cln atPut(name, _ParseResultsWithOffset with(ParseResults with(toklist[0]),0))
                )
                cln at(name) __name := name
            )
        )
        cln
    )

    forward := method(
        # Scheduler currentCoroutine showStack
        resend
    )
    isEmpty := method(call delegateTo(self __toklist))

    appendSeq := method(other,
        results := if(other isKindOf(ParseResults), other __toklist, other)
        self __toklist appendSeq(results)
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
            if(i in(self __accumNames),
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

    remove := method(i,
        if(i isKindOf(Number),
            mylen := self __toklist size
            self __toklist removeAt(i)
        ,
            self __tokdict removeAt(i)
        )
    )


    size := method(call delegateTo(self __toklist))
    foreach := method(call delegateTo(self __toklist))
    reverse := method(call delegateTo(self __toklist))
    asBoolean := method(self __toklist ?isEmpty not)

    haskeys  := method(self __tokdict ?isEmpty not)
    contains := method(k, self __tokdict hasKey(k))
    keys     := method(call delegateTo(self __tokdict))
    values   := method(call delegateTo(self __tokdict))
    items    := method(self __tokdict map(k,v, [k, v]))

    asString := method(self __toklist fmt)
    # pop := method(arg, default,
    #     if(call args isEmpty,
    #         args := [-1]
    #     )
    #     for k,v in kwargs items(),
    #         if k == 'default',
    #             args := [args[0], v]
    #         else,
    #              TypeError raise("pop() got an unexpected keyword argument '%s'" % k)
    #     if (isinstance(args[0], int) or
    #                     len(args) == 1 or
    #                     args[0] in self),
    #         index := args[0]
    #         ret := self[index]
    #         del self[index]
    #        return  ret
    #     else,
    #         defaultvalue := args[1]
    #        return  defaultvalue
    # )

)


ParserElement := Object clone do(
    # Abstract base level parser element class.
    defaultWhiteChars ::= " \n\t\r"
    verbose_stacktrace := false
    strRepr := nil
    setDefaultWhitespaceChars := method(chars,
        ParserElement defaultWhiteChars := chars
    )

    inlineLiteralsUsing := method(cls,
        ParserElement _literalStringClass := cls
    )

    with := method(savelist,
        cln := self clone
        cln parseAction := list()
        cln failAction := nil
        cln strRepr := nil
        cln resultsName := nil
        cln saveAsList := savelist
        cln skipWhitespace := true
        cln whiteChars := ParserElement defaultWhiteChars
        cln copyDefaultWhiteChars := true
        cln mayReturnEmpty := false
        cln keepTabs := false
        cln ignoreExprs := list()
        cln debug := false
        cln streamlined := false
        cln mayIndexError := true
        cln errmsg := ""
        cln modalResults := true
        cln debugActions := [nil, nil, nil]
        cln re := nil
        cln callPreparse := true
        cln callDuringTry := false
        cln
    )

    copy := method(
        cpy := self clone
        cpy parseAction := self parseAction
        cpy ignoreExprs := self ignoreExprs
        if(self copyDefaultWhiteChars,
            cpy whiteChars := ParserElement defaultWhiteChars
        )
        cpy
    )

    setName := method(name,
        self name := name
        self errmsg := "Expected " .. name
        if(self hasSlot("exception"),
            self exception msg := self errmsg
        )
        self
    )

    setResultsName := method(name, listAllMatches,
        newself := self copy()
        name endsWithSeq("*") ifTrue(
            name := name exSlice(-1)
            listAllMatches := true
        )
        newself resultsName := name
        newself modalResults := listAllMatches not
        newself
    )

    setParseAction := method(
        self parseAction := call evalArgs
        self callDuringTry := false
        self
    )

    addParseAction := method(
        self parseAction appendSeq(call evalArgs)
        self
    )

    addCondition := method(
        msg := "failed user-defined condition"
        fns := call evalArgs
        fns foreach(fn,
            pa := block(s, l, t,
                if(fn call(s,l,t) not,
                    ParseException with(s, l, msg) raise
                )
            )
            self parseAction append(pa)
        )
        return self
    )

    setFailAction := method(fn,
        self failAction := fn
        self
    )

    _skipIgnorables := method(instring, loc,
        exprsFound := true
        while(exprsFound,
            exprsFound := false
            self ignoreExprs foreach(e,
                ex := try(
                    loop(
                        loc := e _parse(instring, loc) first
                        exprsFound := true
                    )
                )
                ex catch(ParseException,
                    nil
                ) pass
            )
        )
        loc
    )

    preParse := method(instring, loc,
        if(self ignoreExprs isEmpty not,
            loc := self _skipIgnorables(instring, loc)
        )

        if(self skipWhitespace,
            wt := self whiteChars
            instrlen := instring size
            while((loc < instrlen) and (instring at(loc) in(wt)),
                loc := loc + 1
            )
        )
        loc
    )

    parseImpl := method(instring, loc, doActions,
        doActions ifNil(doActions := true)
        [loc, []]
    )

    postParse := method(instring, loc, tokenlist,
        tokenlist
    )

    _parseNoCache := method(instring, loc, doActions, callPreParse,
        doActions ifNil(doActions := true)
        callPreParse ifNil(callPreParse := true)

        if(callPreParse and self callPreparse,
            preloc := self preParse( instring, loc )
        ,
            preloc := loc
        )
        tokensStart := preloc

        res := self parseImpl( instring, preloc, doActions )
        loc := res first
        tokens := res last

        tokens := self postParse(instring, loc, tokens)

        retTokens := ParseResults with(
            tokens, self resultsName, self saveAsList, self modalResults
        )

        if(self parseAction isEmpty not and (doActions or self callDuringTry),
            self parseAction foreach(fn,
                tokens := fn call(retTokens, instring, tokensStart)
                if(tokens,
                    retTokens := ParseResults with(
                        tokens, self resultsName, self saveAsList, self modalResults
                    )
                )
            )
        )
        [loc, retTokens]
    )

    tryParse := method(instring, loc,
        self _parse(instring, loc, false) first
    )

    canParseNext := method(instring, loc,
        ex := try(
            self tryParse(instring, loc)
        )
        ex catch(ParseException,
            return false
        )
        return true
    )


    _parse := method(
        self performWithArgList("_parseNoCache", call evalArgs)
    )

    parseString := method(instring, parseAll,
        parseAll ifNil(
            parseAll := false
        )
        # ParserElement resetCache()
        self streamlined ifFalse(
            self streamline()
        )
        self ignoreExprs foreach(e,
            e streamline()
        )
        self keepTabs ifFalse(
            instring := instring asMutable replaceSeq("\t", "    ")
        )
        res := self _parse(instring, 0)
        loc := res first
        tokens := res last
        parseAll ifTrue(
            loc := self preParse(instring, loc)
            se := Empty clone + StringEnd clone
            se _parse(instring, loc)
        )
        tokens
    )


    + := method(other,
        other isKindOf(Sequence) ifTrue(
            other := ParserElement _literalStringClass with(other)
        )
        if(other isKindOf(ParserElement) not,
            debugWriteln($"Cannot combine element of type #{other type} with ParserElement")
            return nil
        )
        return And with([self, other])
    )


    * := method(other,
        if(other isKindOf(Number) not or other isKindOf(List),
            TypeError raise($"Cannot multiply 'ParserElement' and '#{other}' objects")
        )
        if(other isKindOf(Number),
            minElements := other
            optElements := 0
        )
        if(other isKindOf(List),
            numberOrNil := block(val, val isKindOf(Number) or val isNil)
            if(other detect(val, numberOrNil call(val) not),
                 TypeError raise($"Cannot multiply 'ParserElement' and #{other} objects")
            )
            if(other[0] not,
                other := [0, other[1]]
            )
            if(other[0] isKindOf(Number) and other[1] not,
                if(other[0] == 0,
                    return ZeroOrMore(self)
                )
                if(other[0] == 1,
                    return OneOrMore(self)
                )
                return self * other[0] + ZeroOrMore(self)
            )
            if(other[0] isKindOf(Number) and other[1] isKindOf(Number),
                minElements := other first
                optElements := other last
                optElements -= minElements
            )
        )


        if(minElements < 0,
             ValueError raise("cannot multiply ParserElement by negative value"))
        if(optElements < 0,
             ValueError raise("second tuple value must be greater or equal to first tuple value"))
        if(minElements == 0 and optElements == 0,
             ValueError raise("cannot multiply ParserElement by 0 or (0,0)"))

        if(optElements,
            makeOptionalList := block(n,
                if(n > 1,
                    return Optional(self + makeOptionalList(n - 1))
                ,
                    return Optional(self)
                )
            )
            if(minElements,
                if(minElements == 1,
                    ret := self + makeOptionalList call(optElements)
                ,
                    ret := And([self]*minElements) + makeOptionalList(optElements))
            ,
                ret := makeOptionalList(optElements)
            )
        ,
            if(minElements == 1,
                ret := self
            ,
                ret := And(0 to(minElements) map(self))
            )
        )
        return ret
    )

    | := method(other,
        if(other isKindOf(Sequence),
            other := ParserElement _literalStringClass with(other)
        )
        if(other isKindOf(ParserElement) not,
            return nil
        )
        return MatchFirst with([ self, other ])
    )

    ^ := method(other,
        if(other isKindOf(Sequence),
            other := ParserElement _literalStringClass with(other)
        )
        if(other isKindOf(ParserElement) not,
            debugWriteln($"Cannot combine element #{other} with ParserElement")
            return nil
        )
        return Or with([self, other])
    )


    and := method(other,
        if(other isKindOf(Sequence) not,
            other := ParserElement _literalStringClass with(other)
        )
        if(other isKindOf(ParserElement) not,
            debugWriteln("Cannot combine element #{other} with ParserElement")
            return nil
        )
        return Each with([self, other])
    )


    ~ := method(
        return NotAny with(self)
    )

    suppress := method(Suppress with(self))

    leaveWhitespace := method(
        self skipWhitespace := false
        return self
    )

    setWhitespaceChars := method(chars,
        self skipWhitespace := true
        self whiteChars := chars
        self copyDefaultWhiteChars := false
        self
    )

    parseWithTabs := method(
        self keepTabs := true
        return self
    )

    ignore := method(other,
        if(other isKindOf(Sequence),
            other := Suppress with(other)
        )

        if(other isKindOf(Suppress),
            if(other in(self ignoreExprs) not,
                self ignoreExprs append(other))
        ,
            self ignoreExprs append(Suppress with(other copy))
        )
        return self
    )

    asString := method(
        self type
    )

    streamline := method(
        self streamlined := true
        self strRepr := nil
        self
    )

    checkRecursion := method(parseElementList,
        nil
    )

    validate := method( validateTrace=[] ,
        # Check defined expressions for valid structure, check for infinite
        #  recursive definitions.
        self.checkRecursion([])
    )

    parseFile := method(fileOrFilename, parseAll,
        # Execute the parse expression on the given file or filename.
        # If a filename is specified (instead of a file object),
        # the entire file is opened, read, and closed before parsing.
        contents := if(fileOrFilename isKindOf(File),
            fileOrFilename
        ,
            File with(fileOrFilename)
        ) contents
        return self parseString(contents, parseAll)
    )

    == := method(other,
        if(other isKindOf(ParserElement),
            return (self uniqueId == other uniqueId) or \
                self slotNames detect(sn, self getSlot(sn) != other getSlot(sn)) not
        )
        if(other isKindOf(Sequence),
            return self matches(other)
        )
        resend
    )

    != := method(other,
        return (self == other) not
    )


    matches := method(testString, parseAll,
        # Method for quick testing of a parser against a test string. Good for simple
        # inline microtests of sub expressions while building up larger parser.

        # Parameters,
        #  - testString - to test against this expression for a match
        #  - parseAll - (default=C{True}) - flag to pass to C{L{parseString}} when running tests

        # Example:,
        #     expr := Word with(nums)
        #     assert expr matches("100")
        ex := try(
            self parseString(testString, parseAll ifNilEval(true))
            return true
        )

        ex catch(ParseBaseException,
            return false
        ) pass
    )
)

Token := ParserElement clone do(
    # Abstract C{ParserElement} subclass, for defining atomic matching patterns.
    with := method(
        super(with(false))
    )
)


Empty := Token clone do(
    # An empty token, will always match.
    with := method(
        cln := resend
        cln name := "Empty"
        cln mayReturnEmpty := true
        cln mayIndexError := false
        cln
    )
)


NoMatch := Token clone do(
    # A token that will never match.
    with := method(
        cln := resend
        cln name := "NoMatch"
        cln mayReturnEmpty := true
        cln mayIndexError := false
        cln errmsg := "Unmatchable token"
        cln
    )

    parseImpl := method(instring, loc, doActions,
        doActions := doActions whenNil(true)
        ParseException with(instring, loc, self errmsg, self) raise
    )
)


Literal := Token clone do(
    # Token to exactly match a specified string.

    # Example:,
    #     Literal('blah').parseString('blah')  # -> ['blah']
    #     Literal('blah').parseString('blahfooblah')  # -> ['blah']
    #     Literal('blah').parseString('bla')  # -> Exception: Expected "blah"

    # For case-insensitive matching, use L{CaselessLiteral}.

    # For keyword matching (force word break before and after the matched string),
    # use L{Keyword} or L{CaselessKeyword}.
    with := method(matchString,
        if(matchString not or matchString size == 0,
            return Empty with()
        )
        cln := super(with)
        cln match := matchString
        cln matchLen := matchString size
        cln firstMatchChar := matchString at(0)
        cln name := $"'#{cln match}'"
        cln errmsg := "Expected " .. cln name
        cln mayReturnEmpty := false
        cln mayIndexError := false
        cln

    )

    asString := method(self match quoted)

    # Performance tuning: this routine gets called a *lot*
    # if this is a single character match string  and the first character matches,
    # short-circuit as quickly as possible, and avoid calling startswith
    parseImpl := method(instring, loc, doActions,
        doActions ifNil(doActions := true)
        beginsWithMatch := block(
            instring exSlice(loc) beginsWithSeq(self match)
        )
        if((instring at(loc) == self firstMatchChar) and \
            ((self matchLen == 1) or beginsWithMatch call),
            return [loc + self matchLen, self match]
        )
        ParseException with(instring, loc, self errmsg, self) raise(self errmsg)
    )
)
ParserElement _literalStringClass := Literal


ParseExpression := ParserElement clone do(
    """
    Abstract subclass of ParserElement, for combining and post-processing parsed tokens.
    """
    with := method(exprs, savelist,
        savelist := savelist ifNilEval(false)
        cln := super(with(savelist))

        if(exprs isKindOf(Sequence),
           cln exprs := [ParserElement _literalStringClass with(exprs)])

        if(exprs isKindOf(List),
            cln exprs := exprs map(x,
                if(x isKindOf(Sequence),
                    ParserElement _literalStringClass with(x)
                ,
                    x
                )
            )
        )
        if(exprs isKindOf(ParserElement),
            cln exprs := [ exprs ]
        )
        cln callPreparse := false
        cln
    )

    at := method(exprs doMessage(call message))

    append := method(other ,
       self exprs append(other)
       self strRepr := nil
       self
    )

    leaveWhitespace := method( self ,
        # Extends C{leaveWhitespace} defined in base class, and also
        # invokes C{leaveWhitespace} on all contained expressions.
        self skipWhitespace := false
        self exprs := exprs map(copy leaveWhitespace)
        self
    )

    ignore := method(other,
        if(other isKindOf(Suppress),
            if(other in(self ignoreExprs) not,
                resend
                self exprs map(ignore(self ignoreExprs last))
            )
        ,
            resend
            self exprs map(ignore(self ignoreExprs last))
        )
        self
    )

    asString := method(
        try(return resend)

        if(self strRepr isNil,
           self strRepr := self type ..  self exprs
        )
        self strRepr
    )

    streamline := method(
        resend
        self exprs foreach(streamline)
        # collapse nested And's of the form And( And( And( a,b), c), d) to And( a,b,c,d )
        # but only if there are no parse actions or resultsNames on the nested And's
        # (likewise for Or's and MatchFirst with's)
        # if ( len(self.exprs) == 2 ):
        #     other =self exprs[0]
        #     if (  other isKindOf(self __class__ ) and
        #           not(other.parseAction) and
        #          other resultsName is None and
        #           notother debug ):
        #        self exprs =other exprs[:] + [self exprs[1] ]
        #        self strRepr = None
        #        self mayReturnEmpty |=other mayReturnEmpty
        #        self mayIndexError  |=other mayIndexError

        #     other =self exprs[-1]
        #     if (  other isKindOf(self __class__ ) and
        #           not(other.parseAction) and
        #          other resultsName is None and
        #           notother debug ):
        #        self exprs =self exprs[:-1] +other exprs[:]
        #        self strRepr = None
        #        self mayReturnEmpty |=other mayReturnEmpty
        #        self mayIndexError  |=other mayIndexError

        self errmsg := "Expected " .. self asString
        self
    )

    setResultsName := method(resend)

    validate := method(validateTrace,
        validateTrace := validateTrace ifNilEval([])
        tmp := validateTrace clone append(self)
        self exprs map(validate(tmp))
        self checkRecursion([])
    )

    copy := method(
        ret := resend
        ret exprs copy(self exprs map(copy))
        ret
    )
)

And := ParseExpression clone do(
    _ErrorStop := Empty clone do(
        with := method(
            cln := resend
            cln name := '-'
            cln leaveWhitespace()
            cln
        )
    )

    with := method(exprs, savelist,
        cln := resend
        cln mayReturnEmpty := cln exprs detect(mayReturnEmpty not) ifNilEval(true)
        cln setWhitespaceChars(cln exprs first whiteChars)
        cln skipWhitespace := cln exprs first skipWhitespace
        cln callPreparse := true
        cln
    )

    parseImpl := method(instring, loc, doActions,
        doActions ifNil(
            doActions := true
        )

        # pass False as last arg to _parse for first element, since we already
        # pre-parsed the string as part of our And pre-parsing
        res := self exprs[0] _parse(instring, loc, doActions, false)
        loc := res[0]
        resultlist := res[1]
        errorStop := false

        self exprs slice(1) foreach(e,
            if(e isKindOf(And _ErrorStop),
                errorStop := true
                continue)
            if(errorStop,
                ex := try(
                    res := e _parse(instring, loc, doActions)
                    loc := res[0]
                    exprtokens := res[1]
                )
                ex catch(ParseSyntaxException,
                    raise(ex)
                ) catch(ParseBaseException,
                    ex __traceback__ = nil
                    ParseSyntaxException._from_exception(ex) raise
                ) catch(IndexError,
                     ParseSyntaxException raise(instring, len(instring), self.errmsg, self)
                )
            ,
                res := e _parse( instring, loc, doActions )
                loc := res[0]
                exprtokens := res[1]
            )
            if(exprtokens isNil not,
                resultlist appendSeq(exprtokens)
             )
        )
        return [loc, resultlist]
    )

    checkRecursion := method(parseElementList,
        subRecCheckList := parseElementList clone append(self)
        self exprs foreach(
            checkRecursion(subRecCheckList)
            if(mayReturnEmpty not, break)
        )
    )

    asString := method(
        if(self hasSlot("name"),
            return self name)

        if(self strRepr isNil,
            self strRepr := "{" .. self exprs map(?asString) fmt .. "}")

        return self strRepr
    )

)

Or := ParseExpression clone do(
    with := method(exprs, savelist,
        cln := resend
        if(cln exprs isEmpty not,
           self mayReturnEmpty := self exprs detect(mayReturnEmpty)
        ,
           self mayReturnEmpty := true)
    )

    parseImpl := method(instring, loc, doActions,
        doActions ifNil(doActions := true)
        maxExcLoc := -1
        maxException := nil
        matches := []
        loc2 := nil

        self exprs foreach(e,
            ex := try(loc2 := e tryParse(instring, loc))
            ex catch(ParseException,
                ex __traceback__ := nil
                if(err loc > maxExcLoc,
                    maxException := err
                    maxExcLoc := err loc)
            ) catch(IndexError,
                if(instring size > maxExcLoc,
                    maxException := ParseException with(instring,len(instring),e.errmsg,self) raise
                    maxExcLoc := instring size)
            )
            ex ifNil(
               matches append([loc2, e])
            )
        )

        if(matches isEmpty not,
           matches sort(key=lambda x: -x[0])
            for _,e in matches,
                try,
                   return e _parse( instring, loc, doActions )
                except ParseException as err,
                   err __traceback__:=None
                    iferr loc > maxExcLoc,
                        maxException:=err
                        maxExcLoc =err loc)

        if (maxException isNil not,
            maxException msg := self errmsg
            maxException raise
        ,
            ParseException raise(
                instring, loc, "no defined alternatives to match", self
            )
        )
    )


    __ixor__ := method( other ,
        if(other isKindOf(Sequence),
            other := ParserElement _literalStringClass with(other))
        self append(other) #Or( [ self, other ] )
    )

    asString := method(
        if(self hasSlot("name"),
           return self name)

        if(self strRepr isNil,
           self strRepr := "{" .. self exprs map(asString) join(" ^ ") .. "}")

       return self strRepr
    )

    checkRecursion := method(parseElementList,
        subRecCheckList := parseElementList clone append(self)
        self exprs foreach(e,
           e checkRecursion(subRecCheckList)
        )
    )
)

MatchFirst := ParseExpression clone do(
    with := method(exprs, savelist,
        savelist ifNil(savelist := true)
        cln := resend
        if(cln exprs isEmpty not,
           cln mayReturnEmpty := cln exprs detect(mayReturnEmpty)
        ,
           cln mayReturnEmpty := true)
        cln
    )

    parseImpl := method(instring, loc, doActions,
        doActions ifNil(doActions := true)

        maxExcLoc := -1
        maxException := nil
        found := nil

        self exprs foreach(e,
            if(found isNil not,
                break
            )
            err := try(
                found := e _parse(instring, loc, doActions)
            )
            err catch(ParseException,
                if(err loc > maxExcLoc,
                    maxException := err
                    maxExcLoc := err loc
                )
            ) catch(IndexError,
                if(instring size > maxExcLoc,
                    maxException := ParseException with(
                        instring, instring size, e errmsg, self
                    )
                    maxExcLoc := instring size
                )
            )
        )


        if(found isNil not,
            return found)

        if(maxException isNil not,
           maxException msg := self errmsg
           maxException raise
        ,
           ParseException with(
                instring, loc, "no defined alternatives to match", self
           ) raise
        )
    )

    | := method(other,
        if(other isKindOf(Sequence),
            other := ParserElement _literalStringClass with(other))
       return self append(other) #MatchFirst with( [ self, other ] )
    )

    asString := method(
        if(self hasSlot("name"),
           return self name)

        if(self strRepr isNil,
           self strRepr := "{" .. self exprs map(asString) join(" | ") .. "}")

       return self strRepr
    )

    checkRecursion := method(parseElementList,
        subRecCheckList := parseElementList clone append(self)
        self exprs foreach(checkRecursion(subRecCheckList))
    )
)


_escapeRegexRangeChars := method(s,
    s = s asMutable
    "\\^-]" foreach(c,
        c := c asCharacter
        s replaceSeq(c, _bslash .. c)
    )
    s replaceSeq("\n", "\\n")
    s replaceSeq("\t", "\\t")
    s asString
)

Word := Token clone do(
    with := method(initChars, bodyChars, opts,
        defaults := {
            min := 1; max := 0; exact := 0; asKeyword := false;
            excludeChars := nil
        }
        opts := defaults merge(opts whenNil({})) asObject

        cln := resend

        if(opts excludeChars,
            initChars := initChars asList select(in(opts excludeChars) not)
            if(bodyChars,
                bodyChars := bodyChars asList select(in(opts excludeChars) not)
            )
        )

        cln initCharsOrig := initChars
        cln initChars := initChars asList
        if(bodyChars,
           cln bodyCharsOrig := bodyChars
           cln bodyChars := bodyChars asList
        ,
           cln bodyCharsOrig := initChars
           cln bodyChars := initChars asList
        )

        cln maxSpecified := opts max > 0

        if(opts min < 1,
            ValueError raise("
                Cannot specify a minimum length < 1; use Optional(Word()) if
                zero-length word is permitted
            " dedent)
        )

        cln minLen := opts min

        if(opts max > 0,
           cln maxLen := opts max
        ,
           cln maxLen := _MAX_LEN)

        if(opts exact > 0,
           cln maxLen := opts exact
           cln minLen := opts exact
        )

        cln name := cln asString
        cln errmsg := "Expected " .. cln name
        cln mayIndexError := false
        cln asKeyword := opts asKeyword
        allChars := (cln initCharsOrig .. cln bodyCharsOrig)

        if(allChars containsSeq(" ") not and (opts min == 1 \
                                              and opts max == 0 \
                                              and opts exact == 0),
            if(cln bodyCharsOrig == cln initCharsOrig,
                esc := _escapeRegexRangeChars(cln initCharsOrig)
                cln reString := $"[#{esc}]+"
            ,
                if(cln initCharsOrig size == 1,
                    initEsc := cln initCharsOrig
                    bodyEsc := _escapeRegexRangeChars(cln.bodyCharsOrig)
                    cln reString := $"#{initEsc}[#{bodyEsc}]*"
                ) else(
                    initEsc := _escapeRegexRangeChars(cln initCharsOrig)
                    bodyEsc := _escapeRegexRangeChars(cln bodyCharsOrig)
                    cln reString := $"[#{initEsc}][#{bodyEsc}]*"
                )
            )
            if(cln asKeyword,
                cln reString := "\\b" .. cln.reString .. "\\b")

            try(
                # cln reString println
                cln re = cln reString asRegex
            ) catch(Exception,
                cln re := nil
            )
        )
        cln
    )

    parseImpl := method(instring, loc, doActions,
        doActions := doActions whenNil(true)
        if(self re,
            matches := self re matchesIn(instring exSlice(loc))
            if(matches ?count == 0,
                 ParseException with(instring, loc, self errmsg, self) raise
            )
            # TODO: inefficient, needs fixing the RegexToken Addon
            loc := loc + matches at(0) end
            return [loc, matches at(0) string]
        )

        if(self initChars contains(instring[loc]) not,
             ParseException with(instring, loc, self errmsg, self) raise)

        start := loc
        loc := loc + 1
        instrlen := instring size
        bodychars := self bodyChars
        maxloc := start + self maxLen
        maxloc := maxloc min(instrlen)
        while(loc < maxloc and instring[loc] in(bodychars),
            loc := loc + 1
        )

        throwException := false
        if(loc - start < self minLen,
            throwException := true)
        if(self maxSpecified and loc < instrlen and instring[loc] in(bodychars),
            throwException := true)
        if(self asKeyword,
            if((start > 0 and instring[start-1] in(bodychars)) or \
               (loc < instrlen and instring[loc] in(bodychars)),
                throwException := true
            )
        )

        if(throwException,
             ParseException with(instring, loc,self errmsg, self) raise
        )

       return [loc, instring exSlice(start, loc)]
    )

    asString := method(
        try(return resend) catch(Exception, nil)
        if(self strRepr isNil,
            charsAsStr := block(s, if(s size > 4, s[0 .. 4] .. "...", s))

            if (self initCharsOrig != self bodyCharsOrig,
                init := charsAsStr call(self initCharsOrig)
                body := charsAsStr call(self bodyCharsOrig)
                self strRepr := "W:(#{init},#{body})"
            ,
                self strRepr := "W:(" .. charsAsStr call (self initCharsOrig) .. ")"
            )
        )
        self strRepr
    )
)


ParseElementEnhance := ParserElement clone do(
    with := method(expr, savelist,
        savelist := savelist whenNil(false)
        cln := super(with(savelist))
        if(expr isKindOf(Sequence),
            if(ParserElement._literalStringClass isKindOf(Token),
                expr := ParserElement _literalStringClass with(expr)
            ,
                expr := ParserElement _literalStringClass with(Literal with(expr))
            )
        )
        cln expr := expr
        cln strRepr := nil
        if(expr isNil not,
           cln mayIndexError := expr mayIndexError
           cln mayReturnEmpty := expr mayReturnEmpty
           cln setWhitespaceChars(expr whiteChars)
           cln skipWhitespace := expr skipWhitespace
           cln saveAsList := expr saveAsList
           cln callPreparse := expr callPreparse
           cln ignoreExprs appendSeq(expr ignoreExprs)
        )
        cln
    )

    parseImpl := method(instring, loc, doActions,
        doActions ifNil(doActions := true)

        if(self expr isNil not,
           return self expr _parse(instring, loc, doActions, false)
        ,
             ParseException with("", loc, self.errmsg, self) raise
        )
    )

    leaveWhitespace := method(
        self skipWhitespace := false
        self expr := self expr
        if(self expr isNil not, self expr leaveWhitespace())
        self
    )

    ignore := method(other,
        if(other isKindOf(Suppress),
            if(other in(self ignoreExprs) not,
                super(ignore(other))
                if(self expr, self expr ignore(self ignoreExprs last))
            )
        ,
            super(ignore( other ))
            if(self expr, self expr ignore(self ignoreExprs last))
        )
        self
    )

    streamline := method(
        resend
        if(self expr, self expr streamline())
        self
    )

    checkRecursion := method(parseElementList,
        if(self in(parseElementList),
             RecursiveGrammarException with(parseElementList append(self)) raise
        )
        subRecCheckList := parseElementList clone append(self)
        if(self expr, self expr checkRecursion(subRecCheckList))
    )

    validate := method(validateTrace,
        validateTrace := validateTrace whenNil([]) clone append(self)
        if(self expr, self expr validate(validateTrace))
        self checkRecursion([])
    )

    asString := method(
        try(return resend) catch(Exception, nil)
        if(self strRepr isNil and self,
           self strRepr := self type .. ":(" .. self expr asString .. ")"
        )
        self strRepr
    )
)


_MultipleMatch := ParseElementEnhance clone do(
    with := method(expr, stopOn,
        cln := super(with(expr))
        cln saveAsList := true
        ender := stopOn
        if(ender isKindOf(Sequence),
            ender := ParserElement _literalStringClass with(ender)
        )
        cln not_ender := if(ender isNil, nil, ~ender)
        cln
    )

    parseImpl := method(instring, loc, doActions,
        doActions ifNil(doActions := true)
        sExprParse := block(s,l,a, self expr _parse(s,l,a))
        checkEnder := self not_ender isNil not

        if(checkEnder,
            tryNotEnder := block(s,l, self not_ender tryParse(s,l))
        )

        # must be at least one (but first see if we are the stopOn sentinel;
        # if so, fail)
        if(checkEnder,
            tryNotEnder call(instring, loc)
        )
        res := sExprParse call(instring, loc, doActions, false)
        loc := res[0]
        tokens := res[1]

        try(
            hasIgnoreExprs := self ignoreExprs isEmpty not
            while(true,
                if(checkEnder, tryNotEnder call(instring, loc))
                if(hasIgnoreExprs,
                    preloc := self _skipIgnorables(instring, loc)
                ,
                    preloc := loc
                )
                res := sExprParse call(instring, preloc, doActions)
                loc := res[0]
                tmptokens := res[1]
                if(tmptokens isEmpty not, # or tmptokens haskeys(),
                    tokens appendSeq(tmptokens)
                )
            )
        ) catch(ParseException, nil)

        return [loc, tokens]
    )
)

OneOrMore := _MultipleMatch clone do(
    asString := method(
        self getSlot("name") whenNotNil(return self name)
        self strRepr whenNil(
            self strRepr := "{" .. self expr  .. "}..."
        )
        self strRepr
    )
)

ZeroOrMore := _MultipleMatch clone do(
    with := method(expr, stopOn,
        cln := super(with(expr, stopOn))
        cln mayReturnEmpty := true
        cln
    )

    parseImpl := method(instring, loc, doActions,
        try(return resend) \
            catch(ParseException, return [loc, []])
    )

    asString := method(
        self hasSlot("name") whenTrue(
           return self name
        )

        self strRepr whenNil(
           self strRepr := "[" .. self expr asString .. "]..."
        )
        self strRepr
    )
)


_NullToken := Object clone do(
    asBoolean := method(false)
    asString := method("")
)

_optionalNotMatched := _NullToken()
Optional := ParseElementEnhance clone do(
    with := method(expr, default,
        cln := super(with(expr, false))
        cln saveAsList := cln expr saveAsList
        cln defaultValue := default whenNil(_optionalNotMatched)
        cln mayReturnEmpty := true
    )

    parseImpl := method(instring, loc, doActions,
        doActions := doActions whenNil(true)
        try(
            res := self expr _parse(instring, loc, doActions, false)
            loc := res[0]
            tokens := res[1]
        ) catch(ParseException, #IndexError,
            if(self defaultValue != _optionalNotMatched,
                if(self expr resultsName,
                    tokens := ParseResults with([self defaultValue])
                    tokens atPut(self expr resultsName, self defaultValue)
                ,
                    tokens := [self defaultValue]
                )
            ,
                tokens := []
            )
        )
        return [loc, tokens]
    )

    asString := method(
        self getSlot("name") whenNotNil(return self name)
        self strRepr whenNil(self strRepr := "[" .. self expr  .. "]")
        self strRepr
    )
)

TokenConverter := ParseElementEnhance clone do(
    with := method(expr,
        cln := resend
        cln saveAsList := false
        cln
    )
)

Suppress := TokenConverter clone do(
    postParse := method(instring, loc, tokenlist,
        return []
    )

    suppress := method(self)
)


RegexToken := Token clone do(       # changed name because it clashed with addon
    with := method(pattern, flags,
        # The parameters C{pattern} and C{flags} are passed to the
        # C{re.compile()} function as-is. See the Python C{re} module for an
        # explanation of the acceptable patterns and flags
        cln := resend
        flags := flags whenNil(0)

        if(pattern isKindOf(Sequence),
            cln pattern := pattern
            cln flags := flags
            cln re := cln pattern asRegex
            cln reString := cln pattern
        ,
            if(pattern isKindOf(RegexToken),
                cln re := pattern
                cln pattern :=  pattern pattern
                cln reString := pattern pattern
                cln flags := flags
            ,
                ValueError raise("
                    RegexToken may only be constructed with a string or a compiled
                    RE object
                " dedent)
            )
        )
        cln name := cln asString
        cln errmsg := "Expected " + cln name
        cln mayIndexError := false
        cln mayReturnEmpty := true
        cln
    )

    parseImpl := method( instring, loc, doActions,
        doActions ifNil(doActions := true)
        result := self re matchesIn(instring exSlice(loc))
        if(result count == 0,
             ParseException with(instring, loc, self errmsg, self) raise)

        result := result at(0)
        loc := loc + result end
        return [loc, result captures]
    )

    asString := method(
        try(return resend) catch(Exception, nil)
        self strRepr ifNil(
            self strRepr := "Re:(%s)" %%(self pattern)
        )
        self strRepr
    )
)


Each := ParseExpression clone do(
    with := method(exprs, savelist,
        savelist ifNil(savelist := true)
        cln := super(with(exprs, savelist))
        cln mayReturnEmpty := cln exprs detect(mayReturnEmpty not)
        cln skipWhitespace := true
        cln initExprGroups := true
        cln
    )
    parseImpl := method(instring, loc, doActions,
        doActions ifNil(doActions := true)

        if(self initExprGroups,
            optionals := self exprs select(isKindOf(Optional)) map(e,
                [e expr uniqueId, e expr]
            )
            self opt1map := Map performWithArgList("with", optionals flatten)
            opt1 := self exprs select(isKindOf(Optional)) map(expr)
            opt2 := self exprs select(mayReturnEmpty and isKindOf(Optional) not)
            self optionals := opt1 appendSeq(opt2)
            self multioptionals := self exprs select(isKindOf(ZeroOrMore)) map(expr)
            self multirequired := self exprs select(isKindOf(OneOrMore)) map(expr)
            self required := self exprs select(isInstance(Optional, ZeroOrMore, OneOrMore))
            self required appendSeq(self multirequired)
            self initExprGroups := False
        )
        tmpLoc := loc
        tmpReqd := self required clone
        tmpOpt  := self optionals clone
        matchOrder := []

        keepMatching := true
        while(keepMatching,
            tmpExprs := tmpReqd appendSeq(tmpOpt) \
                appendSeq(self multioptionals) \
                appendSeq(self multirequired)
            failed := []
            tmpExprs foreach(e,
                ex := try(
                    tmpLoc := e tryParse( instring, tmpLoc )
                )
                ex catch(ParseException,
                    failed append(e)
                )
                ex ifNil(
                    matchOrder append(self opt1map at(e uniqueId, e))
                    if(e in(tmpReqd),
                        tmpReqd remove(e)
                    ,
                        if(e in(tmpOpt), tmpOpt remove(e))
                    )
                )
            )
            if(failed size == tmpExprs size,
                keepMatching := false)
        )
        if(tmpReqd,
            missing := tmpReqd map(asString) join(", ")
            ParseException with(
                instring, loc, "Missing one or more required elements (%s)" % missing
            ) raise)

        # add any unmatched Optionals, in case they have default values defined
        matchOrder appendSeq(
            self exprs select(isKindOf(Optional) and expr in(tmpOpt))
        )

        resultlist := []
        matchOrder foreach(e,
            res := e _parse(instring,loc,doActions)
            loc := res[0]
            results := res[1]
            resultlist append(results)
        )

        finalResults := resultlist reduce(appendSeq)
        return [loc, finalResults]
    )

    asString := method(
        try(return resend) catch(Exception, nil)
        self strRepr ifNil(
            self strRepr := "{" ..  self exprs map(asString) join(" & ") .. "}"
        )
        self strRepr
    )

    checkRecursion := method(parseElementList,
        subRecCheckList := parseElementList clone append(self)
        self exprs foreach(checkRecursion(subRecCheckList))
    )
)

NotAny := ParseElementEnhance clone do(
    with := method(expr,
        cln := resend
        # do NOT use self leaveWhitespace(), don't want to propagate to exprs
        cln skipWhitespace := false
        cln mayReturnEmpty := true
        cln errmsg := "Found unwanted token, " .. self expr asString
        cln
    )

    parseImpl := method( instring, loc, doActions=True ,
        if(self expr canParseNext(instring, loc),
             ParseException raise(instring, loc, self errmsg, self)
        )
        return [loc, []]
    )

    asString := method(
        try(return resend) catch(Exception, nil)
        self strRepr ifNil(self strRepr := "~{" .. self expr asString + "}")
        self strRepr
    )
)

SkipTo := ParseElementEnhance clone do(
    with := method(other, include, ignore, failOn,
        include := include whenNil(false)

        cln := super(with(other))
        cln ignoreExpr := ignore
        cln mayReturnEmpty := true
        cln mayIndexError := false
        cln includeMatch := include
        cln asList := false
        if(failOn isKindOf(Sequence),
            cln failOn := ParserElement _literalStringClass with(failOn)
        ,
            cln failOn := failOn
        )
        cln errmsg := "No match found for " .. cln expr asString
        cln
    )

    parseImpl := method(instring, loc, doActions,
        doActions ifNil(doActions := true)

        startloc := loc
        instrlen := instring size
        expr := self expr
        expr_parse := block(s,l,a,p, self expr _parse(s,l,a,p))
        failOn_canParseNext := block(s,l, self failOn ?canParseNext(s,l))
        ignoreExpr_tryParse := block(s,l, self ignoreExpr ?tryParse(s,l))

        tmploc := loc
        broke := false
        while(tmploc <= instrlen,
            if(failOn_canParseNext call(instring, tmploc),
                broke := true; break
            )
            self ignoreExpr whenNotNil(
                loop(
                    try(tmploc := ignoreExpr_tryParse call(instring, tmploc)
                    ) catch(ParseBaseException, break)
                )
            )

            ex := try(
                expr_parse call(instring, tmploc, false, false)
            )
            ex isInstance(ParseException, IndexError) ifTrue(
                # no match, advance loc in string
                tmploc := tmploc + 1
            )
            ex ifNil(broke := true; break)
        )
        if(broke not,
            # ran off the end of the input string without matching skipto expr,
            # fail
            ParseException with(instring, loc, self errmsg, self) raise
        )

        # build upreturn  values
        loc := tmploc
        skiptext := instring exSlice(startloc, loc)
        skipresult := ParseResults with(skiptext)

        if(self includeMatch,
            res := expr_parse call(instring, loc, doActions, false)
            loc := res[0]
            mat := res[1]
            skipresult appendSeq(mat)
        )

       return [loc, skipresult]
    )
)


__ParseResults := Object clone do(

)

#     get := method( key, defaultValue=nil,
# )
#         """
#        return s named result matching the given key, or if there is no
#         such name, thenreturn s the given C{defaultValue} or C{nil} if no
#         C{defaultValue} is specified.

#         Similar to C{dict.get()}.

#         Example:,
#             integer := Word(nums)
#             date_str := integer("year") + '/' + integer("month") + '/' + integer("day")

#             result := date_str.parseString("1999/12/31")
#             print(result.get("year")) # -> '1999'
#             print(result.get("hour", "not specified")) # -> 'not specified'
#             print(result.get("hour")) # -> nil
#         """
#         if key in self,
#            return  self[key]
#         else,
#            return  defaultValue

#     insert := method( index, insStr ,
# )
#         """
#         Inserts new element at location index in the list of parsed tokens.

#         Similar to C{list.insert()}.

#         Example:,
#             print(OneOrMore(Word(nums)).parseString("0 123 321")) # -> ['0', '123', '321']

#             # use a parse action to insert the parse location in the front of the parsed results
#             insert_locn := method(locn, tokens,
# )
#                 tokens insert(0, locn)
#             print(OneOrMore(Word(nums)).addParseAction(insert_locn).parseString("0 123 321")) # -> [0, '0', '123', '321']
#         """
#         self __toklist.insert(index, insStr)
#         # fixup indices in token dictionary
#         for name,occurrences in self __tokdict.items(),
#             for k, (value, position) in enumerate(occurrences),
#                 occurrences[k] := _ParseResultsWithOffset(value, position + (position > index))

#     append := method( item ,
# )
#         """
#         Add single element to end of ParseResults list of elements.

#         Example:,
#             print(OneOrMore(Word(nums)).parseString("0 123 321")) # -> ['0', '123', '321']

#             # use a parse action to compute the sum of the parsed integers, and add it to the end
#             append_sum := method(tokens,
#                 tokens append(sum(map(int, tokens)))
# )
#             print(OneOrMore(Word(nums)).addParseAction(append_sum).parseString("0 123 321")) # -> ['0', '123', '321', 444]
#         """
#         self __toklist.append(item)

#     extend := method( itemseq ,
# )
#         """
#         Add sequence of elements to end of ParseResults list of elements.

#         Example:,
#             patt := OneOrMore(Word(alphas))

#             # use a parse action to append the reverse of the matched strings, to make a palindrome
#             make_palindrome := method(tokens,
# )
#                 tokens extend(reversed([t[::-1] for t in tokens]))
#                return  ''.join(tokens)
#             print(patt.addParseAction(make_palindrome).parseString("lskdj sdlkjf lksd")) # -> 'lskdjsdlkjflksddsklfjkldsjdksl'
#         """
#         if itemseq isKindOf( ParseResults),
#             self += itemseq
#         else,
#             self __toklist.extend(itemseq)

#     clear := method(
#         """
#         Clear all elements and results names.
#         """
#         del self __toklist[:]
#         self __tokdict.clear()
# )

#     __getattr__ := method( name ,
# )
#         try,
#            return  self[name]
#         except KeyError,
#            return  ""

#         if name in self __tokdict,
#             if name not in self __accumNames,
#                return  self __tokdict[name][-1][0]
#             else,
#                return  ParseResults([ v[0] for v in self __tokdict[name] ])
#         else,
#            return  ""

#     __add__ := method( other ,
# )
#         ret := self copy()
#         ret += other
#        return  ret

    # extend := method(other,
    #     if(other ?__tokdict,
    #         offset := len(self.__toklist)
    #         addoffset := lambda a: offset if a<0 else a+offset
    #         otheritems := other __tokdict.items()
    #         otherdictitems := [(k, _ParseResultsWithOffset(v[0],addoffset(v[1])) ) for (k,vlist) in otheritems for v in vlist]
    #         for k,v in otherdictitems,
    #             self[k] := v
    #             if isinstance(v[0],ParseResults),
    #                 v[0].__parent := wkref(self)
    #     )
    #     return  self
    # )

#     __radd__ := method( other,
# )
#         if other isKindOf(int) and other == 0,
#             # useful for merging many ParseResults using sum() builtin
#            return  self copy()
#         else,
#             # this may  a TypeError  raise- so be it
#            return  other + self

#     __repr__ := method(
# )
#        return  "(%s, %s)" % ( repr( self __toklist ), repr( self __tokdict ) )

#     asString := method(
# )
#        return  '[' + ', '.join(_ustr(i) if i isKindOf( ParseResults) else repr(i) for i in self __toklist) + ']'

#     _asStringList := method( sep='' ,
# )
#         out := []
#         for item in self __toklist,
#             if out and sep,
#                 out append(sep)
#             if  item isKindOf( ParseResults ),
#                 out += item _asStringList()
#             else,
#                 out append( _ustr(item) )
#        return  out

#     asList := method(
# )
#         """
#        return s the parse results as a nested list of matching tokens, all converted to strings.

#         Example:,
#             patt := OneOrMore(Word(alphas))
#             result := patt parseString("sldkj lsdkj sldkj")
#             # even though the result prints in string-like form, it is actually a pyparsing ParseResults
#             print(type(result), result) # -> <class 'pyparsing.ParseResults'> ['sldkj', 'lsdkj', 'sldkj']

#             # Use asList() to create an actual list
#             result_list := result asList()
#             print(type(result_list), result_list) # -> <class 'list'> ['sldkj', 'lsdkj', 'sldkj']
#         """
#        return  [res.asList() if res isKindOf(ParseResults) else res for res in self __toklist]

#     asDict := method(
# )
#         """
#        return s the named parse results as a nested dictionary.

#         Example:,
#             integer := Word(nums)
#             date_str := integer("year") + '/' + integer("month") + '/' + integer("day")

#             result := date_str.parseString('12/31/1999')
#             print(type(result), repr(result)) # -> <class 'pyparsing.ParseResults'> (['12', '/', '31', '/', '1999'], {'day': [('1999', 4)], 'year': [('12', 0)], 'month': [('31', 2)]})

#             result_dict := result asDict()
#             print(type(result_dict), repr(result_dict)) # -> <class 'dict'> {'day': '1999', 'year': '12', 'month': '31'}

#             # even though a ParseResults supports dict-like access, sometime you just need to have a dict
#             import json
#             print(json.dumps(result)) # -> Exception: TypeError: ... is not JSON serializable
#             print(json.dumps(result.asDict())) # -> {"month": "31", "day": "1999", "year": "12"}
#         """
#         if PY_3,
#             item_fn := self items
#         else,
#             item_fn := self iteritems

#         toItem := method(obj,
# )
#             if obj isKindOf( ParseResults),
#                 if obj haskeys(),
#                    return  obj asDict()
#                 else,
#                    return  [toItem(v) for v in obj]
#             else,
#                return  obj

#        return  dict((k,toItem(v)) for k,v in item_fn())

#     copy := method(
# )
#         """
#        return s a new copy of a C{ParseResults} object.
#         """
#         ret := ParseResults( self __toklist )
#         ret __tokdict := self __tokdict.copy()
#         ret __parent := self __parent
#         ret __accumNames.update( self __accumNames )
#         ret __name := self __name
#        return  ret

#     __lookup := method(sub,
# )
#         for k,vlist in self __tokdict.items(),
#             for v,loc in vlist,
#                 if sub is v,
#                    return  k
#        return  nil

#     getName := method(
# )
#         """
#        return s the results name for this token expression. Useful when several
#         different expressions might match at a particular location.

#         Example:,
#             integer := Word(nums)
#             ssn_expr := RegexToken(r"\d\d\d-\d\d-\d\d\d\d")
#             house_number_expr := Suppress('#') + Word(nums, alphanums)
#             user_data := (Group(house_number_expr)("house_number")
#                         | Group(ssn_expr)("ssn")
#                         | Group(integer)("age"))
#             user_info := OneOrMore(user_data)

#             result := user_info.parseString("22 111-22-3333 #221B")
#             for item in result,
#                 print(item.getName(), ':', item[0])
#         prints:,
#             age : 22
#             ssn : 111-22-3333
#             house_number : 221B
#         """
#         if self __name,
#            return  self __name
#         elif self __parent,
#             par := self __parent()
#             if par,
#                return  par __lookup(self)
#             else,
#                return  nil
#         elif (len(self) == 1 and
#                len(self.__tokdict) == 1 and
#                next(iter(self.__tokdict.values()))[0][1] in (0,-1)),
#            return  next(iter(self.__tokdict.keys()))
#         else,
#            return  nil

#     dump := method( indent='', depth=0, full=True,
# )
#         """
#         Diagnostic method for listing out the contents of a C{ParseResults}.
#         Accepts an optional C{indent} argument so that this string can be embedded
#         in a nested display of other data.

#         Example:,
#             integer := Word(nums)
#             date_str := integer("year") + '/' + integer("month") + '/' + integer("day")

#             result := date_str.parseString('12/31/1999')
#             print(result.dump())
#         prints:,
#             ['12', '/', '31', '/', '1999']
#             - day: 1999
#             - month: 31
#             - year: 12
#         """
#         out := []
#         NL := '\n'
#         out append( indent+_ustr(self.asList()) )
#         if full,
#             if self haskeys(),
#                 items := sorted((str(k), v) for k,v in self items())
#                 for k,v in items,
#                     if out,
#                         out append(NL)
#                     out append( "%s%s- %s: " % (indent,('  '*depth), k) )
#                     if v isKindOf(ParseResults),
#                         if v,
#                             out append( v dump(indent,depth+1) )
#                         else,
#                             out append(_ustr(v))
#                     else,
#                         out append(repr(v))
#             elif any(vv isKindOf(ParseResults) for vv in self),
#                 v := self
#                 for i,vv in enumerate(v),
#                     if vv isKindOf(ParseResults),
#                         out append("\n%s%s[%d]:\n%s%s%s" % (indent,('  '*(depth)),i,indent,('  '*(depth+1)),vv.dump(indent,depth+1) ))
#                     else,
#                         out append("\n%s%s[%d]:\n%s%s%s" % (indent,('  '*(depth)),i,indent,('  '*(depth+1)),_ustr(vv)))

#        return  "".join(out)

#     pprint := method( *args, **kwargs,
# )
#         """
#         Pretty-printer for parsed results as a list, using the C{pprint} module.
#         Accepts additional positional or keyword args as defined for the
#         C{pprint.pprint} method. (U{http://docs.python.org/3/library/pprint.html#pprint.pprint})

#         Example:,
#             ident := Word(alphas, alphanums)
#             num := Word(nums)
#             func := Forward()
#             term := ident | num | Group('(' + func + ')')
#             func <<= ident + Group(Optional(delimitedList(term)))
#             result := func parseString("fna a,b,(fnb c,d,200),100")
#             result pprint(width=40)
#         prints:,
#             ['fna',
#              ['a',
#               'b',
#               ['(', 'fnb', ['c', 'd', '200'], ')'],
#               '100']]
#         """
#         pprint pprint(self.asList(), *args, **kwargs)

#     # add support for pickle protocol
#     __getstate__ := method(
# )
#        return  ( self __toklist,
#                  ( self __tokdict.copy(),
#                    self __parent isNil not and self __parent() or nil,
#                    self __accumNames,
#                    self __name ) )

#     __setstate__ := method(state,
# )
#         self __toklist := state[0]
#         (self.__tokdict,
#          par,
#          inAccumNames,
#          self __name) := state[1]
#         self __accumNames := {}
#         self __accumNames.update(inAccumNames)
#         if par isNil not,
#             self __parent := wkref(par)
#         else,
#             self __parent := nil

#     __getnewargs__ := method(
# )
#        return  self __toklist, self __name, self __asList, self __modal

#     __dir__ := method(
# )
#        return  (dir(type(self)) + list(self.keys()))
