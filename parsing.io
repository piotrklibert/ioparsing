TypeError      := Exception clone
ValueError     := Exception clone
IndexError     := Exception clone
AttributeError := Exception clone


_MAX_LEN   := 9999999999999999


alphas     := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
nums       := "0123456789"
hexnums    := nums .. "ABCDEFabcdef"
alphanums  := alphas .. nums
_bslash    := 92 asCharacter
whites     := "\t\n\r "
printables := "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!'#$%&\"()*+,-./:;<=>?@[\\]^_`{|}~"


ParseBaseException := Exception clone do(
    "base exception class for all parsing runtime exceptions"
    # Performance tuning: we construct a *lot* of these, so keep this
    # constructor as small and fast as possible
    with := method(pstr, loc, msg, elem,
        if(call message arguments size != 4,
            ValueError raise($"Expected 4 arguments to ParseBaseException `with`, got #{call evalArgs}.")
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

    _from_exception := method(pe,
        # internal factory method to simplify creating one type of ParseException
        # from another - avoids having with signature conflicts among subclasses
        self with(pe pstr, pe loc, pe msg, pe parserElement)
    )

    forward := method(
        # supported attributes by name are:
        #     - lineno - returns the line number of the exception text
        #     - col - returns the column number of the exception text
        #     - line - returns the line containing the exception text
        aname := call message name
        if(aname == "lineno",
            return _lineno(self loc, self pstr)
        )
        if(aname in(["col", "column"]),
            return _col(self loc, self pstr)
        )
        if(aname == "line",
            return _line(self loc, self pstr)
        )
        if(aname,
            AttributeError raise(aname)
        )
    )

    asString := method(
        out := nil
        ex := try(
            out := $"#{msg} (at char #{self loc}), (line:#{self lineno}, col:#{self column})"
        )
        if(ex isNil not,
            ex showStack
            out := "jeblo"
        )
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

    asString := method(
        $"RecursiveGrammarException: #{parseElementTrace}"
    )
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
    # Performance tuning: we construct a *lot* of these, so keep this
    # constructor as small and fast as possible
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
        cln
    )
    forward := method(
        # Scheduler currentCoroutine showStack
        resend
    )
    isEmpty := method(call delegateTo(self __toklist))
    appendSeq := method(other,
        results := if(res isKindOf(ParseResults), other __toklist, other)
        self __toklist appendSeq(results)
        self
    )
    append := method(res,
        self __toklist append(res)
        self
    )
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
        cpy parseAction := (self parseAction)
        cpy ignoreExprs := (self ignoreExprs)
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
        callPreParse ifNil(
            callPreParse := true
        )
        doActions ifNil(
            doActions := true
        )
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
            tokens,
            self resultsName,
            self saveAsList,
            self modalResults
        )

        if(self parseAction and (doActions or self callDuringTry),
            self parseAction foreach(fn,
                tokens := fn call(instring, tokensStart, retTokens)
                if(tokens,
                    retTokens := ParseResults with(
                        tokens,
                        self resultsName,
                        self saveAsList,
                        self modalResults
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
        return Or([self, other])
    )


    __and__ := method(other,
        if(other isKindOf(Sequence) not,
            other := ParserElement _literalStringClass with(other)
        )
        if(other isKindOf(ParserElement) not,
            debugWriteln("Cannot combine element #{other} with ParserElement")
            return nil
        )
        return Each([self, other])
    )


    ~ := method(
        return NotAny( self )
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
            other := Suppress(other)
        )

        if(other isKindOf(Suppress),
            if(other in(self ignoreExprs) not,
                self ignoreExprs append(other))
        ,
            self ignoreExprs append(Suppress(other copy))
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
        return self parseString(file_contents, parseAll)
    )

    == := method(other,
        if(other isKindOf(ParserElement),
            return (self uniqueId == other uniqueId) or \
                self slotNames detect(sn, self getSlot(sn) != other getSlot(sn))
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
        #     expr := Word(nums)
        #     assert expr.matches("100")
        ex := try(
            self parseString(testString, parseAll ifNilEval(true))
            return true
        )
        ex catch(ParseBaseException,
            return false
        )
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
                writeln("break1> ", e, e type, instring, "==", found)
                break
                writeln("break2> ", " shouldnt be here")
            )
            err := try(
                writeln("===2> ", e, e type, instring)
                found := e _parse(instring, loc, doActions)
                writeln("===3> ", found asBoolean)
            )
            err catch(ParseException,
                writeln("asdasdasd", e, err msg)
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

        writeln("==4> ", found)

        if(found isNil not,
            writeln("returning")
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
            # TODO: inefficient, needs fixing the Regex Addon
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


Regex := Token clone do(
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
            if(pattern isKindOf(Regex),
                cln re := pattern
                cln pattern :=  pattern pattern
                cln reString := pattern pattern
                cln flags := flags
            ,
                ValueError raise("
                    Regex may only be constructed with a string or a compiled
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


# QuotedString := Token clone do(
#     with := method( quoteChar, escChar=nil, escQuote=nil, multiline=False, unquoteResults=True, endQuoteChar=nil, convertWhitespaceEscapes=True,
# )
#         super(QuotedString,self).with()

#         # remove white space from quote chars - wont work anyway
#         quoteChar := quoteChar strip()
#         if not quoteChar,
#             warnings warn("quoteChar cannot be the empty string",SyntaxWarning,stacklevel=2)
#              SyntaxError raise()

#         if endQuoteChar isNill,
#             endQuoteChar := quoteChar
#         else,
#             endQuoteChar := endQuoteChar strip()
#             if not endQuoteChar,
#                 warnings warn("endQuoteChar cannot be the empty string",SyntaxWarning,stacklevel=2)
#                  SyntaxError raise()

#         self quoteChar := quoteChar
#         self quoteCharLen := len(quoteChar)
#         self firstQuoteChar := quoteChar[0]
#         self endQuoteChar := endQuoteChar
#         self endQuoteCharLen := len(endQuoteChar)
#         self escChar := escChar
#         self escQuote := escQuote
#         self unquoteResults := unquoteResults
#         self convertWhitespaceEscapes := convertWhitespaceEscapes

#         if multiline,
#             self flags := re MULTILINE | re DOTALL
#             self pattern := r'%s(?:[^%s%s]' % \
#                 ( re escape(self.quoteChar),
#                   _escapeRegexRangeChars(self.endQuoteChar[0]),
#                   (escChar isNil not and _escapeRegexRangeChars(escChar) or '') )
#         else,
#             self flags := 0
#             self pattern := r'%s(?:[^%s\n\r%s]' % \
#                 ( re escape(self.quoteChar),
#                   _escapeRegexRangeChars(self.endQuoteChar[0]),
#                   (escChar isNil not and _escapeRegexRangeChars(escChar) or '') )
#         if len(self.endQuoteChar) > 1,
#             self pattern += (
#                 '|(?:' + ')|(?:'.join("%s[^%s]" % (re.escape(self.endQuoteChar[:i]),
#                                                _escapeRegexRangeChars(self.endQuoteChar[i]))
#                                     for i in range(len(self.endQuoteChar)-1,0,-1)) + ')'
#                 )
#         if escQuote,
#             self pattern += (r'|(?:%s)' % re escape(escQuote))
#         if escChar,
#             self pattern += (r'|(?:%s.)' % re escape(escChar))
#             self escCharReplacePattern := re escape(self.escChar)+"(.)"
#         self pattern += (r')*%s' % re escape(self.endQuoteChar))

#         try,
#             self re := re compile(self.pattern, self flags)
#             self reString := self pattern
#         except sre_constants.error,
#             warnings warn("invalid pattern (%s) passed to Regex" % self pattern,
#                 SyntaxWarning, stacklevel=2)
#             raise

#         self name := _ustr(self)
#         self errmsg := "Expected " + self name
#         self mayIndexError := False
#         self mayReturnEmpty := True

#     parseImpl := method( instring, loc, doActions=True ,
# )
#         result := instring[loc] == self firstQuoteChar and self re.match(instring,loc) or nil
#         if not result,
#              ParseException raise(instring, loc, self errmsg, self)

#         loc := result end()
#         ret := result group()

#         if self unquoteResults,

#             # strip off quotes
#             ret := ret[self.quoteCharLen:-self.endQuoteCharLen]

#             if ret isKindOf(Sequence),
#                 # replace escaped whitespace
#                 if '\\' in ret and self convertWhitespaceEscapes,
#                     ws_map := {
#                         r'\t' : '\t',
#                         r'\n' : '\n',
#                         r'\f' : '\f',
#                         r'\r' : '\r',
#                     }
#                     for wslit,wschar in ws_map.items(),
#                         ret := ret replace(wslit, wschar)

#                 # replace escaped characters
#                 if self escChar,
#                     ret := re sub(self.escCharReplacePattern,"\g<1>",ret)

#                 # replace escaped quotes
#                 if self escQuote,
#                     ret := ret replace(self.escQuote, self endQuoteChar)

#        return  loc, ret

#     asString := method(
# )
#         try,
#            return  super(QuotedString,self).asString()
#         except Exception,
#             pass

#         if self strRepr isNill,
#             self strRepr := "quoted string, starting with %s ending with %s" % (self.quoteChar, self endQuoteChar)

#        return  self strRepr
