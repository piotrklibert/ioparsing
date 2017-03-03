# IoParsing - A port of great PyParsing library to Io.

## What?

PyParsing is a great PEG library. It allows one to create arbitrary grammars,
which integrate lexing, parsing and (if desired) interpretation. Personally, I'm
tempted to use PyParsing every time a regexp I write grows to be longer than a
few characters.

Most class, method and attribute names are the same as in the original code, I
only modified

## Why?

I'm writing a text-based game in Io where command processing an important part.
I want to give players maximum flexibility when issuing commands, so I need a
fairly robust parser for them. I don't want to recode the parser for every
command and I don't want to use regexes because these are not easily composable.

When prototyping the command handling I thought of using "something like"
PyParsing a lot of times. There was nothing of the sort, so I decided to make it
myself.
