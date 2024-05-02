-- lexit.lua
-- Spencer Baysinger
-- Started: 2024-02-17
-- Forth Secret Code: I am one with the Forth. The Forth is with me.
-- For CS 331 Spring 2024

-- History:
-- - v1:
--   - Framework written. Lexer treats every character as punctuation.
-- - v2:
--   - Add state LETTER, with handler. Write skipToNextLexeme. Add
--     comment on invariants.
-- - v3
--   - Finished (hopefully). Add states DIGIT, DIGDOT, DOT, PLUS, MINUS,
--     STAR. Comment each state-handler function. Check for MAL lexeme.
-- - v4
--   - Ownership taken over by Spencer Baysinger
--   - Program edited to Assignment 3 and PL Nilgai specifications
--      - Patched Keywords, Star, NUMLIT
--      - Added Keywords, String Literal, Operators
--      - All tests in lexit_test.lua are successful

-- Usage:
--
--    program = "print a+b;"  -- program to lex
--    for lexstr, cat in lexer.lex(program) do
--        -- lexstr is the string form of a lexeme.
--        -- cat is a number representing the lexeme category.
--        -- It can be used as an index for array lexer.catnames.
--    end

-- *********************************************************************
-- Module Table Initialization
-- *********************************************************************

local lexit = {}  -- Our module; members are added below

-- *********************************************************************
-- Public Constants
-- *********************************************************************

-- Numeric constants representing lexeme categories
lexit.KEY       = 1
    -- 16 Keywords: {"and", "char", "def", "else", "elseif", "eol", "false", "if", "inputnum", "not", "or", "output", "rand", "return", "true", "while"}
    -- Finished Keywords: 
lexit.ID        = 2
lexit.NUMLIT    = 3
    -- Any sequence of digits: {1, 123, 1444422}
    -- Along with exponent: {7e34, 34567E+1, 9876e0}
lexit.STRLIT    = 4
lexit.OP        = 5
    -- 14 Operators: {"==", "!=", "<", "<=", ">", ">=", "+", "-", "*", "/", "%", "[", "]", "="}
    -- Finished Operators: 
lexit.PUNCT     = 6
    -- Any single character that is not whitespace, part of a comment, and not part of a lexem in any of the other categories
    -- ex: ; { } ( ) , & $
lexit.MAL       = 7

-- catnames
-- Array of names of lexeme categories.
-- Human-readable strings. Indices are above numeric constants.
lexit.catnames = {
    "Keyword",
    "Identifier",
    "NumericLiteral",
    "StringLiteral",
    "Operator",
    "Punctuation",
    "Malformed"
}

-- *********************************************************************
-- Kind-of-Character Functions
-- *********************************************************************

-- All functions return false when given a string whose length is not
-- exactly 1.

-- isLetter
-- Returns true if string c is a letter character, false otherwise.
local function isLetter(c)
    if c:len() ~= 1 then
        return false
    elseif c >= "A" and c <= "Z" then
        return true
    elseif c >= "a" and c <= "z" then
        return true
    else
        return false
    end
end

-- isDigit
-- Returns true if string c is a digit character, false otherwise.
local function isDigit(c)
    if c:len() ~= 1 then
        return false
    elseif c >= "0" and c <= "9" then
        return true
    else
        return false
    end
end

-- isWhitespace
-- Returns true if string c is a whitespace character, false otherwise.
local function isWhitespace(c)
    if c:len() ~= 1 then
        return false
    elseif c == " " or c == "\t" or c == "\n" or c == "\r"
      or c == "\f" then
        return true
    else
        return false
    end
end

-- isPrintableASCII
-- Returns true if string c is a printable ASCII character (codes 32 " "
-- through 126 "~"), false otherwise.
local function isPrintableASCII(c)
    if c:len() ~= 1 then
        return false
    elseif c >= " " and c <= "~" then
        return true
    else
        return false
    end
end

-- isIllegal
-- Returns true if string c is an illegal character, false otherwise.
local function isIllegal(c)
    if c:len() ~= 1 then
        return false
    elseif isWhitespace(c) then
        return false
    elseif isPrintableASCII(c) then
        return false
    else
        return true
    end
end

-- *********************************************************************
-- The lexer
-- *********************************************************************

-- lex
-- Our lexer
-- Intended for use in a for-in loop:
--     for lexstr, cat in lexit.lex(program) do
-- Here, lexstr is the string form of a lexeme, and cat is a number
-- representing a lexeme category. (See Public Constants.)
function lexit.lex(program)
    -- ***** Variables (like class data members) *****
    local pos       -- Index of next character in program
                    -- INVARIANT: when getLexeme is called, pos is
                    --  EITHER the index of the first character of the
                    --  next lexeme OR program:len()+1
    local state     -- Current state for our state machine
    local ch        -- Current character
    local lexstr    -- The lexeme, so far
    local category  -- Category of lexeme, set when state set to DONE
    local handlers  -- Dispatch table; value created later

    -- ***** States *****
    local DONE              = 0
    local START             = 1
    local LETTER            = 2
    local DIGIT             = 3
    local SQUOTE            = 4
    local DQUOTE            = 5
    local EXPONENT          = 6
    local PLUS              = 7
    local MINUS             = 8
    local STAR              = 9
    local EQUAL             = 10
    local DOUBLEEQUAL       = 11
    local NOTEQUAL          = 12
    local LESSTHAN          = 13
    local LESSTHANEQUAL     = 14
    local GREATERTHAN       = 15
    local GREATERTHANEQUAL  = 16
    local MODULO            = 17
    local SQUAREBRACKETOPEN = 18

    -- ***** Character-Related Utility Functions *****
    -- currChar
    -- Return the current character, at index pos in program. Return
    -- value is a single-character string, or the empty string if pos is
    -- past the end.
    local function currChar()
        return program:sub(pos, pos)
    end

    -- nextChar
    -- Return the next character, at index pos+1 in program. Return
    -- value is a single-character string, or the empty string if pos+1
    -- is past the end.
    local function nextChar()
        return program:sub(pos+1, pos+1)
    end

    -- nextnextChar
    -- Return the next character, at index pos+2 in program. Return
    -- value is a single-character string, or the empty string if pos+1
    -- is past the end.
    local function nextnextChar()
        return program:sub(pos+2, pos+2)
    end

    -- drop1
    -- Move pos to the next character.
    local function drop1()
        pos = pos+1
    end

    -- add1
    -- Add the current character to the lexeme, moving pos to the next
    -- character.
    local function add1()
        lexstr = lexstr .. currChar()
        drop1()
    end

    -- skipToNextLexeme
    -- Skip whitespace and comments, moving pos to the beginning of
    -- the next lexeme, or to program:len()+1.
    local function skipToNextLexeme()
        while true do
            -- Skip whitespace characters
            while isWhitespace(currChar()) do
                drop1()
            end

            -- Done if no comment
            if currChar() ~= "#" then
                break
            end

            -- Skip comment
            drop1()  -- Drop leading "#"
            while true do
                if currChar() == "\n" then
                    drop1()  -- Move past Newline
                    break
                elseif currChar() == "" then  -- End of input
                   return
                else
                    drop1()  -- Drop character inside comment
                end
            end
        end
    end

    -- ***** State-Handler Functions *****
    -- A function with a name like handle_XYZ is the handler function for state XYZ
    -- State START: no character read yet.
    local function handle_START()
        -- State Malformed
        if isIllegal(ch) then
            add1()
            state = DONE
            category = lexit.MAL

        -- ***** Keyword, Identifier, String and Numeric Literal Handlers *****

        -- State String Literal
        elseif ch == "\"" or ch == "'" then
            add1()
            if ch == "'" then state = SQUOTE end
            if ch == "\"" then state = DQUOTE end
        -- State Identifier and Keyword
        elseif isLetter(ch) or ch == "_" then
            add1()
            state = LETTER
        -- State Numeric Literal
        elseif isDigit(ch) then
            add1()
            state = DIGIT

        -- ***** Operator Handlers *****
        
        -- Double Character Operators
        -- State Operator ==
        elseif ch == "=" and nextChar() == "=" then
            add1()
            add1()
            state = DOUBLEEQUAL
        -- State Operator !=
        elseif ch == "!" and nextChar() == "=" then
            add1()
            add1()
            state = NOTEQUAL
        -- State Operator <=
        elseif ch == "<" and nextChar() == "=" then
            add1()
            add1()
            state = LESSTHANEQUAL
        -- State Operator >=
        elseif ch == ">" and nextChar() == "=" then
            add1()
            add1()
            state = GREATERTHANEQUAL

        -- Single Character Operators

        -- State Operator +
        elseif ch == "+" then
            add1()
            state = PLUS
        -- State Operator -
        elseif ch == "-" then
            add1()
            state = MINUS
        -- State Operator * and / 
        elseif ch == "*" or ch == "/" then
            add1()
            state = STAR
        -- State Operator =
        elseif ch == "=" then
            add1()
            state = EQUAL
        -- State Operator <
        elseif ch == "<" then
            add1()
            state = LESSTHAN
        -- State Operator >
        elseif ch == ">" then
            add1()
            state = GREATERTHAN
        -- State Operator %
        elseif ch == "%" then
            add1()
            state = MODULO
        -- State Operator [ 
        elseif ch == "[" or ch == "]" then
            add1()
            state = SQUAREBRACKETOPEN

        -- Punctuation Handler
        else
            add1()
            state = DONE
            category = lexit.PUNCT
        end
    end

    -- Identifier or Keyword State LETTER: we are in an ID.
    local function handle_LETTER()
        if isLetter(ch) or isDigit(ch) or ch == "_" then
            add1()
        else
            state = DONE
            -- 16 Keywords: {"and", "char", "def", "else", "elseif", "eol", "false", "if", "inputnum", "not", "or", "output", "rand", "return", "true", "while"}
            if  lexstr == "and" or lexstr == "char" or lexstr == "def" or
                lexstr == "else" or lexstr == "elseif" or lexstr == "eol" or
                lexstr == "false" or lexstr == "if" or lexstr == "inputnum" or
                lexstr == "not" or lexstr == "or" or lexstr == "output" or
                lexstr == "rand" or lexstr == "return" or lexstr == "true" or
                lexstr == "while" then
                category = lexit.KEY
            else
                category = lexit.ID
            end
        end
    end

    -- Numeric Literal State DIGIT: we are in a NUMLIT.
    local function handle_DIGIT()
        if isDigit(ch) then
            add1()
        elseif ch == "e" or ch == "E" then
            if isDigit(nextChar()) then
                add1()
                state = EXPONENT
                category = lexit.NUMLIT
            elseif nextChar() == "+" and isDigit(nextnextChar()) then
                add1()
                add1()
                state = EXPONENT
            else
                category = lexit.NUMLIT
                state = DONE
            end
        else
            state = DONE
            category = lexit.NUMLIT
        end
    end

    -- Numeric Literal State EXPONENT: we have seen a NUMLIT with an ("e" or "E").
    local function handle_EXPONENT()
        if isDigit(ch) then
            add1()
        else
            state = DONE
            category = lexit.NUMLIT
        end
    end

    -- State String Literal SQUOTE: we have seen a single quote (')
    local function handle_SQUOTE()  -- Handle '
        if ch ~= "'" and ch ~= "" and ch ~= "\n" then
            add1()
        elseif ch == "'" then
            add1()
            state = DONE
            category = lexit.STRLIT
        elseif ch == "" or ch == "\n" then
            -- drop1()
            state = DONE
            category = lexit.MAL
        end
    end

    -- String Literal State DQUOTE: we have seen a single quote (")
    local function handle_DQUOTE()  -- Handle "
        if ch ~= "\"" and ch ~= "" and ch ~= "\n" then
            add1()
        elseif ch == "\"" then
            add1()
            state = DONE
            category = lexit.STRLIT
        elseif ch == "" or ch == "\n" then
            -- drop1()
            state = DONE
            category = lexit.MAL
        end
    end


    -- Operator State PLUS: we have seen a plus ("+") and nothing else.
    local function handle_PLUS()
        state = DONE
        category = lexit.OP
    end
    -- Operator State MINUS: we have seen a minus ("-") and nothing else.
    local function handle_MINUS()
        state = DONE
        category = lexit.OP
    end
    -- Operator State STAR: we have seen a star ("*"), slash ("/"), or equal
    -- ("=") and nothing else.
    local function handle_STAR()  -- Handle * or / or =
        state = DONE
        category = lexit.OP
    end
    -- Operator State EQUAL : we have seen a =
    local function handle_EQUAL()
        state = DONE
        category = lexit.OP
    end
    -- Operator State NOTEQUAL : we have seen a !=
    local function handle_NOTEQUAL()
        state = DONE
        category = lexit.OP
    end
    -- Operator State DOUBLEEQUAL : we have seen a ==
    local function handle_DOUBLEEQUAL()
        state = DONE
        category = lexit.OP
    end
    -- Operator State LESSTHAN: we have seen a <
    local function handle_LESSTHAN()
        state = DONE
        category = lexit.OP
    end
    -- Operator State LESSTHANEQUAL: we have seen a <=
    local function handle_LESSTHANEQUAL()
        state = DONE
        category = lexit.OP
    end
    -- Operator State GREATERTHANEQUAL: we have seen a >=
    local function handle_GREATERTHANEQUAL()
        state = DONE
        category = lexit.OP
    end
    -- Operator State GREATERTHAN: we have seen a >
    local function handle_GREATERTHAN()
        state = DONE
        category = lexit.OP
    end
    -- Operator State MODULO: we have seen a %
    local function handle_MODULO()
        state = DONE
        category = lexit.OP
    end
    -- Operator State SQUAREBRACKETOPEN: we have seen a [
    local function handle_SQUAREBRACKETOPEN()
        state = DONE
        category = lexit.OP
    end

    -- State DONE: lexeme is done; this handler should not be called.
    local function handle_DONE()
        error("'DONE' state should not be handled\n")
    end

    -- ***** Table of State-Handler Functions *****
    handlers = {
        [DONE]=handle_DONE,
        [START]=handle_START,
        [LETTER]=handle_LETTER,
        [DIGIT]=handle_DIGIT,
        [EXPONENT]=handle_EXPONENT,
        [SQUOTE]=handle_SQUOTE,
        [DQUOTE]=handle_DQUOTE,
        [PLUS]=handle_PLUS,
        [MINUS]=handle_MINUS,
        [STAR]=handle_STAR,
        [EQUAL]=handle_EQUAL,
        [DOUBLEEQUAL]=handle_DOUBLEEQUAL,
        [NOTEQUAL]=handle_NOTEQUAL,
        [LESSTHAN]=handle_LESSTHAN,
        [LESSTHANEQUAL]=handle_LESSTHANEQUAL,
        [GREATERTHAN]=handle_GREATERTHAN,
        [GREATERTHANEQUAL]=handle_GREATERTHANEQUAL,
        [MODULO]=handle_MODULO,
        [SQUAREBRACKETOPEN]=handle_SQUAREBRACKETOPEN,
    }

    -- ***** Iterator Function *****

    -- getLexeme
    -- Called each time through the for-in loop.
    -- Returns a pair: lexeme-string (string) and category (int), or
    -- nil, nil if no more lexemes.
    local function getLexeme()
        if pos > program:len() then
            return nil, nil
        end
        lexstr = ""
        state = START
        while state ~= DONE do
            ch = currChar()
            handlers[state]()
        end

        skipToNextLexeme()
        return lexstr, category
    end

    -- ***** Body of Function lex *****

    -- Initialize & return the iterator function
    pos = 1
    skipToNextLexeme()
    return getLexeme
end

-- *********************************************************************
-- Module Table Return
-- *********************************************************************

return lexit

