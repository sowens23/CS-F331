-- lexer.lua  UNFINISHED
-- VERSION 2
-- Glenn G. Chappell
-- Started: 2024-02-06
-- Updated: 2024-02-07
--
-- For CS 331 Spring 2024
-- In-Class Lexer Module

-- History:
-- - v1:
--   - Framework written. Lexer treats every character as punctuation.
-- - v2:
--   - Add state LETTER, with handler. Write skipToNextLexeme. Add
--     comment on invariants.

-- Usage:
--
--    program = "print a+b;"  -- program to lex
--    for lexstr, cat in lexer.lex(program) do
--        -- lexstr is the string form of a lexeme.
--        -- cat is a number representing the lexeme category.
--        --  It can be used as an index for array lexer.catnames.
--    end


-- *********************************************************************
-- Module Table Initialization
-- *********************************************************************


local lexer = {}  -- Our module; members are added below


-- *********************************************************************
-- Public Constants
-- *********************************************************************


-- Numeric constants representing lexeme categories
lexer.KEY    = 1
lexer.ID     = 2
lexer.NUMLIT = 3
lexer.OP     = 4
lexer.PUNCT  = 5
lexer.MAL    = 6


-- catnames
-- Array of names of lexeme categories.
-- Human-readable strings. Indices are above numeric constants.
lexer.catnames = {
    "Keyword",
    "Identifier",
    "NumericLiteral",
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
-- The Lexer
-- *********************************************************************


-- lex
-- Our lexer
-- Intended for use in a for-in loop:
--     for lexstr, cat in lexer.lex(program) do
-- Here, lexstr is the string form of a lexeme, and cat is a number
-- representing a lexeme category. (See Public Constants.)
function lexer.lex(program)
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

    local DONE   = 0
    local START  = 1
    local LETTER = 2

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
            if currChar() ~= "/" or nextChar() ~= "*" then
                break
            end

            -- Skip comment
            drop1()  -- Drop leading "/"
            drop1()  -- Drop leading "*"
            while true do
                if currChar() == "*" and nextChar() == "/" then
                    drop1()  -- Drop trailing "*"
                    drop1()  -- Drop trailing "/"
                    break
                elseif currChar() == "" then  -- End of input?
                   return
                end
                drop1()  -- Drop character inside comment
            end
        end
    end

    -- ***** State-Handler Functions *****

    -- A function with a name like handle_XYZ is the handler function
    -- for state XYZ

    -- State DONE: lexeme is done; this handler should not be called.
    local function handle_DONE()
        error("'DONE' state should not be handled\n")
    end

    -- State START: no character read yet.
    local function handle_START()
        if isLetter(ch) or ch == "_" then
            add1()
            state = LETTER
        else
            add1()
            state = DONE
            category = lexer.PUNCT
        end
    end

    -- State LETTER
    local function handle_LETTER()
        if isLetter(ch) or isDigit(ch) or ch == "_" then
            add1()
        else
            state = DONE
            if lexstr == "begin" or lexstr == "end"
              or lexstr == "print" then
                category = lexer.KEY
            else
                category = lexer.ID
            end
        end
    end

    -- ***** Table of State-Handler Functions *****

    handlers = {
        [DONE]=handle_DONE,
        [START]=handle_START,
        [LETTER]=handle_LETTER,
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


return lexer

