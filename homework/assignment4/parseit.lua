-- parseit.lua  
-- Spencer Baysinger
-- 2024-02-20
--
-- For CS 331 Spring 2024
-- Solution to Assignment 4, Exercise A
-- Requires lexit.lua

-- File based on parseit.lua (skeleton) written by Professor Glenn Chappell

-- Grammar & AST specification
    -- 1.  	program	        ->  { statement }
    -- 2.  	statement       ->  ‘;’
    -- 3.  	 	            |  	    ‘output’ ‘(’ [ output_arg { ‘,’ output_arg } ] ‘)’ ‘;’
    -- 4.  	 	            |  	    ‘return’ expr ‘;’
    -- 5.  	 	            |  	    ID ( ‘(’ ‘)’ | [ ‘[’ expr ‘]’ ] ‘=’ expr ) ‘;’
    -- 6.  	 	            |  	    ‘def’ ID ‘(’ ‘)’ ‘{’ program ‘}’
    -- 7.  	 	            |   	‘if’ ‘(’ expr ‘)’ ‘{’ program ‘}’ { ‘elseif’ ‘(’ expr ‘)’ ‘{’ program ‘}’ } [ ‘else’ ‘{’ program ‘}’ ]
    -- 8.  	 	            |  	    ‘while’ ‘(’ expr ‘)’ ‘{’ program ‘}’
    -- 9.  	output_arg	    ->   STRLIT
    -- 10.  	 	        |  	    ‘eol’
    -- 11.  	 	        |  	    ‘char’ ‘(’ expr ‘)’
    -- 12.  	 	        |   	expr
    -- 13.  expr	        ->  compare_expr { ( ‘and’ | ‘or’ ) compare_expr }
    -- 14.  compare_expr    ->  arith_expr { ( ‘==’ | ‘!=’ | ‘<’ | ‘<=’ | ‘>’ | ‘>=’ ) arith_expr }
    -- 15.  arith_expr	    ->  term { ( ‘+’ | ‘–’ ) term }
    -- 16.  term	        ->  factor { ( ‘*’ | ‘/’ | ‘%’ ) factor }
    -- 17.  factor	        ->  NUMLIT
    -- 18.  	 	        |  	    ‘(’ expr ‘)’
    -- 19.  	 	        |   	( ‘true’ | ‘false’ )
    -- 20.  	 	        |   	( ‘+’ | ‘-’ | ‘not’ ) factor
    -- 21.  	 	        |   	‘inputnum’ ‘(’ ‘)’
    -- 22.  	 	        |   	‘rand’ ‘(’ expr ‘)’
    -- 23.  	 	        |   	ID [ ‘(’ ‘)’ | ‘[’ expr ‘]’ ]

local lexit = require "lexit"


-- *********************************************************************
-- Module Table Initialization
-- *********************************************************************


local parseit = {}  -- Our module


-- *********************************************************************
-- Variables
-- *********************************************************************


-- For lexit iteration
local iter          -- Iterator returned by lexit.lex
local state         -- State for above iterator (maybe not used)
local lexit_out_s   -- Return value #1 from above iterator
local lexit_out_c   -- Return value #2 from above iterator

-- For current lexeme
local lexcat = 0    -- Category of current lexeme:
                    --  one of categories below, or 0 for past the end

-- For last matched lexeme
local lexstr = ""   -- String form of current lexeme
local matched = ""  -- String form of last matched lexeme


-- *********************************************************************
-- Symbolic Constants for AST
-- *********************************************************************


local PROGRAM      = 1
local EMPTY_STMT   = 2
local OUTPUT_STMT  = 3
local RETURN_STMT  = 4
local ASSN_STMT    = 5
local FUNC_CALL    = 6
local FUNC_DEF     = 7
local IF_STMT      = 8
local WHILE_LOOP   = 9
local STRLIT_OUT   = 10
local EOL_OUT      = 11
local CHAR_CALL    = 12
local BIN_OP       = 13
local UN_OP        = 14
local NUMLIT_VAL   = 15
local BOOLLIT_VAL  = 16
local INPUT_CALL   = 17
local RAND_CALL    = 18
local SIMPLE_VAR   = 19
local ARRAY_VAR    = 20


-- *********************************************************************
-- Utility Functions
-- *********************************************************************


-- advance
-- Go to next lexeme and load it into lexstr, lexcat.
-- Should be called once before any parsing is done.
-- Function init must be called before this function is called.
local function advance()
    -- Advance the iterator
    lexit_out_s, lexit_out_c = iter(state, lexit_out_s)

    -- If we're not past the end, copy current lexeme into vars
    if lexit_out_s ~= nil then
        lexstr, lexcat = lexit_out_s, lexit_out_c
    else
        lexstr, lexcat = "", 0
    end
end


-- init
-- Initial call. Sets input for parsing functions.
local function init(prog)
    iter, state, lexit_out_s = lexit.lex(prog)
    advance()
end


-- atEnd
-- Return true if pos has reached end of input.
-- Function init must be called before this function is called.
local function atEnd()
    return lexcat == 0
end


-- matchString
-- Given string, see if current lexeme string form is equal to it. If
-- so, then set "matched" to the matched string, advance to next lexeme
-- & return true. If not, then do not advance, return false.
-- Function init must be called before this function is called.
local function matchString(s)
    if lexstr == s then
        matched = lexstr
        advance()
        return true
    else
        return false
    end
end


-- matchCat
-- Given lexeme category (integer), see if current lexeme category is
-- equal to it. If so, then set "matched" to the string form of the
-- matched lexeme, advance to next lexeme & return true. If not, then do
-- not advance, return false.
-- Function init must be called before this function is called.
local function matchCat(c)
    if lexcat == c then
        matched = lexstr
        advance()
        return true
    else
        return false
    end
end


-- *********************************************************************
-- "local" Statements for Parsing Functions
-- *********************************************************************


local parse_program
local parse_statement
local parse_output_arg
local parse_expr
local parse_compare_expr
local parse_arith_expr
local parse_term
local parse_factor


-- *********************************************************************
-- The Parser: Function "parse" - EXPORTED
-- *********************************************************************


-- parse
-- Given program, initialize parser and call parsing function for start
-- symbol. Returns pair of booleans & AST. First boolean indicates
-- successful parse or not. Second boolean indicates whether the parser
-- reached the end of the input or not. AST is only valid if first
-- boolean is true.
function parseit.parse(prog)
    -- Initialization
    init(prog)

    -- Get results from parsing
    local good, ast = parse_program()  -- Parse start symbol
    local done = atEnd()

    -- And return them
    return good, done, ast
end


-- *********************************************************************
-- Parsing Functions
-- *********************************************************************


-- Each of the following is a parsing function for a nonterminal in the
-- grammar. Each function parses the nonterminal in its name and returns
-- a pair: boolean, AST. On a successul parse, the boolean is true, the
-- AST is valid, and the current lexeme is just past the end of the
-- string the nonterminal expanded into. Otherwise, the boolean is
-- false, the AST is not valid, and no guarantees are made about the
-- current lexeme. See the AST Specification in the Assignment 4
-- description for the format of the returned AST.

-- NOTE. Declare parsing functions "local" above, but not below. This
-- allows them to be called before their definitions.


-- parse_program
-- Parsing function for nonterminal "program".
-- Function init must be called before this function is called.
function parse_program()
    local good, ast, ast2

    ast = { PROGRAM }
    while lexstr == ";"
          or lexstr == "output"
          or lexstr == "return"
          or lexcat == lexit.ID
          or lexstr == "def"
          or lexstr == "if"
          or lexstr == "while" do
        good, ast2 = parse_statement()
        if not good then
            return false, nil
        end

        table.insert(ast, ast2)
    end

    return true, ast
end


-- parse_statement
-- Parsing function for nonterminal "statement".
-- Function init must be called before this function is called.
function parse_statement()
    local good, ast1, ast2, saveid

    -- | ';'
    if matchString(";") then
        return true, { EMPTY_STMT }

    -- | ‘output’ ‘(’ [ output_arg { ‘,’ output_arg } ] ‘)’ ‘;’
    elseif matchString("output") then
        if not matchString("(") then
            return false, nil
        end

        if matchString(")") then
            if not matchString(";") then
                return false, nil
            end

            return true, { OUTPUT_STMT }
        end

        good, ast2 = parse_output_arg()
        if not good then
            return false, nil
        end

        ast1 = { OUTPUT_STMT, ast2 }

        while matchString(",") do
            good, ast2 = parse_output_arg()
            if not good then
                return false, nil
            end

            table.insert(ast1, ast2)
        end

        if not matchString(")") then
            return false, nil
        end

        if not matchString(";") then
            return false, nil
        end

        return true, ast1

    -- | ‘return’ expr ‘;’
    elseif matchString("return") then
        good, ast1 = parse_expr()
        if not good then
            return false, nil
        end

        if not matchString(";") then
            return false, nil
        end

        return true, { RETURN_STMT, ast1 }

    -- ID ( ‘(’ ‘)’ | [ ‘[’ expr ‘]’ ] ‘=’ expr ) ‘;’
    elseif matchCat(lexit.ID) then
        -- TODO: DONE
        saveid = matched

        if matchString("(") then
            if not matchString(")") then
                return false, nil
            end

            if not matchString(";") then
                return false, nil
            end

            return true, { FUNC_CALL, saveid }

        elseif matchString("[") then
            good, ast1 = parse_expr()
            if not good or not matchString("]") then
                return false, nil
            end
            
            if not matchString("=") then
                return false, nil
            end

            good, ast2 = parse_expr()
            if not good then
                return false, nil
            end

            if not matchString(";") then
                return false, nil
            end

            return true, { FUNC_DEF, saveid, ast1, ast2 }

        elseif matchString("=") then
            good, ast1 = parse_expr()
            if not good then
                return false, nil
            end
            
            if not matchString(";") then
                return false, nil
            end

            return true, { ASSN_STMT, {SIMPLE_VAR, saveid}, ast1 }
        else
            return false, nil
        end

    elseif matchString("def") then
        if not matchCat(lexit.ID) then
            return false, nil
        end
        saveid = matched

        if not matchString("(") then
            return false, nil
        end
        if not matchString(")") then
            return false, nil
        end

        if not matchString("{") then
            return false, nil
        end

        good, ast1 = parse_program()
        if not good then
            return false, nil
        end

        if not matchString("}") then
            return false, nil
        end

        return true, { FUNC_DEF, saveid, ast1 }

    -- | ‘if’ ‘(’ expr ‘)’ ‘{’ program ‘}’ { ‘elseif’ ‘(’ expr ‘)’ ‘{’ program ‘}’ } [ ‘else’ ‘{’ program ‘}’ ]
    elseif matchString("if") then
        -- TODO: DONE
        if not matchString("(") then
            return false, nil
        end

        good, ast1 = parse_expr()
        if not good then
            return false, nil
        end

        if not matchString(")") then
            return false, nil
        end

        if not matchString("{") then
            return false, nil
        end

        good, ast2 = parse_program()
        if not good then
            return false, nil
        end

        if not matchString("}") then
            return false, nil
        end

        -- local if_ast = {IF_STMT, ast1, ast2}
        local if_ast = {IF_STMT, ast1, ast2}

        while matchString("elseif") do
            if not matchString("(") then
                return false, nil
            end

            good, ast1 = parse_expr()
            if not good then
                return false, nil
            end

            if not matchString(")") then
                return false, nil
            end

            if not matchString("{") then
                return false, nil
            end

            good, ast2 = parse_program()
            if not good then
                return false, nil
            end

            if not matchString("}") then
                return false, nil
            end

            table.insert(if_ast, ast1)
            table.insert(if_ast, ast2)
        end

        if matchString("else") then
            if not matchString("{") then
                return false, nil
            end

            good, ast1 = parse_program()
            if not good then
                return false, nil
            end

            if not matchString("}") then
                return false, nil
            end

            table.insert(if_ast, ast1)
        end

        return true, if_ast

    -- | ‘while’ ‘(’ expr ‘)’ ‘{’ program ‘}’
    elseif matchString("while") then
        -- TODO: DONE
            if not matchString("(") then
                return false, nil
            end

            good, ast1 = parse_expr()
            if not good then
                return false, nil
            end

            if not matchString(")") then
                return false, nil
            end

            if not matchString("{") then
                return false, nil
            end

            good, ast2 = parse_program()
            if not good then
                return false, nil
            end

            if not matchString("}") then
                return false, nil
            end

            return true, {WHILE_LOOP, ast1, ast2}
    else
        -- ???: Not sure what to do here
        -- This might be right
        return false, nil
    end
end


-- parse_output_arg
-- Parsing function for nonterminal "output_arg".
-- Function init must be called before this function is called.
function parse_output_arg()
    -- TODO: DONE   
    local good, ast1

    if matchCat(lexit.STRLIT) then
        return true, { STRLIT_OUT, matched }

    elseif matchString("eol") then
        return true, { EOL_OUT }

    elseif matchString("char") then
        if not matchString("(") then
            return false, nil
        end

        good, ast1 = parse_expr()
        if not good then
            return false, nil
        end

        if not matchString(")") then
            return false, nil
        end

        return true, {CHAR_CALL, ast1}
    else
        good, ast1 = parse_expr()
        if not good then
            return false, nil
        end

        return true, ast
    end
end


-- parse_expr
-- Parsing function for nonterminal "expr".
-- Function init must be called before this function is called.
-- →  	compare_expr { ( ‘and’ | ‘or’ ) compare_expr }
function parse_expr()
    -- TODO: Done
    local good, ast1 = parse_compare_expr()
    local ast2
    if not good then
        return false, nil
    end


    while matchString("and") or matchString("or") do
        local op = matched -- Capture binary operator

        good, ast2 = parse_compare_expr()
        if not good then
            return false, nil
        end

        ast1 = {{ BIN_OP, op}, ast1, ast2}
    end

    return true, ast1
end


-- parse_compare_expr
-- Parsing function for nonterminal "compare_expr".
-- Function init must be called before this function is called.
-- compare_expr	  →  	arith_expr { ( ‘==’ | ‘!=’ | ‘<’ | ‘<=’ | ‘>’ | ‘>=’ ) arith_expr }
function parse_compare_expr()
    -- TODO: DONE
    local good, ast1 = parse_arith_expr()
    local ast2
    if not good then
        return false, nil
    end


    while matchString("==") or matchString("!=") 
        or matchString("<") or matchString("<=")
        or matchString(">") or matchString(">=") do
        local op = matched -- Capture arithmetic operator

        good, ast2 = parse_arith_expr()
        if not good then
            return false, nil
        end

        ast1 = {{ BIN_OP, op}, ast1, ast2}
    end

    return true, ast1
end


-- parse_arith_expr
-- Parsing function for nonterminal "arith_expr".
-- Function init must be called before this function is called.
-- arith_expr	  →  	term { ( ‘+’ | ‘–’ ) term }
function parse_arith_expr()
    -- TODO: DONE
    local good, ast1 = parse_term()
    local ast2
    if not good then
        return false, nil
    end


    while matchString("+") or matchString("-") do
        local op = matched -- Capture arithmetic operator

        good, ast2 = parse_term()
        if not good then
            return false, nil
        end

        ast1 = {{ NUMLIT_VAL, op}, ast1, ast2}
    end

    return true, ast1
end


-- parse_term
-- Parsing function for nonterminal "term".
-- Function init must be called before this function is called.
-- term	  →  	factor { ( ‘*’ | ‘/’ | ‘%’ ) factor }
function parse_term()
    -- TODO: DONE
    local good, ast1 = parse_factor()
    local ast2
    if not good then
        return false, nil
    end


    while matchString("*") or matchString("/") or matchString("%") do
        local op = matched -- Capture arithmetic operator

        good, ast2 = parse_factor()
        if not good then
            return false, nil
        end

        ast1 = {{ NUMLIT_VAL, op}, ast1, ast2}
    end

    return true, ast1
end


-- parse_factor
-- Parsing function for nonterminal "factor".
-- Function init must be called before this function is called.
function parse_factor()
    -- TODO: WRITING THIS
    local good, ast1, ast2

    -- factor	  →  	NUMLIT
    if matchCat(lexit.NUMLIT) then
        ast1 = {NUMLIT_VAL, matched}

    -- ‘(’ expr ‘)’
    elseif matchString("(") then
        good, ast1 = parse_expr()
        if not good or matchString(")") then
            return false, nil
        end

    -- ( ‘true’ | ‘false’ )
    elseif matchString("true") or matchString("false") then
        ast1 = {BOOLLIT_VAL, matched}

        -- ( ‘+’ | ‘-’ | ‘not’ ) factor
    elseif matchString("+") or matchString("-") or matchString("not") then
        good, ast1 = parse_factor()
        if not good then
            return false, nil
        end

        ast1 = {UN_OP, matched}

        -- ‘inputnum’ ‘(’ ‘)’
    elseif matchString("inputnum") then
        if not matchString("(") then
            return false, nil
        end

        if not matchString(")") then
            return false, nil
        end

        ast1 = {INPUT_CALL}

        -- ‘rand’ ‘(’ expr ‘)’
    elseif matchString("rand") then
        if not matchString("(") then
            return false, nil
        end

        good, ast2 = parse_expr()
        if not good then
            return false, nil
        end

        if not matchString(")") then
            return false, nil
        end

        ast1 = {RAND_CALL, ast2}

    -- ID [ ‘(’ ‘)’ | ‘[’ expr ‘]’ ]
    elseif matchCat(lexit.ID) then
        local id = matchString()

        if matchString("(") then
           if not matchString(")") then
            return false, nil
           end

           return {FUNC_CALL, id}

        elseif matchString("[") then
            good, ast2 = parse_expr()
            if not good then
                return false, nil
            end

            if not matchString("]") then
                return false, nil
            end

            ast1 = {ARRAY_VAR, id, ast1}

        else
            ast1 = {SIMPLE_VAR, id}
        end
    else
        return false, nil
    end

    return true, ast1
end


-- *********************************************************************
-- Module Table Return
-- *********************************************************************


return parseit

