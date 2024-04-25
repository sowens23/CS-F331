-- interpit.lua
-- Spencer Baysinger
-- 2024-04-15

-- For CS 331 Spring 2024

-- Assignment 6, part A - Running a Prolog Program
    -- Secret message:
    -- What do you call a fallen tree that has lost its amateur status?
    -- true.

-- Bruce Smith helped me sort through my Binary/Unary functionality.

-- File based on interpit.lua (skeleton) written by Professor Glenn Chappell
    -- interpit.lua (SKELETON)
    -- Glenn G. Chappell
    -- Started: 2024-04-02
    -- Updated: 2024-04-03

-- Interpret AST from parseit.parse
-- Solution to Assignment 6, Exercise B
-- *** To run a Nilgai program, use nilgai.lua, which uses this file.

-- *********************************************************************
-- Module Table Initialization
-- *********************************************************************

local interpit = {}  -- Our module

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

-- numToInt
-- Given a number, return the number rounded toward zero.
local function numToInt(n)
    assert(type(n) == "number")

    if n >= 0 then
        return math.floor(n)
    else
        return math.ceil(n)
    end
end

-- strToNum
-- Given a string, attempt to interpret it as an integer. If this
-- succeeds, return the integer. Otherwise, return 0.
local function strToNum(s)
    assert(type(s) == "string")

    -- Try to do string -> number conversion; make protected call
    -- (pcall), so we can handle errors.
    local success, value = pcall(function() return tonumber(s) end)

    -- Return integer value, or 0 on error.
    if success and value ~= nil then
        return numToInt(value)
    else
        return 0
    end
end

-- numToStr
-- Given a number, return its string form.
local function numToStr(n)
    assert(type(n) == "number")

    return tostring(n)
end

-- boolToInt
-- Given a boolean, return 1 if it is true, 0 if it is false.
local function boolToInt(b)
    assert(type(b) == "boolean")

    if b then
        return 1
    else
        return 0
    end
end

-- astToStr
-- Given an AST, produce a string holding the AST in (roughly) Lua form,
-- with numbers replaced by names of symbolic constants used in parseit.
-- A table is assumed to represent an array.
-- See the Assignment 4 description for the AST Specification.
--
-- THIS FUNCTION IS INTENDED FOR USE IN DEBUGGING ONLY!
-- IT SHOULD NOT BE CALLED IN THE FINAL VERSION OF THE CODE.
function astToStr(x)
    local symbolNames = {
        "PROGRAM", "EMPTY_STMT", "OUTPUT_STMT", "RETURN_STMT",
        "ASSN_STMT", "FUNC_CALL", "FUNC_DEF", "IF_STMT", "WHILE_LOOP",
        "STRLIT_OUT", "EOL_OUT", "CHAR_CALL", "BIN_OP", "UN_OP",
        "NUMLIT_VAL", "BOOLLIT_VAL", "INPUT_CALL", "RAND_CALL",
        "SIMPLE_VAR", "ARRAY_VAR",
    }
    if type(x) == "number" then
        local name = symbolNames[x]
        if name == nil then
            return "<Unknown numerical constant: "..x..">"
        else
            return name
        end
    elseif type(x) == "string" then
        return '"'..x..'"'
    elseif type(x) == "boolean" then
        if x then
            return "true"
        else
            return "false"
        end
    elseif type(x) == "table" then
        local first = true
        local result = "{"
        for k = 1, #x do
            if not first then
                result = result .. ","
            end
            result = result .. astToStr(x[k])
            first = false
        end
        result = result .. "}"
        return result
    elseif type(x) == "nil" then
        return "nil"
    else
        return "<"..type(x)..">"
    end
end

-- Define a table for binary operations
local binaryoperations = {
    ["and"] = function(x, y) return (x ~= 0 and y ~= 0) and 1 or 0 end,
    ["or"]  = function(x, y) return (x ~= 0 or y ~= 0) and 1 or 0 end,
    ["=="]  = function(x, y) return (x == y) and 1 or 0 end,
    ["!="]  = function(x, y) return (x ~= y) and 1 or 0 end,
    ["<"]   = function(x, y) return (x < y) and 1 or 0 end,
    ["<="]  = function(x, y) return (x <= y) and 1 or 0 end,
    [">"]   = function(x, y) return (x > y) and 1 or 0 end,
    [">="]  = function(x, y) return (x >= y) and 1 or 0 end,
    ["+"]   = function(x, y) return x + y end,
    ["-"]   = function(x, y) return x - y end,
    ["*"]   = function(x, y) return x * y end,
    ["/"]   = function(x, y) return (y ~= 0) and numToInt(x / y) or 0 end,
    ["%"]   = function(x, y) return (y ~= 0) and numToInt(x % y) or 0 end,
}

-- Define a table for unary operations
local unaryOperations = {
    ["-"] = function(x) return -x end,
    ["+"] = function(x) return x end,
    ["not"] = function(x) return (x == 0) and 1 or 0 end,
}

-- *********************************************************************
-- Primary Function for Client Code
-- *********************************************************************

-- interp
-- Interpreter, given AST returned by parseit.parse.
-- Parameters:
--   ast        - AST constructed by parseit.parse
--   state      - Table holding Nilgai variables & functions
--              - AST for function xyz is in state.f["xyz"]
--              - Value of simple variable xyz is in state.v["xyz"]
--              - Value of array item xyz[42] is in state.a["xyz"][42]
--   util       - Table with 3 members, all functions:
--              - incall() inputs line, returns string with no newline
--              - outcall(str) outputs str with no added newline
--                  To print a newline, do outcall("\n")
--              - random(n), for an integer n, returns a pseudorandom
--                  integer from 0 to n-1, or 0 if n < 2.
-- Return Value:
--   state, updated with changed variable values
function interpit.interp(ast, state, util)
    -- Each local interpretation function is given the AST for the
    -- portion of the code it is interpreting. The function-wide
    -- versions of state and until may be used. The function-wide
    -- version of state may be modified as appropriate.

    -- Forward declare local functions
    local interp_program
    local interp_stmt
    local eval_output_arg
    local eval_expr

    -- interp_program
    -- Given the ast for a program, execute it.
    function interp_program(ast)
        assert(type(ast) == "table")
        assert(ast[1] == PROGRAM)
        for i = 2, #ast do
            interp_stmt(ast[i])
        end
    end

    -- interp_stmt
    -- Given the ast for a statement, execute it.
    function interp_stmt(ast)
        local str
        local funcname, funcbody
        local assignname, assignvalue, asttemp
        local arrayname, arrayval, arrayindex
        local condition, loopcount

        assert(type(ast) == "table")
        -- [1/8] Handle empty statement 
        if ast[1] == EMPTY_STMT then
            -- Do nothing

        -- [2/8] Handle output statement 
        elseif ast[1] == OUTPUT_STMT then
            for i = 2, #ast do
                str = eval_output_arg(ast[i])
                util.output(str)
            end

        -- [3/8] Handle function definition 
        elseif ast[1] == FUNC_DEF then
            funcname = ast[2]
            funcbody = ast[3]
            state.f[funcname] = funcbody

        -- [4/8] Handle function call 
        elseif ast[1] == FUNC_CALL then
            funcname = ast[2]
            funcbody = state.f[funcname]
            if funcbody == nil then
                funcbody = { PROGRAM }
            end
            interp_program(funcbody)

        -- [5/8] Handle return statement
        elseif ast[1] == RETURN_STMT then
            state.v["return"] = eval_expr(ast[2])

        -- [6/8] Handle assignment statement 
        elseif ast[1] == ASSN_STMT then
            asttemp = ast[2]
            assignvalue = eval_expr(ast[3])

            if asttemp[1] == SIMPLE_VAR then
                assignname = asttemp[2]
                state.v[assignname] = assignvalue

            elseif asttemp[1] == ARRAY_VAR then
                arrayname = asttemp[2]
                arrayindex = eval_expr(asttemp[3])
                state.a[arrayname] = state.a[arrayname] or {}
                state.a[arrayname][arrayindex] = assignvalue
            end

        -- [7/8] Handle if statement
        elseif ast[1] == IF_STMT then
            local executed = false

            -- Check 'if' and all 'elseif' conditions
            for i = 2, #ast - 1, 2 do
                if eval_expr(ast[i], state, util) ~= 0 then
                    interp_program(ast[i + 1])
                    executed = true;
                    break
                end
            end

            -- Handle 'else' part, which should be the last block if present
            if not executed and #ast % 2 == 0 then
                interp_program(ast[#ast])
            end
        
        -- [8/8] Handle while loop
        elseif ast[1] == WHILE_LOOP then
            condition = ast[2]
            funcbody = ast[3]

            -- Continue to check loop condition
            while eval_expr(condition) ~= 0 do
                interp_program(funcbody)
            end

        else
            print("*** INTERP_STMT(AST) ERROR ***")
        end
    end

    -- eval_output_arg
    -- Given the AST for an output argument, evaluate it and return the
    -- value, as a string.
    function eval_output_arg(ast)
        local result, str, val

        assert(type(ast) == "table")
        -- String
        if ast[1] == STRLIT_OUT then
            str = ast[2]
            result = str:sub(2, str:len()-1)
        elseif ast[1] == EOL_OUT then
            result = "\n"

        -- Character
        elseif ast[1] == CHAR_CALL then
            val = tonumber(eval_expr(ast[2]))
            if val == nil or val < 0 or val > 255 then
                val = 0
            end
            result = string.char(val)

        -- Expression
        else
            val = eval_expr(ast)
            result = numToStr(val)
        end

        return result
    end

    -- eval_expr
    -- Given the AST for an expression, evaluate it and return the
    -- value, as a number.
    function eval_expr(ast)
        local result, var, val, index
        local funcname, funcbody
        local opand1, opand2, op1, op2, op3, opfunc

        assert(type(ast) == "table")

        -- [1/9] Numeric literal
        if ast[1] == NUMLIT_VAL then
            result = strToNum(ast[2])

        -- [2/9] Variable
        elseif ast[1] == SIMPLE_VAR then
            var = ast[2]
            result = state.v[var] or 0

        -- [3/9] Function Call
        elseif ast[1] == FUNC_CALL then
            funcname = ast[2]
            funcbody = state.f[funcname] or { PROGRAM }
            interp_program(funcbody)
            result = state.v["return"] or 0

        -- [4/9] Rand
        elseif ast[1] == RAND_CALL then
            result = util.random(eval_expr(ast[2]))

        -- [5/9] User Input 
        elseif ast[1] == INPUT_CALL then
            result = strToNum(util.input())

        -- [6/9] Boolean
        elseif ast[1] ==  BOOLLIT_VAL then
            if ast[2] == "true" then
                result = 1
            else
                result = 0
            end

        -- [7/9] Array Variable
        elseif ast[1] == ARRAY_VAR then
            var = state.a[ast[2]] or {}
            index = eval_expr(ast[3])
            result = var[index] or 0

        -- [8/9] UN_OP
        elseif ast[1][1] == UN_OP then
            op1 = ast[1][2]
            opand1 = eval_expr(ast[2])
            opfunc = unaryOperations[op1]
            if op1 == "-" then
                result = -opand1
            elseif op1 == "+" then
                result = opand1
            elseif op1 == "not" then
                result = (opand1 == 0) and 1 or 0
            end

        -- [9/9] BIN_OP
        elseif ast[1][1] == BIN_OP then
            op1 = ast[1][2]
            opand1 = eval_expr(ast[2])
            opand2 = eval_expr(ast[3])
            opfunc = binaryoperations[op1]
            if opfunc then
                result = opfunc(opand1, opand2)
            end
        end
        return result
    end

    -- Body of function interp
    interp_program(ast)
    return state
end

-- *********************************************************************
-- Module Table Return
-- *********************************************************************

return interpit

