-- testparse.lua
-- Spencer Baysinger
-- 2024-03-05
-- 
-- For CS 331 Spring 2024
-- Self testing file to check case fails individually

parseit = require "parseit"  -- Import parseit module

-- *****************************
-- Utility Functions
-- *****************************

-- printAST_parseit
-- Write an AST, in (roughly) Lua form, with numbers replaced by the
-- symbolic constants used in parseit, where possible.
-- See the Assignment description for the AST Specification.
function printAST_parseit(...)
    if select("#", ...) ~= 1 then
        error("printAST_parseit: must pass exactly 1 argument")
    end
    local x = select(1, ...)  -- Get argument (which may be nil)
    
    if type(x) == "nil" then
        io.write("nil")
    elseif type(x) == "number" then
        if symbolNames[x] then
            io.write(symbolNames[x])
        else
            io.write("<ERROR: Unknown constant: "..x..">")
        end
    elseif type(x) == "string" then
        io.write('"'..x..'"')
    elseif type(x) == "boolean" then
        if x then
            io.write("true")
        else
            io.write("false")
        end
    elseif type(x) ~= "table" then
        io.write('<'..type(x)..'>')
    else  -- type is "table"
        io.write("{ ")
        local first = true  -- First iteration of loop?
        local maxk = 0
        for k, v in ipairs(x) do
            if first then
                first = false
            else
                io.write(", ")
            end
            maxk = k
            printAST_parseit(v)
        end
        for k, v in pairs(x) do
            if type(k) ~= "number"
            or k ~= math.floor(k)
            or (k < 1 and k > maxk) then
                if first then
                    first = false
                else
                    io.write(", ")
                end
                io.write("[")
                printAST_parseit(k)
                io.write("]=")
                printAST_parseit(v)
            end
        end
        io.write(" }")
    end
end

-- bool2Str
-- Given boolean, return string representing it: "true" or "false".
function bool2Str(b)
    if b then
        return "true"
    else
        return "false"
    end
end

-- *********************************************************************
-- Definitions for This Test Program
-- *********************************************************************


-- Symbolic Constants for AST
-- Names differ from those in assignment, to avoid interference.
local PROGRAMx     = 1
local EMPTYxSTMT   = 2
local OUTPUTxSTMT  = 3
local RETURNxSTMT  = 4
local ASSNxSTMT    = 5
local FUNCxCALL    = 6
local FUNCxDEF     = 7
local IFxSTMT      = 8
local WHILExLOOP   = 9
local STRLITxOUT   = 10
local EOLxOUT      = 11
local CHARxCALL    = 12
local BINxOP       = 13
local UNxOP        = 14
local NUMLITxVAL   = 15
local BOOLLITxVAL  = 16
local INPUTxCALL   = 17
local RANDxCALL    = 18
local SIMPLExVAR   = 19
local ARRAYxVAR    = 20


-- String forms of symbolic constants
-- Used by printAST_parseit
symbolNames = {
  [1]="PROGRAM",
  [2]="EMPTY_STMT",
  [3]="OUTPUT_STMT",
  [4]="RETURN_STMT",
  [5]="ASSN_STMT",
  [6]="FUNC_CALL",
  [7]="FUNC_DEF",
  [8]="IF_STMT",
  [9]="WHILE_LOOP",
  [10]="STRLIT_OUT",
  [11]="EOL_OUT",
  [12]="CHAR_CALL",
  [13]="BIN_OP",
  [14]="UN_OP",
  [15]="NUMLIT_VAL",
  [16]="BOOLLIT_VAL",
  [17]="INPUT_CALL",
  [18]="RAND_CALL",
  [19]="SIMPLE_VAR",
  [20]="ARRAY_VAR",
}


-- *****************************
-- Main Program
-- *****************************

local prog = "x=a[1];"

io.write("\n" .. prog .. "\n")
local actualGood, actualDone, actualAST = parseit.parse(prog)


io.write("\n" .. "Good: " .. bool2Str(actualGood) .. "\n")
io.write("Done: " .. bool2Str(actualDone) .. "\n")
io.write("AST Tree: ")
printAST_parseit(actualAST)
io.write("\n")
