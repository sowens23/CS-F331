#!/usr/bin/env lua
-- nilgai.lua
-- Glenn G. Chappell
-- 2024-04-02
--
-- For CS 331 Spring 2024
-- REPL/Shell for Nilgai Programming Language
-- Requires lexit.lua, parseit.lua, interpit.lua


parseit = require "parseit"
interpit = require "interpit"


-- *********************************************************************
-- Variables
-- *********************************************************************


local nilgai_state  -- Nilgai variable values


-- *********************************************************************
-- Object with Callback Functions for Interpreter
-- *********************************************************************


-- We define this object to so we can pass it to the interpreter.

local nilgai_util = {}


-- nilgai_util.input
-- Input a line of text from standard input and return it in string
-- form, with no trailing newline.
function nilgai_util.input()
    io.flush()  -- Ensure previous output is done before input
    local line = io.read("*l")
    if type(line) == "string" then
        return line
    else
        return ""
    end
end


-- nilgai_util.output
-- Output the given string to standard output, with no added newline.
function nilgai_util.output(s)
    assert(type(s) == "string")

    io.write(s)
end


-- nilgai_util.random
-- Given integer n, if n < 2, then return 0. Otherwise return a
-- pseudorandom integer from 0 to n-1. The PRNG is first seeded with a
-- time-dependent value if it has not been seeded yet in the current
-- Nilgai program.
-- Uses local seeded (set to false in runNilgai), seed.
local seeded = false
local seed = 0
function nilgai_util.random(n)
    assert(type(n) == "number")

    if not seeded then
        -- Update seed using time
        seed = seed + os.time() + math.floor(1000000 * os.clock())

        -- Update seed using randomly generated filename
        t = os.tmpname()
        bs = table.pack(t:byte(1, #t))
        for i, v in ipairs(bs) do
            seed = seed + v
        end

        -- Seed the PRNG
        math.randomseed(seed)
        seeded = true

        -- Skip early values (bad on some OSs)
        local dummy = math.random(10)
        dummy = math.random(10)
        dummy = math.random(10)
    end

    if n < 2 then
        return 0
    end
    return math.random(math.floor(n)) - 1
end


-- *********************************************************************
-- Functions for Nilgai REPL
-- *********************************************************************


-- printHelp
-- Print help for REPL.
local function printHelp()
    io.write("Type Nilgai code to execute it.\n")
    io.write("Commands (these may be abbreviated;")
    io.write(" for example, \":e\" for \":exit\")\n")
    io.write("  :exit          - Exit.\n")
    io.write("  :run FILENAME  - Execute Nilgai source file.\n")
    io.write("  :clear         - Clear Nilgai state")
    io.write(" (defined variables, functions).\n")
    io.write("  :help          - Print this help.\n")
end


-- elimSpace
-- Given a string, remove all leading & trailing whitespace, and return
-- result. If given nil, returns nil.
local function elimSpace(s)
    if s == nil then
        return nil
    end

    assert(type(s) == "string")

    local ss = s:gsub("^%s+", "")
    ss = ss:gsub("%s+$", "")
    return ss
end


-- elimLeadingNonspace
-- Given a string, remove leading non-whitespace, and return result.
local function elimLeadingNonspace(s)
    assert(type(s) == "string")

    local ss = s:gsub("^%S+", "")
    return ss
end


-- errMsg
-- Given an error message, prints it in flagged-error form, with a
-- newline appended.
local function errMsg(msg)
    assert(type(msg) == "string")

    io.write("*** ERROR - "..msg.."\n")
end


-- clearState
-- Clear Nilgai state: functions, simple variables, arrays.
local function clearState()
    nilgai_state = { f={}, v={}, a={} }
end


-- runNilgai
-- Given a string, attempt to treat it as source code for a Nilgai
-- program, and execute it. I/O uses standard input & output.
--
-- Parameters:
--   program  - Nilgai source code
--   state    - Values of Nilgai variables as in interpit.interp.
--   exec_msg  - Optional string. If code parses, then, before it is
--              executed, this string is printed, followed by a newline.
--
-- Returns three values:
--   good     - true if initial portion of program parsed successfully;
--              false otherwise.
--   done     - true if parse reached end of program; false otherwise.
--   newstate - If good, done are both true, then new value of state,
--              updated with revised values of variables. Otherwise,
--              same as passed value of state.
--
-- If good and done are both true, then the code was executed.
local function runNilgai(program, state, exec_msg)
    assert(type(program) == "string")
    assert(type(state) == "table")
    assert(type(state.f) == "table")
    assert(type(state.v) == "table")
    assert(type(state.a) == "table")
    assert(exec_msg == nil or type(exec_msg) == "string")

    -- Mark PRNG as not seeded; it will be seeded the first time a
    -- random number is requested in the program.
    seeded = false

    local good, done, ast = parseit.parse(program)
    local newstate
    if good and done then
        if exec_msg ~= nil then
            io.write(exec_msg.."\n")
        end
        newstate = interpit.interp(ast, state, nilgai_util)
    else
        newstate = state
    end
    return good, done, newstate
end


-- runFile
-- Given filename, attempt to read source for a Nilgai program from it,
-- and execute the program. If prinntmsg is true and the program parses
-- correctly, then print a message before executing the file.
local function runFile(fname, printmsg)
    assert(type(fname) == "string")
    assert(type(printmsg) == "boolean")

    function readable(fname)
        local f = io.open(fname, "r")
        if f ~= nil then
            f:close()
            return true
        else
            return false
        end
    end

    local good, done

    if not readable(fname) then
        errMsg("Nilgai source file not readable: '"..fname.."'")
        return
    end
    local source = ""
    for line in io.lines(fname) do
        source = source .. line .. "\n"
    end
    local exec_msg
    if printmsg then
        exec_msg = "EXECUTING FILE: '"..fname.."'"
    else
        exec_msg = nil
    end
    good, done, nilgai_state = runNilgai(source, nilgai_state, exec_msg)
    if not (good and done) then
        errMsg("Syntax error in Nilgai source file: '"..fname.."'")
    end
end


-- doReplCommand
-- Given input line beginning with ":", execute as REPL command. Return
-- true if execution of REPL should continue; false if it should end.
local function doReplCommand(line)
    assert(line:sub(1,1) == ":")
    if line:sub(1,2) == ":e" then
        return false
    elseif line:sub(1,2) == ":h" then
        printHelp()
        return true
    elseif line:sub(1,2) == ":c" then
        clearState()
        io.write("Nilgai state cleared\n")
        return true
    elseif line:sub(1,2) == ":r" then
        fname = elimLeadingNonspace(line:sub(3))
        fname = elimSpace(fname)
        if (fname == "") then
            errMsg("No filename given")
        else
            runFile(fname, true)  -- true: Print execution message
        end
        return true
    else
        errMsg("Unknown command")
        return true
    end
end


-- repl
-- Nilgai REPL. Prompt & get a line. If it begins with a colon (":")
-- then treat it as a REPL command. Otherwise, treat line as Nilgai
-- program, and attempt to execute it. If it looks like an incomplete
-- Nilgai program, then keep inputting, and continue to attempt to
-- execute. REPEAT.
local function repl()
    local line, good, done, continueflag, prompt
    local source = ""

    printHelp()
    while true do
        -- Prompt
        if source == "" then
            io.write("\n")
            prompt = ">>> "
        else
            prompt = "... "
        end

        -- Input a line + error check
        repeat
            io.write(prompt)
            io.flush()  -- Ensure previous output is done before input
            line = io.read("*l")  -- Read a line
            line = elimSpace(line)
        until line ~= ""

        if line == nil then             -- Read error (EOF?)
            io.write("\n")
            break
        end

        -- Handle input, as approprite
        if line:sub(1,1) == ":" then    -- Command
            source = ""
            continueflag = doReplCommand(line)
            if not continueflag then
                break
            end
        else                            -- Nilgai code
            source = source .. line
            good, done, nilgai_state = runNilgai(source, nilgai_state)
            if (not good) and done then
                source = source .. "\n" -- Continue inputting source
            else
                source = ""             -- Start over
            end
            if not done then
                errMsg("Syntax error in Nilgai code")
            end
        end
    end
end


-- *********************************************************************
-- Main Program
-- *********************************************************************


-- Initialize Nilgai
clearState()

-- Command-line argument? If so treat as Nilgai source filename, read
-- source, and execute.
if arg[1] ~= nil then
    runFile(arg[1], false)  -- false: Do not print execution message
    io.write("\n")
    io.write("Press ENTER to quit ")
    io.flush()  -- Ensure previous output is done before input
    io.read("*l")
-- Otherwise, fire up the Nilgai REPL.
else
    repl()
end

