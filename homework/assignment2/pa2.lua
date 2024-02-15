-- Secret Message for Assignment 2 : Be VERY sure to drink your Ovaltine.

-- pa2.lua
-- Spencer Baysinger
-- 2024-02-13
--
-- CS331 Programming Languages

local pa2 = {}

-- Function mapArray
function pa2.mapArray (f, t)
    local result = {}
    local index = 1

    for i, v in ipairs(t) do
        result[i] = f(v)
    end

    return result
end

-- Function concatMax
function pa2.concatMax (str, maxLen)
    local result = ""
    local strLen = #str

    if strLen == 0 or maxLen == 0 then
        return result
    end

    local fullRepeats = math.floor(maxLen / strLen)

    for i = 1, fullRepeats do
        result = result .. str
    end

    return result
end

-- Coroutine collatz
function pa2.collatz(k)
    local n = k

    while n > 1 do
        coroutine.yield(n)  -- Sequence is finished
        if n % 2 == 0 then
            n = n / 2
            --print(n)
        else
            n = n * 3 + 1
        end
    end
    coroutine.yield(n)  -- Sequence is finished

end

-- Function substrs
function pa2.substrs(s)
    local len = #s
    local i, j = 0, 0
    local iterations = 0
    local max_iterations = 1

    for i = 1, len do
        for j = i, len do 
            max_iterations = max_iterations + 1
        end
    end

    -- print("\nInput String: '" .. s .. "'")
    -- print("Max iterations: " .. max_iterations)

    local sub = ""
    local len = #s
    local index = 0
    local gap = 0

    return function()
        while iterations < max_iterations do

    		-- First segment to return empty string
            if iterations == 0 then
                iterations = iterations + 1
                sub = s:sub(index, index+gap)
                -- print("0) index=" .. index .. "- gap=" .. gap .. "- str=" .. sub)
                index = index + 1
                return sub
            end
            
            -- Second segment to return all strings of length less than full input string s
            if iterations > 0 and iterations < max_iterations-1 then
                if index+gap > len then
                    index = 1
                    gap = gap + 1
                end
                sub = s:sub(index, index+gap)
                -- print("1) index=" .. index .. "- gap=" .. gap .. "- str=" .. sub)
                index = index + 1
                iterations = iterations + 1
                return sub
            end
            
            -- Third segment to return full input string s
            if iterations ~= 1 and iterations == max_iterations-1 then
                iterations = iterations + 1
                sub = s
                -- print("2) index=" .. index .. "- gap=" .. gap .. "- str=" .. sub)
                return sub
            end
            -- print("\n")
            -- io.write("Waiting... Press enter: ")
            -- local userInput = io.read()
        end
    end
end

return pa2