lexit = require "lexit"

program = "+ - * / = == != > >= < <= % [ ]"

for lexstr, cat in lexit.lex(program) do
    print(lexstr, lexit.catnames[cat],"\n")
end