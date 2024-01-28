print("Hello, world!")

print(30+31) -- number of days in June and July
print(60*60) -- number of seconds in one hour
print(15-9) -- number of hours between 9am and 3pm
print(365*24) -- number of hours in a year

print(2200/6/365) -- lifespan of a light bulb rated for 2200 hours
print(2200/(6*365)) -- same result with a different way to calculate it

require "testwell"
is('', '', 'Strings can be empty')
is('Hello, World!', "Hello, World!", 'Strings in double quotes')
is('Hello, World!', "Hello, " .. 'World!', 'Strings concatenated using ..')
is("She said, 'I love to code'", "She said, 'I love to code'", 'Single quotes in double quotes')
is('abc', 'a' .. 'b' .. 'c', 'Expressions with multiple concatenations')
is('abc', 'a' .. ('b' .. 'c'), 'Parentheses used for grouping')
is("I'm 8 years old", "I'm ".. 8 .." years old", 'Numbers convert to strings')
is("I'm 8 years old", ("I'm %d years old"):format(8), 'Strings can be formatted')
is("5 is more than 2", ("%d is more than %d"):format(5, 2), 'Strings ~with placeholders')
report()