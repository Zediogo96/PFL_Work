% a 

print_n(_, 0) :- !.
print_n(Symbol, N) :- 
    N > 0,
    write(Symbol),
    print_n(Symbol, N-1).

% b

print_string("").
print_string([Code | T]) :- char_code(Char, Code),
                            write(Char),
                            print_string(T).

print_text(Text, Symbol, Padding) :- write(Symbol),
                                     print_n(' ', Padding),
                                     print_string(Text),
                                     print_n(' ', Padding),
                                     write(Symbol).

